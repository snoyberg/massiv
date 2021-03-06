{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Stencil.Internal
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Stencil.Internal where

import           Control.Applicative
import           Control.Monad                      (unless, void, when)
import           Control.Monad.Primitive            (PrimMonad)
import           Control.Monad.ST
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest
import           Data.Default                       (Default (def))
import           GHC.Exts                           (inline)

import qualified Data.Vector.Unboxed                as VU
import qualified Data.Vector.Unboxed.Mutable        as MVU

data Stencil ix e a = Stencil
  { stencilBorder :: Border e
  , stencilSize   :: !ix
  , stencilCenter :: !ix
  , stencilFunc   :: (ix -> e) -> ix -> a
  }

data Dependency ix = Dependency { depDimension :: Int
                                , depDirection :: Int }

data StencilM ix e a = StencilM
  { mStencilBorder :: Border e
  , mStencilSize   :: !ix
  , mStencilCenter :: !ix
  , mStencilDeps   :: Array M Int Int
  , mStencilFunc   :: forall m . PrimMonad m => (ix -> e) -> (ix -> m a) -> ix -> m a
  }


-- | __Warning__: Highly experimental and untested, use at your own risk.
makeStencilM
  :: forall ix e a . (Index ix, Default e, Default a)
  => Border e
  -> ix
  -> ix
  -> (forall m . PrimMonad m => ((ix -> e) -> (ix -> m a) -> m a))
  -> StencilM ix e a
makeStencilM b !sSz !sCenter relStencil =
  StencilM b sSz sCenter deps stencil
  where
    stencil
      :: (Default a, PrimMonad m)
      => (ix -> e) -> (ix -> m a) -> ix -> m a
    stencil getVal getCurValM !ix =
      inline
        relStencil
        (\ !ixD -> getVal (liftIndex2 (-) ix ixD))
        (\ !ixD -> getCurValM (liftIndex2 (-) ix ixD))
    {-# INLINE stencil #-}
    Dim sRank = rank sSz
    defArrA :: Array D ix a
    defArrA = DArray Seq sSz (const def)
    defArr = DArray Seq sSz (const def)
    deps = toManifest $ fromVector' sRank $ VU.create makeDeps
    -- TODO: switch to mutable Array, once it is implemented.
    makeDeps :: ST s (MVU.MVector s Int)
    makeDeps = do
      mv <- MVU.new sRank
      -- Need to record at which dimensions and directions there are dependencies
      let checkWrite ix = do
            unless (isSafeIndex sSz ix) $
              error "mkStaticStencilM: Invalid StencilM index access"
            loopM_ 0 (< sRank) (+ 1) $ \i -> do
              let !r =
                    maybe (errorImpossible "mkStaticStencilM") signum $
                    getIndex ix (Dim i)
              when (r /= 0) $ do
                curVal <- MVU.read mv i
                if curVal == 0
                  then MVU.write mv i r
                  else when (curVal /= r) $
                       error
                         "mkStaticStencilM: Stencil creates an invalid dependency"
            return $ unsafeIndex defArrA ix
          checkRead = return . safeStencilIndex defArrA
      void $ stencil (safeStencilIndex defArr) checkRead sCenter
      void $ relStencil (unsafeIndex defArr) checkWrite
      return mv
{-# INLINE makeStencilM #-}






instance Functor (Stencil ix e) where
  fmap f stencil@(Stencil { stencilFunc = g }) =
    stencil { stencilFunc = (\ s -> f . g s) }
  --{-# INLINE fmap #-}


-- TODO: Figure out interchange law (u <*> pure y = pure ($ y) <*> u) and issue
-- with discarding size and center. Best idea so far is to increase stencil size to
-- the maximum one and shift the center of the other stencil so the yboth match
-- up. This approach would also remove requirement to validate the result
-- Stencil - both stencils are trusted, increasing the size will not affect the
-- safety.
instance (Default e, Index ix) => Applicative (Stencil ix e) where
  pure a = Stencil Edge (liftIndex (+ 1) zeroIndex) zeroIndex (const (const a))
  {-# INLINE pure #-}
  (<*>) (Stencil _ sSz1 sC1 f1) (Stencil sB sSz2 sC2 f2) =
    validateStencil def (Stencil sB newSz maxCenter (\gV ix -> (f1 gV ix) (f2 gV ix)))
    where
      newSz =
        liftIndex2
          (+)
          maxCenter
          (liftIndex2 max (liftIndex2 (-) sSz1 sC1) (liftIndex2 (-) sSz2 sC2))
      !maxCenter = liftIndex2 max sC1 sC2
  {-# INLINE (<*>) #-}

instance (Index ix, Default e, Num a) => Num (Stencil ix e a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Index ix, Default e, Fractional a) => Fractional (Stencil ix e a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance (Index ix, Default e, Floating a) => Floating (Stencil ix e a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh


safeStencilIndex :: Index a => Array D a t -> a -> t
safeStencilIndex DArray {..} ix
  | isSafeIndex dSize ix = dUnsafeIndex ix
  | otherwise =
    error $
    "Index is out of bounds: " ++ show ix ++ " for stencil size: " ++ show dSize



validateStencil
  :: Index ix
  => e -> Stencil ix e a -> Stencil ix e a
validateStencil d s@(Stencil _ sSz sCenter stencil) =
  let valArr = DArray Seq sSz (const d)
  in stencil (safeStencilIndex valArr) sCenter `seq` s
{-# INLINE validateStencil #-}


