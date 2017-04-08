{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
-- |
-- Module      : Data.Array.Massiv.Common.Index
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common.Index where

import GHC.TypeLits
import Data.Proxy

-- type family DIM ix (n :: Nat) :: *

--   --DIM 0 = Z
-- type instance DIM DIM1 1 = DIM1
-- type instance DIM DIM2 2 = DIM2
-- type instance DIM DIM3 3 = DIM3
-- type instance DIM DIM4 4 = DIM4
-- type instance DIM DIM5 5 = DIM5




-- getI :: (1 <= n, n <= 3, KnownNat n) => DIM DIM3 3 -> DiX n -> Int
-- getI (i, j, k) n = case natVal n of
--                      1 -> i
--                      2 -> j
--                      _ -> k

-- data family Sz (n :: Nat) :: *

-- data instance Sz 2 = Sz2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- data instance Sz 3 = Sz3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int


-- data L x = x :. Int deriving Show

--data I x = Int :. x deriving Show


-- type family Ix (n :: Nat) :: * where
--   Ix 1 = Int
--   Ix n = L (Ix (n-1))

--type instance DIM 1 = Ix 1
--type instance DIM 2 = L Int
-- type instance DIM 3 = DIM3
-- type instance DIM 4 = DIM4
-- type instance DIM 5 = DIM5

-- data family Ix (n :: Nat) :: *

-- data instance Ix 3 = Ix3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int


-- instance Dimension Sz 2 where
--   tElem (Sz2 m n) = m * n

-- instance Dimension Sz 3 where
--   tElem (Sz3 m n k) = m * n * k

-- class (KnownNat (Rank ix), Eq ix, Ord ix, Show ix) => Index' ix where

--   unsnocDim' :: Index (Lower ix) => ix -> (Lower ix, Int)

--   getIndex' :: ValidRank ix n => ix -> DiX n -> Int

--   dropIndex' :: ix -> Int -> Maybe (Lower ix)

data D1 = D1

type DIM1 = Int

type DIM2 = (Int, Int)

type DIM3 = (Int, Int, Int)

type DIM4 = (Int, Int, Int, Int)

type DIM5 = (Int, Int, Int, Int, Int)

-- | Relates this dimension to the lower one.
type family Lower ix :: *

-- | Relates this dimension to the higher one.
type family Higher ix :: *

type family Rank ix :: Nat

type instance Rank Z = 0
type instance Rank DIM1 = 1
type instance Rank DIM2 = 2
type instance Rank DIM3 = 3
type instance Rank DIM4 = 4
type instance Rank DIM5 = 5

type instance Lower Z = DIM5
type instance Lower DIM1 = Z
type instance Lower DIM2 = DIM1
type instance Lower DIM3 = DIM2
type instance Lower DIM4 = DIM3
type instance Lower DIM5 = DIM4

type instance Higher Z = DIM1
type instance Higher DIM1 = DIM2
type instance Higher DIM2 = DIM3
type instance Higher DIM3 = DIM4
type instance Higher DIM4 = DIM5
type instance Higher DIM5 = Z


data DimIx (n :: Nat) = DimIx (Proxy n) deriving Show

dimIx1 :: DimIx 1
dimIx1 = DimIx Proxy

dimIx2 :: DimIx 2
dimIx2 = DimIx Proxy

dimIx3 :: DimIx 3
dimIx3 = DimIx Proxy

dimIx4 :: DimIx 4
dimIx4 = DimIx Proxy

dimIx5 :: DimIx 5
dimIx5 = DimIx Proxy


type ValidDimIx ix n = (1 <= n, n <= Rank ix, KnownNat n)


class (KnownNat (Rank ix), 1 <= Rank ix, Rank (Lower ix) <= Rank ix, Eq ix, Ord ix, Show ix) =>
  Index ix where

  zeroIndex :: ix

  -- | Check whether index is within the size.
  isSafeIndex :: ix -- ^ Size
              -> ix -- ^ Index
              -> Bool

  -- | Total number of elements in an array of this size.
  totalElem :: ix -> Int

  -- | Produce linear index from size and index
  toLinearIndex :: ix -- ^ Size
                -> ix -- ^ Index
                -> Int

  -- | Produce N Dim index from size and linear index
  fromLinearIndex :: ix -> Int -> ix

  liftIndex :: (Int -> Int) -> ix -> ix

  liftIndex2 :: (Int -> Int -> Int) -> ix -> ix -> ix

  repairIndex :: ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix

  consDim :: Int -> Lower ix -> ix

  unconsDim :: ix -> (Int, Lower ix)

  snocDim :: Lower ix -> Int -> ix

  unsnocDim :: ix -> (Lower ix, Int)

  getIndex :: ValidDimIx ix n => ix -> DimIx n -> Int

  setIndex :: ValidDimIx ix n => ix -> DimIx n -> Int -> ix

  dropIndex :: ValidDimIx ix n => ix -> DimIx n -> Lower ix

  iter :: ix -> ix -> Int -> (Int -> Int -> Bool) -> a -> (ix -> a -> a) -> a

  iterM :: Monad m => ix -> ix -> Int -> (Int -> Int -> Bool) -> a -> (ix -> a -> m a) -> m a

  iterM_ :: Monad m => ix -> ix -> Int -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()



data Z = Z deriving (Eq, Ord, Show)

-- errorBelowZero :: a
-- errorBelowZero = error "There is no dimension that is lower than DIM0"

-- instance Index Z where
--   rank _ = 0
--   {-# INLINE rank #-}
--   zeroIndex = Z
--   {-# INLINE zeroIndex #-}
--   totalElem _ = 0
--   {-# INLINE totalElem #-}
--   isSafeIndex _   _    = False
--   {-# INLINE isSafeIndex #-}
--   toLinearIndex _ _ = 0
--   {-# INLINE toLinearIndex #-}
--   fromLinearIndex _ _ = Z
--   {-# INLINE fromLinearIndex #-}
--   repairIndex _ _ _ _ = Z
--   {-# INLINE repairIndex #-}
--   consDim _ _ = Z
--   {-# INLINE consDim #-}
--   unconsDim _ = errorBelowZero
--   {-# INLINE unconsDim #-}
--   snocDim _ _ = Z
--   {-# INLINE snocDim #-}
--   unsnocDim _ = errorBelowZero
--   {-# INLINE unsnocDim #-}
--   getIndex _ _ = Nothing
--   {-# INLINE getIndex #-}
--   setIndex _ _ _ = Nothing
--   {-# INLINE setIndex #-}
--   dropIndex _ _ = Nothing
--   {-# INLINE dropIndex #-}
--   liftIndex _ _ = Z
--   {-# INLINE liftIndex #-}
--   liftIndex2 _ _ _ = Z
--   {-# INLINE liftIndex2 #-}
--   iter _ _ _ _ acc f = f Z acc
--   {-# INLINE iter #-}
--   iterM _ _ _ _ acc f = f Z acc
--   {-# INLINE iterM #-}
--   iterM_ _ _ _ _ f = void $ f Z
--   {-# INLINE iterM_ #-}


instance Index DIM1 where
  zeroIndex = 0
  {-# INLINE zeroIndex #-}
  totalElem = id
  {-# INLINE totalElem #-}
  isSafeIndex !k !i = 0 <= i && i < k
  {-# INLINE isSafeIndex #-}
  toLinearIndex _ = id
  {-# INLINE toLinearIndex #-}
  fromLinearIndex _ = id
  {-# INLINE fromLinearIndex #-}
  repairIndex !k !i rBelow rOver
    | i < 0 = rBelow k i
    | i >= k = rOver k i
    | otherwise = i
  {-# INLINE repairIndex #-}
  consDim i _ = i
  {-# INLINE consDim #-}
  unconsDim i = (i, Z)
  {-# INLINE unconsDim #-}
  snocDim _ i = i
  {-# INLINE snocDim #-}
  unsnocDim i = (Z, i)
  {-# INLINE unsnocDim #-}
  getIndex i _ = i
  {-# INLINE getIndex #-}
  setIndex _ _ x = x
  {-# INLINE setIndex #-}
  dropIndex _ _ = Z
  {-# INLINE dropIndex #-}
  liftIndex f = f
  {-# INLINE liftIndex #-}
  liftIndex2 f = f
  {-# INLINE liftIndex2 #-}
  iter k0 k1 inc cond = loop k0 (`cond` k1) (+inc)
  {-# INLINE iter #-}
  iterM k0 k1 inc cond = loopM k0 (`cond` k1) (+inc)
  {-# INLINE iterM #-}
  iterM_ k0 k1 inc cond = loopM_ k0 (`cond` k1) (+inc)
  {-# INLINE iterM_ #-}


instance Index DIM2 where
  zeroIndex = (0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(m, n) = m * n
  {-# INLINE totalElem #-}
  isSafeIndex !(m, n) !(i, j) = 0 <= i && 0 <= j && i < m && j < n
  {-# INLINE isSafeIndex #-}
  toLinearIndex !(_, n) !(i, j) = n * i + j
  {-# INLINE toLinearIndex #-}
  fromLinearIndex !(_, n) !k = k `quotRem` n
  {-# INLINE fromLinearIndex #-}
  consDim = (,)
  {-# INLINE consDim #-}
  unconsDim = id
  {-# INLINE unconsDim #-}
  snocDim = (,)
  {-# INLINE snocDim #-}
  unsnocDim = id
  {-# INLINE unsnocDim #-}
  getIndex (i, j) dIx =
    case natVal dIx of
      1 -> i
      _ -> j
  {-# INLINE getIndex #-}
  setIndex (i, j) dIx x =
    case natVal dIx of
      1 -> (x, j)
      _ -> (i, x)
  {-# INLINE setIndex #-}
  dropIndex (i, j) dIx =
    case natVal dIx of
      1 -> j
      _ -> i
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i, j) = (f i, f j)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, j0) (i1, j1) = (f i0 i1, f j0 j1)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index DIM3 where
  zeroIndex = (0, 0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(m, n, o) = m * n * o
  {-# INLINE totalElem #-}
  isSafeIndex !(m, n, o) !(i, j, k) =
    0 <= i && 0 <= j && 0 <= k && i < m && j < n && k < o
  {-# INLINE isSafeIndex #-}
  toLinearIndex !(_, n, o) !(i, j, k) = (n * i + j) * o + k
  {-# INLINE toLinearIndex #-}
  fromLinearIndex !(_, n, o) !l = (i, j, k)
    where !(h, k) = quotRem l o
          !(i, j) = quotRem h n
  {-# INLINE fromLinearIndex #-}
  consDim i (j, k) = (i, j, k)
  {-# INLINE consDim #-}
  unconsDim (i, j, k) = (i, (j, k))
  {-# INLINE unconsDim #-}
  snocDim (i, j) k = (i, j, k)
  {-# INLINE snocDim #-}
  unsnocDim (i, j, k) = ((i, j), k)
  {-# INLINE unsnocDim #-}
  getIndex (i, j, k) dIx =
    case natVal dIx of
      1 -> i
      2 -> j
      _ -> k
  {-# INLINE getIndex #-}
  setIndex (i, j, k) dIx x =
    case natVal dIx of
      1 -> (x, j, k)
      2 -> (i, x, k)
      _ -> (i, j, x)
  {-# INLINE setIndex #-}
  dropIndex (i, j, k) dIx =
    case natVal dIx of
      1 -> (j, k)
      2 -> (i, k)
      _ -> (i, j)
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i, j, k) = (f i, f j, f k)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, j0, k0) (i1, j1, k1) = (f i0 i1, f j0 j1, f k0 k1)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index DIM4 where
  zeroIndex = (0, 0, 0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(n1, n2, n3, n4) = n1 * n2 * n3 * n4
  {-# INLINE totalElem #-}
  isSafeIndex = isSafeIndexRec
  {-# INLINE isSafeIndex #-}
  toLinearIndex = toLinearIndexRec
  {-# INLINE toLinearIndex #-}
  fromLinearIndex = fromLinearIndexRec
  {-# INLINE fromLinearIndex #-}
  consDim i1 (i2, i3, i4) = (i1, i2, i3, i4)
  {-# INLINE consDim #-}
  unconsDim (i1, i2, i3, i4) = (i1, (i2, i3, i4))
  {-# INLINE unconsDim #-}
  snocDim (i1, i2, i3) i4 = (i1, i2, i3, i4)
  {-# INLINE snocDim #-}
  unsnocDim (i1, i2, i3, i4) = ((i1, i2, i3), i4)
  {-# INLINE unsnocDim #-}
  getIndex (i1, i2, i3, i4) dIx =
    case natVal dIx of
      1 -> i1
      2 -> i2
      3 -> i3
      _ -> i4
  {-# INLINE getIndex #-}
  setIndex (i1, i2, i3, i4) dIx x =
    case natVal dIx of
      1 -> ( x, i2, i3, i4)
      2 -> (i1,  x, i3, i4)
      3 -> (i1, i2,  x, i4)
      _ -> (i1, i2, i3,  x)
  {-# INLINE setIndex #-}
  dropIndex (i1, i2, i3, i4) dIx =
    case natVal dIx of
      1 -> (i2, i3, i4)
      2 -> (i1, i3, i4)
      3 -> (i1, i2, i4)
      _ -> (i1, i2, i3)
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i0, i1, i2, i3) = (f i0, f i1, f i2, f i3)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, i1, i2, i3) (j0, j1, j2, j3) =
    (f i0 j0, f i1 j1, f i2 j2, f i3 j3)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


instance Index DIM5 where
  zeroIndex = (0, 0, 0, 0, 0)
  {-# INLINE zeroIndex #-}
  totalElem !(n1, n2, n3, n4, n5) = n1 * n2 * n3 * n4 * n5
  {-# INLINE totalElem #-}
  isSafeIndex = isSafeIndexRec
  {-# INLINE isSafeIndex #-}
  toLinearIndex = toLinearIndexRec
  {-# INLINE toLinearIndex #-}
  fromLinearIndex = fromLinearIndexRec
  {-# INLINE fromLinearIndex #-}
  consDim i1 (i2, i3, i4, i5) = (i1, i2, i3, i4, i5)
  {-# INLINE consDim #-}
  unconsDim (i1, i2, i3, i4, i5) = (i1, (i2, i3, i4, i5))
  {-# INLINE unconsDim #-}
  snocDim (i1, i2, i3, i4) i5 = (i1, i2, i3, i4, i5)
  {-# INLINE snocDim #-}
  unsnocDim (i1, i2, i3, i4, i5) = ((i1, i2, i3, i4), i5)
  {-# INLINE unsnocDim #-}
  getIndex (i1, i2, i3, i4, i5) dIx =
    case natVal dIx of
      1 -> i1
      2 -> i2
      3 -> i3
      4 -> i4
      _ -> i5
  {-# INLINE getIndex #-}
  setIndex (i1, i2, i3, i4, i5) dIx x =
    case natVal dIx of
      1 -> ( x, i2, i3, i4, i5)
      2 -> (i1,  x, i3, i4, i5)
      3 -> (i1, i2,  x, i4, i5)
      4 -> (i1, i2, i3,  x, i5)
      _ -> (i1, i2, i3,  i4, x)
  {-# INLINE setIndex #-}
  dropIndex (i1, i2, i3, i4, i5) dIx =
    case natVal dIx of
      1 -> (i2, i3, i4, i5)
      2 -> (i1, i3, i4, i5)
      3 -> (i1, i2, i4, i5)
      4 -> (i1, i2, i3, i5)
      _ -> (i1, i2, i3, i4)
  {-# INLINE dropIndex #-}
  repairIndex = repairIndexRec
  {-# INLINE repairIndex #-}
  liftIndex f (i0, i1, i2, i3, i4) = (f i0, f i1, f i2, f i3, f i4)
  {-# INLINE liftIndex #-}
  liftIndex2 f (i0, i1, i2, i3, i4) (j0, j1, j2, j3, j4) =
    (f i0 j0, f i1 j1, f i2 j2, f i3 j3, f i4 j4)
  {-# INLINE liftIndex2 #-}
  iter = iterRec
  {-# INLINE iter #-}
  iterM = iterMRec
  {-# INLINE iterM #-}
  iterM_ = iterMRec_
  {-# INLINE iterM_ #-}


-- | Approach to be used with respect to the border of an array
-- when index goes out of bounds.
data Border e = Fill e | Wrap | Edge | Reflect | Continue deriving (Eq, Show)



handleBorderIndex :: Index ix => Border e -> ix -> (ix -> e) -> ix -> e
handleBorderIndex border !sz getVal !ix =
  case border of
    Fill val -> if isSafeIndex sz ix then getVal ix else val
    Wrap     -> getVal (repairIndex sz ix (flip mod) (flip mod))
    Edge     -> getVal (repairIndex sz ix (const (const 0)) (\ !k _ -> k - 1))
    Reflect  -> getVal (repairIndex sz ix (\ !k !i -> (abs i - 1) `mod` k)
                        (\ !k !i -> (-i - 1) `mod` k))
    Continue -> getVal (repairIndex sz ix (\ !k !i -> abs i `mod` k)
                        (\ !k !i -> (-i - 2) `mod` k))
{-# INLINE handleBorderIndex #-}

isSafeIndexRec :: (Index (Lower ix), Index ix) => ix -> ix -> Bool
isSafeIndexRec !sz !ix = isSafeIndex n0 i0 && isSafeIndex szL ixL
    where
      !(n0, szL) = unconsDim sz
      !(i0, ixL) = unconsDim ix
{-# INLINE isSafeIndexRec #-}


repairIndexRec :: (Index (Lower ix), Index ix) =>
                  ix -> ix -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ix
repairIndexRec !sz !ix rBelow rOver =
    snocDim (repairIndex szL ixL rBelow rOver) (repairIndex sz0 ix0 rBelow rOver)
    where !(szL, sz0) = unsnocDim sz
          !(ixL, ix0) = unsnocDim ix
{-# INLINE repairIndexRec #-}


-- liftIndexRec :: (Index (Lower ix), Index ix) =>
--                 (Int -> Int) -> ix -> ix
-- liftIndexRec f !ix = snocDim (liftIndex f ixL) (liftIndex f ix0)
--   where
--     !(ixL, ix0) = unsnocDim ix
-- {-# INLINE liftIndexRec #-}


-- liftIndex2Rec :: (Index (Lower ix), Index ix) =>
--                 (Int -> Int -> Int) -> ix -> ix -> ix
-- liftIndex2Rec f !ix !ixD = snocDim (liftIndex2 f ixL ixDL) (liftIndex2 f ix0 ixD0)
--   where
--     !(ixL, ix0) = unsnocDim ix
--     !(ixDL, ixD0) = unsnocDim ixD
-- {-# INLINE liftIndex2Rec #-}


toLinearIndexRec :: (Index (Lower ix), Index ix) =>
                      ix -> ix -> Int
toLinearIndexRec !sz !ix = (toLinearIndex szL ixL) * n + i
  where !(szL, n) = unsnocDim sz
        !(ixL, i) = unsnocDim ix
{-# INLINE toLinearIndexRec #-}


fromLinearIndexRec :: (Index (Lower ix), Index ix) =>
                      ix -> Int -> ix
fromLinearIndexRec !sz !k = snocDim (fromLinearIndex szL kL) j
  where !(kL, j) = quotRem k n
        !(szL, n) = unsnocDim sz
{-# INLINE fromLinearIndexRec #-}


iterRec
  :: (Index (Lower ix), Index ix)
  => ix -> ix -> Int -> (Int -> Int -> Bool) -> a -> (ix -> a -> a) -> a
iterRec !sIx !eIx !inc cond !acc f =
    loop k0 (`cond` k1) (+ inc) acc $ \ !i !acc0 ->
      iter sIxL eIxL inc cond acc0 $ \ !ix -> f (consDim i ix)
    where
      !(k0, sIxL) = unconsDim sIx
      !(k1, eIxL) = unconsDim eIx
{-# INLINE iterRec #-}


iterMRec
  :: (Index (Lower ix), Index ix, Monad m)
  => ix -> ix -> Int -> (Int -> Int -> Bool) -> a -> (ix -> a -> m a) -> m a
iterMRec !sIx !eIx !inc cond !acc f = do
    let !(k0, sIxL) = unconsDim sIx
        !(k1, eIxL) = unconsDim eIx
    loopM k0 (`cond` k1) (+ inc) acc $ \ !i !acc0 ->
      iterM sIxL eIxL inc cond acc0 $ \ !ix ->
        f (consDim i ix)
{-# INLINE iterMRec #-}

iterMRec_
  :: (Index (Lower ix), Index ix, Monad m)
  => ix -> ix -> Int -> (Int -> Int -> Bool) -> (ix -> m a) -> m ()
iterMRec_ !sIx !eIx !inc cond f = do
    let !(k0, sIxL) = unconsDim sIx
        !(k1, eIxL) = unconsDim eIx
    loopM_ k0 (`cond` k1) (+ inc) $ \ !i ->
      iterM_ sIxL eIxL inc cond $ \ !ix ->
        f (consDim i ix)
{-# INLINE iterMRec_ #-}


iterLinearM_ :: (Index ix, Monad m) =>
                ix -- ^ Size
             -> Int -- ^ Start
             -> Int -- ^ End
             -> Int -- ^ Increment
             -> (Int -> Int -> Bool) -- ^ Terminating condition
             -> (Int -> ix -> m ()) -- ^ Monadic action that takes index in both forms
             -> m ()
iterLinearM_ !sz !k0 !k1 !inc cond f =
  loopM_ k0 (`cond` k1) (+ inc) $ \ !i -> f i (fromLinearIndex sz i)
{-# INLINE iterLinearM_ #-}

-- | Iterate over N-dimensional space from start to end with accumulator
iterLinearM :: (Index ix, Monad m)
            => ix
            -> Int
            -> Int
            -> Int
            -> (Int -> Int -> Bool)
            -> a
            -> (Int -> ix -> a -> m a)
            -> m a
iterLinearM !sz !k0 !k1 !inc cond !acc f =
  loopM k0 (`cond` k1) (+ inc) acc $ \ !i !acc0 -> f i (fromLinearIndex sz i) acc0
{-# INLINE iterLinearM #-}


-- | Very efficient loop with an accumulator
loop :: Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> a) -> a
loop !init' condition increment !initAcc f = go init' initAcc where
  go !step !acc =
    case condition step of
      False -> acc
      True  -> go (increment step) (f step acc)
{-# INLINE loop #-}


-- | Very efficient monadic loop
loopM_ :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m a) -> m ()
loopM_ !init' condition increment f = go init' where
  go !step =
    case condition step of
      False -> return ()
      True  -> f step >> go (increment step)
{-# INLINE loopM_ #-}


-- | Very efficient monadic loop with an accumulator
loopM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopM !init' condition increment !initAcc f = go init' initAcc where
  go !step acc =
    case condition step of
      False -> return acc
      True  -> f step acc >>= go (increment step)
{-# INLINE loopM #-}
