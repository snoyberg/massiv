{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where



import           Criterion.Main
import           Data.Array.Massiv                 as M
import           Data.Array.Repa                   as R
-- import           Data.Array.Massiv.Common.Ix       as M
import           Prelude                           as P


class ExtraShape sh where
  type ShLower sh :: *
  liftShape :: (Int -> Int) -> sh -> sh
  liftShape2 :: (Int -> Int -> Int) -> sh -> sh -> sh
  unsnocShape :: sh -> (ShLower sh, Int)
  snocShape :: ShLower sh -> Int -> sh


instance ExtraShape R.Z where
  type ShLower Z = Z
  liftShape _ _ = R.Z
  {-# INLINE [1] liftShape #-}
  liftShape2 _ _ _ = R.Z
  {-# INLINE [1] liftShape2 #-}
  unsnocShape = undefined
  snocShape = undefined

instance ExtraShape sh => ExtraShape (sh :. Int) where
  type ShLower (sh :. Int) = sh

  liftShape f (sh R.:. i) = liftShape f sh R.:. f i
  {-# INLINE [1] liftShape #-}
  liftShape2 f (sh1 R.:. i1) (sh2 R.:. i2) = liftShape2 f sh1 sh2 R.:. f i1 i2
  {-# INLINE [1] liftShape2 #-}
  unsnocShape (ix R.:. i) = (ix, i)
  {-# INLINE [1] unsnocShape #-}
  snocShape ix i = (ix R.:. i)
  {-# INLINE [1] snocShape #-}

main :: IO ()
main = do
  let i2 = (200 M.:. 300, 20 M.:. 17) :: (Ix2, Ix2)
      t2 = ((200, 300), (20, 17)) :: (Ix2T, Ix2T)
      r2 = (R.Z R.:. 200 R.:. 300, R.Z R.:. 20 R.:. 17) :: (R.DIM2, R.DIM2)
      i3 = (100 :> 200 M.:. 300, 10 :> 20 M.:. 17) :: (Ix3, Ix3)
      t3 = ((100, 200, 300), (10, 20, 17)) :: (Ix3T, Ix3T)
      r3 = (R.Z R.:. 100 R.:. 200 R.:. 300, R.Z R.:. 10 R.:. 20 R.:. 17) :: (R.DIM3, R.DIM3)
      i4 = (100 :> 200 :> 300 M.:. 40, 10 :> 20 :> 17 M.:. 34) :: (Ix4, Ix4)
      t4 = ((100, 200, 300, 40), (10, 20, 17, 34)) :: (Ix4T, Ix4T)
      r4 = (R.Z R.:. 100 R.:. 200 R.:. 300 R.:. 40, R.Z R.:. 90 R.:. 190 R.:. 290 R.:. 34) :: (R.DIM4, R.DIM4)
  defaultMain [ makeGroup "DIM2" i2 t2 r2
              , makeGroup "DIM3" i3 t3 r3
              , makeGroup "DIM4" i4 t4 r4 ]

makeGroup
  :: forall ix1 ix2 sh. (Index (Lower ix1), Index ix1, Index (Lower ix2), Index ix2, R.Shape sh, ExtraShape sh) =>
     String
     -> (ix1, ix1) -> (ix2, ix2) -> (sh, sh) -> Benchmark
makeGroup groupName !(sz1, i1) !(sz2, i2) !(sz3, i3) =
  bgroup
    groupName
    [ bgroup
        "Regular"
        [ bgroup
            "toLinearIndex"
            [ bench "Ix" $ whnf (toLinearIndex sz1) i1
            , bench "Tuple" $ whnf (toLinearIndex sz2) i2
            , bench "Repa" $ whnf (toIndex sz3) i3
            ]
        , bgroup
            "fromLinearIndex"
            [ bench "Ix" $ whnf (fromLinearIndex sz1) 100
            , bench "Tuple" $ nf (fromLinearIndex sz2) 100
            , bench "Repa" $ whnf (fromIndex sz3) 100
            ]
        , bgroup
            "toFromLinearIndex"
            [ bench "Ix" $ whnf (fromLinearIndex sz1 . toLinearIndex sz1) i1
            , bench "Tuple" $ nf (fromLinearIndex sz2 . toLinearIndex sz2) i2
            , bench "Repa" $ whnf (fromIndex sz3 . toIndex sz3) i3
            ]
        , bgroup
            "totalElem"
            [ bench "Ix" $ whnf totalElem i1
            , bench "Tuple" $ whnf totalElem i2
            , bench "Repa" $ whnf R.size i3
            ]
        , bgroup
            "isSafeIndex"
            [ bench "Ix" $ whnf (isSafeIndex sz1) i1
            , bench "Tuple" $ whnf (isSafeIndex sz2) i2
            , bench "Repa" $ whnf (R.inShapeRange R.zeroDim sz3) i3
            ]
        , bgroup
            "unconsConsDim"
            [ bench "Ix" $ whnf ((consDim 8 . snd . unconsDim) :: ix1 -> ix1) i1
            , bench "Tuple" $
              nf ((consDim 8 . snd . unconsDim) :: ix2 -> ix2) i2
            ]
        , bgroup
            "unsnocSnocDim"
            [ bench "Ix" $
              whnf (((`snocDim` 8) . fst . unsnocDim) :: ix1 -> ix1) i1
            , bench "Tuple" $
              nf (((`snocDim` 8) . fst . unsnocDim) :: ix2 -> ix2) i2
            , bench "Repa" $ whnf (((`snocShape` 8) . fst . unsnocShape) :: sh -> sh) i3
            ]
        , bgroup
            "liftIndex"
            [ bench "Ix" $ whnf (liftIndex succ) i1
            , bench "Tuple" $ nf (liftIndex succ) i2
            , bench "Repa" $ whnf (liftShape succ) i3
            ]
        , bgroup
            "liftIndex2"
            [ bench "Ix" $ whnf (liftIndex2 (+) sz1) i1
            , bench "Tuple" $ nf (liftIndex2 (+) sz2) i2
            , bench "Repa" $ whnf (liftShape2 (+) sz3) i3
            , bench "Repa Add" $ whnf (addDim sz3) i3
            ]
        , bgroup
            "getIndex (1)"
            [ bench "Ix" $ whnf (`getIndex` 1) i1
            , bench "Tuple" $ whnf (`getIndex` 1) i2
            ]
        , bgroup
            "getIndex (rank)"
            [ bench "Ix" $ whnf (`getIndex` M.rank i1) i1
            , bench "Tuple" $ whnf (`getIndex` M.rank i2) i2
            ]
        , bgroup
            "setIndex (1)"
            [ bench "Ix" $ whnf (setIndex i1 1) 8
            , bench "Tuple" $ nf (setIndex i2 2) 8
            ]
        , bgroup
            "setIndex (rank)"
            [ bench "Ix" $ whnf (setIndex i1 (M.rank i1)) 8
            , bench "Tuple" $ nf (setIndex i2 (M.rank i2)) 8
            ]
        , bgroup
            "dropIndex (1)"
            [ bench "Ix" $ whnf (`getIndex` 1) i1
            , bench "Tuple" $ nf (`getIndex` 1) i2
            ]
        , bgroup
            "dropIndex (rank)"
            [ bench "Ix" $ whnf (`dropIndex` M.rank i1) i1
            , bench "Tuple" $ nf (`dropIndex` M.rank i2) i2
            ]
        ]
    , bgroup
        "Expensive"
        [ bgroup
            "iterM"
            [ bench "Ix" $
              nfIO $ iterM i1 sz1 1 (<) 0 (\i acc -> return (totalElem i + acc))
            , bench "Tuple" $
              nfIO $ iterM i2 sz2 1 (<) 0 (\i acc -> return (totalElem i + acc))
            ]
        , bgroup
            "iter"
            [ bench "Ix" $
              nf (\ix -> iter ix sz1 1 (<) 0 (\i acc -> totalElem i + acc)) i1
            , bench "Tuple" $
              nf (\ix -> iter ix sz2 1 (<) 0 (\i acc -> totalElem i + acc)) i2
            ]
        ]
    ]
