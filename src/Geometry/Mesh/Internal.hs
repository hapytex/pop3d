{-# LANGUAGE FlexibleInstances, Safe #-}

module Geometry.Mesh.Internal (
    overlap
  , max3, min3
  , maxv3, minv3
  ) where

import Linear.V3(V3(V3))

overlap :: Ord a => a -> a -> a -> a -> Bool
overlap a0 a1 b0 b1 = a0 <= b1 && b0 <= a1

max3 :: Ord a => a -> a -> a -> a
max3 x = max . max x

min3 :: Ord a => a -> a -> a -> a
min3 x = min . min x

maxv3 :: Ord a => V3 a -> V3 a -> V3 a
maxv3 ~(V3 ax ay az) ~(V3 bx by bz) = V3 (max ax bx) (max ay by) (max az bz)

minv3 :: Ord a => V3 a -> V3 a -> V3 a
minv3 ~(V3 ax ay az) ~(V3 bx by bz) = V3 (min ax bx) (min ay by) (min az bz)

