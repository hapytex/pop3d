{-# LANGUAGE FlexibleInstances, Safe #-}

module Geometry.Mesh.Internal (
    overlap
  , max3, min3
  , maxv3, minv3
  , v3x, v3y, v3z
  , dot
  , notNull
  , normalizeDirection, normalizeDirection'
  ) where

import Linear.V3(V3(V3))

overlap :: Ord a => a -> a -> a -> a -> Bool
overlap a0 a1 b0 b1 = a0 <= b1 && b0 <= a1

v3x :: V3 a -> a
v3x ~(V3 x _ _) = x

v3y :: V3 a -> a
v3y ~(V3 _ y _) = y

v3z :: V3 a -> a
v3z ~(V3 _ _ z) = z

dot :: Num a => V3 a -> V3 a -> a
dot ~(V3 ax ay az) ~(V3 bx by bz) = ax*bx + ay*by + az*bz

max3 :: Ord a => a -> a -> a -> a
max3 x = max . max x

min3 :: Ord a => a -> a -> a -> a
min3 x = min . min x

notNull :: Foldable f => f a -> Bool
notNull = not . null

maxv3 :: Ord a => V3 a -> V3 a -> V3 a
maxv3 ~(V3 ax ay az) ~(V3 bx by bz) = V3 (max ax bx) (max ay by) (max az bz)

minv3 :: Ord a => V3 a -> V3 a -> V3 a
minv3 ~(V3 ax ay az) ~(V3 bx by bz) = V3 (min ax bx) (min ay by) (min az bz)

normalizeDirection' :: (Eq a, Num a) => a -> a
normalizeDirection' 0 = 1
normalizeDirection' x = x

normalizeDirection :: (Eq a, Num a) => a -> (Bool, a)
normalizeDirection 0 = (True, 1)
normalizeDirection x = (False, x)
