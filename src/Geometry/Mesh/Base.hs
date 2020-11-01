{-# LANGUAGE FlexibleInstances, Safe #-}

module Geometry.Mesh.Base where

import Linear.V3(V3)

-- The type of value used for points, matrices, etc.
type Scalar = Double

type P3 a = V3 a
type Point3 = P3 Scalar

_min2 :: Ord a => (a, a) -> (a, a) -> (a, a)
_min2 ~(a1, b1) ~(a2, b2) = (max a1 a2, min b1 b2)

_min2unord2 :: Ord a => (a, a) -> (a, a) -> (a, a)
_min2unord2 ~(a1, b1) ~(a2, b2)
    | a2 <= b2 = (max a1 a2, min b1 b2)
    | otherwise = (max a1 b2, min b1 a2)

_swapOrd :: Ord a => (a, a) -> (a, a)
_swapOrd ~(x, y)
    | x <= y = (x, y)
    | otherwise = (y, x)


class SurfaceEstimate f where
    -- the surface estimate returns an estimate of the surface area squared and
    -- times two. This makes it more convenient since the number type does not
    -- need to represent a square root, and comparing the values is still
    -- possible.
    surfaceEstimate' :: Num a => f a -> a

instance SurfaceEstimate V3 where
    surfaceEstimate' = const 0
