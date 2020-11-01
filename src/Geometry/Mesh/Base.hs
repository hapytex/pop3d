{-# LANGUAGE FlexibleInstances, Safe #-}

module Geometry.Mesh.Base (
    Scalar
  , P3, Point3
  , SurfaceEstimate(surfaceEstimate')
  ) where

import Linear.V3(V3)

-- The type of value used for points, matrices, etc.
type Scalar = Double

type P3 a = V3 a
type Point3 = P3 Scalar

class SurfaceEstimate f where
    -- the surface estimate returns an estimate of the surface area squared and
    -- times two. This makes it more convenient since the number type does not
    -- need to represent a square root, and comparing the values is still
    -- possible.
    surfaceEstimate' :: Num a => f a -> a

instance SurfaceEstimate V3 where
    surfaceEstimate' = const 0
