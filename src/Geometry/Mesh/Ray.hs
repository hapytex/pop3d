{-# LANGUAGE Safe #-}

module Geometry.Mesh.Ray (
    Ray(Ray, origin, direction, near, far)
  , Hittable(rayHits, rayHitsWithFast, rayHitsAt)
  ) where

import Geometry.Mesh.Base(P3)

data Ray a = Ray {
    origin :: P3 a
  , direction :: P3 a
  , near :: a
  , far :: a
  } deriving (Eq, Ord, Read, Show)

class Hittable f where
    rayHits :: (Num a, Ord a) => Ray a -> f a -> Bool

    rayHitsWithFast :: (Num a, Ord a, Hittable fast) => Ray a -> (f a, fast a) -> Bool
    rayHitsWithFast ray ~(o, e) = rayHits ray e && rayHits ray o
    {-# MINIMAL rayHits #-}
