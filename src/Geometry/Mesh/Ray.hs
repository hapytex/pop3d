{-# LANGUAGE Safe #-}

module Geometry.Mesh.Ray (
    Ray(Ray, origin, direction, near, far)
  , HitPoint(HitPoint, tHitNumerator, tHitDenominator)
  , Hittable(rayHits, rayHitsWithFast, rayHitsAt, rayHitsAt')
  ) where

import Geometry.Mesh.Base(P3)
import Geometry.Mesh.Internal(notNull)

data Ray a = Ray {
    origin :: P3 a
  , direction :: P3 a
  , near :: a
  , far :: a
  } deriving (Eq, Ord, Read, Show)

data HitPoint a = HitPoint {
    tHitNumerator :: a
  , tHitDenominator :: a
  }

class Hittable f where
    rayHits :: (Num a, Ord a) => Ray a -> f a -> Bool
    rayHits ray = notNull . rayHitsAt ray

    rayHitsWithFast :: (Num a, Ord a, Hittable fast) => Ray a -> (f a, fast a) -> Bool
    rayHitsWithFast ray ~(o, e) = rayHits ray e && rayHits ray o

    rayHitsAt :: (Num a, Ord a) => Ray a -> f a -> [HitPoint a]
    rayHitsAt r c = rayHitsAt' r c []

    rayHitsAt' :: (Num a, Ord a) => Ray a -> f a -> [HitPoint a] -> [HitPoint a]
    rayHitsAt' r c = (rayHitsAt r c ++)
    {-# MINIMAL rayHitsAt | rayHitsAt' #-}
