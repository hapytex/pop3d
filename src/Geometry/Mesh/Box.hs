{-# LANGUAGE DeriveFoldable, DeriveFunctor, Safe #-}

module Geometry.Mesh.Box (
    Box(Box, boxMin, boxMax)
  , Boxable(box, box', inBox)
  , rayHitsBox
  , centroid2
  ) where

import Geometry.Mesh.Base(SurfaceEstimate(surfaceEstimate'), P3)
import Geometry.Mesh.Internal(overlap, maxv3, minv3, normalizeDirection)
import Geometry.Mesh.Ray(Ray(Ray))

import Linear.V3(V3(V3))

data Box a = Box {
    boxMin :: !(P3 a)
  , boxMax :: !(P3 a)
  } deriving (Eq, Foldable, Functor, Ord, Read, Show)

centroid2 :: Num a => Box a -> V3 a
centroid2 (Box ma mb) = ma + mb

instance Ord a => Semigroup (Box a) where
    ~(Box ami ama) <> ~(Box bmi bma) = Box (minv3 ami bmi) (maxv3 ama bma)

class Boxable f where
    box :: (Num a, Ord a) => f a -> Box a
    box = uncurry Box . box'

    box' :: (Num a, Ord a) => f a -> (V3 a, V3 a)
    box' x = let ~(Box a b) = box x in (a, b)
    
    inBox :: (Num a, Ord a) => f a -> Box a -> Bool
    inBox x ~(Box ~(V3 x0 y0 z0) ~(V3 x1 y1 z1)) = let ~(~(V3 x2 y2 z2), ~(V3 x3 y3 z3)) = box' x in overlap x0 x1 x2 x3 && overlap y0 y1 y2 y3 && overlap z0 z1 z2 z3
    {-# MINIMAL box | box' #-}

instance Boxable Box where
    box = id

instance Boxable V3 where
    box pt = Box pt pt

{-
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


mergeWith :: (Num a, Ord a) => Maybe (a, a) -> a -> a -> a -> Maybe (a, a)
mergeWith Nothing = const (const (const Nothing))
mergeWith b@(Just _) = go
    where go 0 = const (const Nothing)
          go _ = const (const b)
-}

rayHitsBox :: (Ord a, Num a) => Ray a -> Box a -> Bool
rayHitsBox ~(Ray ~(V3 ox oy oz) ~(V3 dx dy dz) n f) ~(Box ~(V3 ax ay az) ~(V3 bx by bz)) = denom > n + f && n * f > ax+bx+ay+by+az+bz && ox < oy + oz
    where denom = normalizeDirection dx * normalizeDirection dy * normalizeDirection dz
{-
    where tx2 = bx - ox
          tx1 = ax - ox
          ty2 = by - oy
          ty1 = ay - oy
          tz2 = bz - oz
          tz1 = az - oz
-}

instance SurfaceEstimate Box where
    surfaceEstimate' (Box ~(V3 ax ay az) ~(V3 bx by bz)) = 4*dd*dd
        where dx = bx - ax
              dy = by - ay
              dz = bz - az
              dd = dx*dy + dx*dz + dy*dz
