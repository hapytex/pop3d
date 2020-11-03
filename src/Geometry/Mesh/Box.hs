{-# LANGUAGE DeriveFoldable, DeriveFunctor, Safe #-}

module Geometry.Mesh.Box (
    Box(Box, boxMin, boxMax)
  , Boxable(box, box', inBox)
  , centroid2
  ) where

import Data.Maybe(isJust)

import Geometry.Mesh.Base(SurfaceEstimate(surfaceEstimate'), P3)
import Geometry.Mesh.Internal(overlap, maxv3, minv3, normalizeDirection)
import Geometry.Mesh.Ray(Ray(Ray), HitPoint(HitPoint), Hittable(rayHits, rayHitsAt'))

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

mergeWith :: (Num a, Ord a) => Bool -> a -> a -> a -> a -> (a, a) -> Maybe (a, a)
mergeWith True _ ox ax bx | ax <= ox && ox <= bx = Just
                          | otherwise = const Nothing
mergeWith _ m ox ax bx = go
    where go ~(a0, a1) | b0 < b1 = Just (b0, b1)
                       | otherwise = Nothing
              where b0 = max (min ba bb) a0
                    b1 = min (max ba bb) a1
          mox = m * ox
          ba = m * ax - mox
          bb = m * bx - mox

_checkHit :: (Num a, Ord a) => (a -> Maybe (a, a) -> b) -> Ray a -> Box a -> b
_checkHit pp ~(Ray ~(V3 ox oy oz) ~(V3 dx dy dz) n f) ~(Box ~(V3 ax ay az) ~(V3 bx by bz)) = pp nxyz (mergeWith zx (ny*nz) ox ax bx (nxyz*n, nxyz*f) >>= mergeWith zy (nx*nz) oy ay by >>= mergeWith zz nxy oz az bz)
        where (zx, nx) = normalizeDirection dx
              (zy, ny) = normalizeDirection dy
              (zz, nz) = normalizeDirection dz
              nxy = nx * ny
              nxyz = nxy * nz


instance Hittable Box where
    rayHits = _checkHit (const isJust)

    rayHitsAt' = _checkHit go
        where go nxyz (Just ~(a, b)) = \tl -> HitPoint a nxyz : HitPoint b nxyz : tl
              go _ ~Nothing = id

instance SurfaceEstimate Box where
    surfaceEstimate' (Box ~(V3 ax ay az) ~(V3 bx by bz)) = 4*dd*dd
        where dx = bx - ax
              dy = by - ay
              dz = bz - az
              dd = dx*dy + dx*dz + dy*dz
