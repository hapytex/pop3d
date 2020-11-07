{-# LANGUAGE DeriveFoldable, DeriveFunctor, Safe #-}

module Geometry.Mesh.Triangle (
    Triangle(Triangle)
  ) where

import Geometry.Mesh.Base(SurfaceEstimate(surfaceEstimate'), P3)
import Geometry.Mesh.Box(Boxable(box'))
import Geometry.Mesh.Ray(Ray(Ray), Hittable(rayHitsAt', rayHitsFirstAt), HitPoint, hitPoint)
import Geometry.Mesh.Internal(maxv3, minv3, dot)
import Geometry.Mesh.Transform(Transformable(transform, scale, scale', shift, shift', rotate, rotate', rotateX, rotateY, rotateZ))

import Linear.V3(V3(V3), cross)

newtype Triangle a = Triangle (V3 (P3 a)) deriving (Eq, Foldable, Functor, Ord, Read, Show)

instance SurfaceEstimate Triangle where
    surfaceEstimate' ~(Triangle ~(V3 ~(V3 ax ay az) ~(V3 bx by bz) ~(V3 cx cy cz))) = dxy*dxy + dxz*dxz + dyz*dyz
        where abx = bx - ax
              aby = by - ay
              abz = bz - az
              acx = cx - ax
              acy = cy - ay
              acz = cz - az
              dxy = abx * acy - aby * acx
              dxz = abx * acz - abz * acx
              dyz = aby * acz - abz * acy

instance Boxable Triangle where
    box' ~(Triangle ~(V3 a b c)) = (minv3 (minv3 a b) c, maxv3 (maxv3 a b) c)

instance Transformable Triangle where
    transform t ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = transform t
    scale t ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = scale t
    scale' sx sy sz ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = scale' sx sy sz
    shift dx dy dz ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = shift dx dy dz
    shift' d ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = shift' d
    rotate axis a ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotate axis a
    rotate' axis a ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotate' axis a
    rotateX t ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotateX t
    rotateY t ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotateY t
    rotateZ t ~(Triangle ~(V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotateZ t

_triangleHitsAt :: (Num a, Ord a) => (HitPoint a -> b) -> b -> Ray a -> Triangle a -> b
_triangleHitsAt pp pq ~(Ray o rd n f) ~(Triangle ~(V3 p0 p1 p2))
    | u >= 0 && v >= 0 && u + v <= a && t >= n*a && t <= n*f = pp (hitPoint t n)
    | otherwise = pq
    where e01 = p1 - p0
          e02 = p2 - p0
          h = cross rd e02
          a = dot e01 h
          s = o - p0
          u = dot s h
          q = cross s e01
          v = dot rd q
          t = dot e02 q

instance Hittable Triangle where
    rayHitsAt' = _triangleHitsAt (:) id
    rayHitsFirstAt = _triangleHitsAt Just Nothing
