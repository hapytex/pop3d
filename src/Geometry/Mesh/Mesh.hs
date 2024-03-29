{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, Safe #-}

module Geometry.Mesh.Mesh (
    Mesh(Mesh)
  ) where

import Control.DeepSeq(NFData)

import Data.Data(Data)
import Data.Foldable(foldl')

import Geometry.Mesh.Base(SurfaceEstimate(surfaceEstimate'))
import Geometry.Mesh.Box(Boxable(box'))
import Geometry.Mesh.Internal(maxv3, minv3, fMaybe)
import Geometry.Mesh.Ray(Hittable(rayHits, rayHitsAt', rayHitsFirstAt))
import Geometry.Mesh.Transform(Transformable(transform, scale, scale', shift, shift', rotate, rotate', rotateX, rotateY, rotateZ))

import GHC.Generics(Generic, Generic1)

newtype Mesh f t a
  = Mesh (f (t a))
  deriving (Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance NFData (f (t a)) => NFData (Mesh f t a)

instance Semigroup (f (t a)) => Semigroup (Mesh f t a) where
    ~(Mesh fa) <> ~(Mesh fb) = Mesh (fa <> fb)

instance Monoid (f (t a)) => Monoid (Mesh f t a) where
    mempty = Mesh mempty

instance (Foldable f, SurfaceEstimate s) => SurfaceEstimate (Mesh f s) where
    surfaceEstimate' ~(Mesh ms) = foldl' ((. surfaceEstimate') . (+)) 0 ms

instance (Boxable g, Functor f, Foldable f) => Boxable (Mesh f g) where
    box' ~(Mesh trs) = foldl1 f (fmap box' trs)
        where f ~(a0, a1) ~(b0, b1) = (minv3 a0 b0, maxv3 a1 b1)

instance (Functor f, Transformable g) => Transformable (Mesh f g) where
    transform t ~(Mesh itms) = Mesh (transform t <$> itms)
    scale t ~(Mesh itms) = Mesh (scale t <$> itms)
    scale' sx sy sz ~(Mesh itms) = Mesh (scale' sx sy sz <$> itms)
    shift dx dy dz ~(Mesh itms) = Mesh (shift dx dy dz <$> itms)
    shift' d ~(Mesh itms) = Mesh (shift' d <$> itms)
    rotate axis a ~(Mesh itms) = Mesh (rotate axis a <$> itms)
    rotate' axis a ~(Mesh itms) = Mesh (rotate' axis a <$> itms)
    rotateX t ~(Mesh itms) = Mesh (rotateX t <$> itms)
    rotateY t ~(Mesh itms) = Mesh (rotateY t <$> itms)
    rotateZ t ~(Mesh itms) = Mesh (rotateZ t <$> itms)

instance (Foldable f, Hittable g) => Hittable (Mesh f g) where
    rayHits ray ~(Mesh es) = any (rayHits ray) es
    rayHitsAt' ray ~(Mesh es) tl = foldr (rayHitsAt' ray) tl es
    rayHitsFirstAt ray ~(Mesh es) = foldl' (flip (fMaybe min . rayHitsFirstAt ray)) Nothing es
