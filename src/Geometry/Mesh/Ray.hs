{-# LANGUAGE Safe #-}

module Geometry.Mesh.Ray (
    Ray(Ray, origin, direction, near, far)
  ) where

import Linear.V3(V3)

data Ray a = Ray {
    origin :: V3 a
  , direction :: V3 a
  , near :: a
  , far :: a
  } deriving (Eq, Ord, Read, Show)
