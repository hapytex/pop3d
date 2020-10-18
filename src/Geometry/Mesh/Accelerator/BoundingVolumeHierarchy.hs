module Geometry.Mesh.Accelerator.BoundingVolumeHierarchy where

import Data.List.NonEmpty(NonEmpty)

import Geometry.Mesh.Base(Boxable, Mesh)

buildBVH :: Boxable f => Int -> Mesh NonEmpty f a -> a
buildBVH = undefined
