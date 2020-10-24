{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

module Geometry.Mesh.Accelerator.BoundingVolumeHierarchy where

import Geometry.Mesh.Base(Box, Boxable, Boxed, Mesh)

data BVH f a
  = BVHLeaf [Boxed f a]
  | BVHNode (Box a) (BVH f a) (BVH f a)
  deriving (Eq, Foldable, Functor, Ord, Read, Show)

buildBVH :: (Boxable f, Foldable g) => Int -> Mesh g f a -> a
buildBVH = undefined
