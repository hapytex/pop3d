{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, Safe #-}

module Geometry.Mesh.Accelerator.BoundingVolumeHierarchy (
    BVH(BVHLeaf, BVHNode)
  , buildBVH
  ) where

import Data.Data(Data)
import Data.Default(Default(def))
import Data.Foldable(foldl', toList)
import Data.Maybe(mapMaybe)
import Data.List(minimumBy, sortOn)

import Geometry.Mesh.Box(Box(Box), Boxable(box), centroid2)
import Geometry.Mesh.Internal(fMaybe, v3x, v3y, v3z, nonEmptyMaybe)
import Geometry.Mesh.Mesh(Mesh(Mesh))
import Geometry.Mesh.Ray(Hittable(rayHits, rayHitsAt', rayHitsFirstAt))

import GHC.Generics(Generic)

import Linear.V3(V3(V3))

data BVH f a
  = BVHLeaf [(f a, Box a)]
  | BVHNode (Box a) (BVH f a) (BVH f a)
  deriving (Data, Eq, Foldable, Functor, Generic, Ord, Read, Show, Traversable)

instance Default (BVH f a) where
    def = BVHLeaf []

-- building up the list of items and increasing the box. We avoid splits where
-- the left or right part is empty.
boxingl :: (Boxable f, Num a, Ord a) => [(f a, Box a)] -> [(Box a, [(f a, Box a)], a)]
boxingl [] = []
boxingl ~(i@(~(_, bi)):is) = go is [i] 1 bi
    where go [] _ _ _ = []
          go ~(x@(~(_, bx)):xs) ls !n b = (b, ls, costFunction n b) : go xs yss (n + 1) (b <> bx)
              where yss = x:ls

boxingr :: (Boxable f, Num a, Ord a) => [(f a, Box a)] -> [(Box a, [(f a, Box a)], a)]
boxingr = reverse . boxingl . reverse

costFunction :: Num a => Int -> Box a -> a
costFunction n ~(Box ~(V3 ax ay az) ~(V3 bx by bz)) = fromIntegral n * dd
        where dx = bx - ax
              dy = by - ay
              dz = bz - az
              dd = dx*dy + dx*dz + dy*dz

boxings :: (Boxable f, Num a, Ord a) => [(f a, Box a)] -> [(a, Box a, [(f a, Box a)], Box a, [(f a, Box a)])]
boxings boxes = zipWith f (boxingl boxes) (boxingr boxes)
    where f ~(ab, ai, ac) ~(bb, bi, bc) = (ac + bc, ab, ai, bb, bi)

bestBoxing :: Ord a => [(a, Box a, [(f a, Box a)], Box a, [(f a, Box a)])] -> Maybe (a, Box a, [(f a, Box a)], Box a, [(f a, Box a)])
bestBoxing [] = Nothing
bestBoxing xs = Just (minimumBy on2 xs)
    where on2 ~(a, _, _, _, _) ~(b, _, _, _, _) = compare a b

{-
bestOfBoxings :: (Boxable f, Num a, Ord a) => [(f a, Box a)] -> Maybe (a, Box a, [(f a, Box a)], Box a, [(f a, Box a)])
bestOfBoxings = bestBoxing . boxings
-}

boxPartitionings :: (Boxable f, Num a, Ord a) => [(f a, Box a)] -> [(a, Box a, [(f a, Box a)], Box a, [(f a, Box a)])]
boxPartitionings bs = go v3x ++ go v3y ++ go v3z
    where go f = boxings (sortOn (f . centroid2 . snd) bs)

bestOfBoxingPartitionings :: (Boxable f, Num a, Ord a) => [(f a, Box a)] -> Maybe (a, Box a, [(f a, Box a)], Box a, [(f a, Box a)])
bestOfBoxingPartitionings = bestBoxing . boxPartitionings

buildBVH :: (Boxable f, Foldable g, Num a, Ord a) => Int -> Mesh g f a -> BVH f a
buildBVH _ ~(Mesh fo)
    | (~(_, b0):es) <- obs = go obs (foldl' ((. snd) . (<>)) b0 es)
    | otherwise = BVHLeaf obs
    where lo = toList fo
          obs = map ((,) <*> box) lo
          go e@(_:_:_:_) b
              | Just (_, ab, aes, bb, bes) <- bestOfBoxingPartitionings e = BVHNode b (go aes ab) (go bes bb)
          go e _ = BVHLeaf e

instance Hittable f => Hittable (BVH f) where
    rayHits ray = go
        where go (BVHLeaf es) = any go' es
              go ~(BVHNode b l r) = gob b && (go l || go r)
              go' = rayHits ray . fst
              gob = rayHits ray
    rayHitsAt' ray = go
        where go (BVHLeaf es) tl = go' tl es
              go ~(BVHNode b l r) tl
                  | gob b = go l (go r tl)
                  | otherwise = tl
              go' = foldr (rayHitsAt' ray . fst)
              gob = rayHits ray
    rayHitsFirstAt ray = go
        where go (BVHLeaf es)  = go' es
              go ~(BVHNode b l r)
                  | gob b = fMaybe min (go l) (go r)
                  | otherwise = Nothing
              go' = nonEmptyMaybe minimum . mapMaybe (rayHitsFirstAt ray . fst)
              gob = rayHits ray
