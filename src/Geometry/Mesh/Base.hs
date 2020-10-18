module Geometry.Mesh.Base where

import Data.List(foldl')
import Data.List.NonEmpty(NonEmpty((:|)))

import Linear.Matrix(M34)
import Linear.V3(V3(V3))
import Linear.V4(V4(V4))

-- The type of value used for points, matrices, etc.
type Scalar = Double

type P3 a = V3 a
type Point3 = P3 Scalar

type Transformer a = M34 a
type Transformation = Transformer Scalar

newtype Triangle a = Triangle (V3 (P3 a)) deriving (Eq, Ord, Read, Show)

newtype Mesh a = Mesh (NonEmpty (Triangle a))

data Box a = Box {
    boxMin :: !(P3 a)
  , boxMax :: !(P3 a)
  }

data Boxed a d = Boxed {
    boxDim :: Box a
  , boxTag :: d
  }

class Boxable f where
    box :: Ord a => f a -> Box a
    box = uncurry Box . box'
    box' :: Ord a => f a -> (V3 a, V3 a)
    box' x = let ~(Box a b) = box x in (a, b)
    boxed :: Ord a => f a -> Boxed a (f a)
    boxed x = Boxed (box x) x
    {-# MINIMAL box | box' #-}

instance Boxable Box where
    box = id

instance Boxable V3 where
    box pt = Box pt pt

_max3 :: Ord a => a -> a -> a -> a
_max3 x = max . max x

_min3 :: Ord a => a -> a -> a -> a
_min3 x = min . min x

_maxv3 :: Ord a => V3 a -> V3 a -> V3 a
_maxv3 (V3 ax ay az) (V3 bx by bz) = V3 (max ax bx) (max ay by) (max az bz)

_minv3 :: Ord a => V3 a -> V3 a -> V3 a
_minv3 (V3 ax ay az) (V3 bx by bz) = V3 (min ax bx) (min ay by) (min az bz)

instance Boxable Triangle where
    box' (Triangle (V3 (V3 ax ay az) (V3 bx by bz) (V3 cx cy cz))) = ((V3 (_min3 ax ay az) (_min3 bx by bz) (_min3 cx cy cz)), (V3 (_max3 ax ay az) (_max3 bx by bz) (_max3 cx cy cz)))

instance Boxable Mesh where
    box' (Mesh (tr :| trs)) = foldl' f (box' tr) trs
        where f ~(a0, a1) ti = let ~(b0, b1) = box' ti in (_minv3 a0 b0, _maxv3 a1 b1)

eye :: Num a => Transformer a
eye = V3 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0)

scaleTransformation :: Num a => a -> Transformer a
scaleTransformation s = V3 (V4 s 0 0 0) (V4 0 s 0 0) (V4 0 0 s 0)

scaleTransformation' :: Num a => a -> a -> a -> Transformer a
scaleTransformation' sx sy sz = V3 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0)

shiftTransformation :: Num a => a -> a -> a -> Transformer a
shiftTransformation dx dy dz = V3 (V4 1 0 0 dx) (V4 0 1 0 dy) (V4 0 0 1 dz)

shiftTransformation' :: Num a => P3 a -> Transformer a
shiftTransformation' (V3 dx dy dz) = V3 (V4 1 0 0 dx) (V4 0 1 0 dy) (V4 0 0 1 dz)

rotateTransformation :: Floating a => P3 a -> a -> Transformer a
rotateTransformation (V3 nx ny nz) = rotateTransformation (V3 (nx*n) (ny*n) (nz*n))
    where n = 1.0 / sqrt (nx*nx + ny*ny + nz*nz)

-- | The axis is a normal: it has length 1, so nx^2+ny^2+nz^2=1.
rotateTransformation' :: Floating a => P3 a -> a -> Transformer a
rotateTransformation' (V3 nx ny nz) a = V3
    (V4 (ca + nx*nx*ca1) (nxy' - nz') (nxz' + ny') 0)
    (V4 (nxy' + nz') (ca + ny*ny*ca1) (nyz' - nx') 0)
    (V4 (nxz' - ny') (nyz' + nx') (ca + ny*ny*ca1) 0)
    where ca = cos a
          ca1 = 1 - ca
          sa = sin a
          nx' = nx * sa
          ny' = ny * sa
          nz' = nz * sa
          nxy' = nx*ny*ca1
          nxz' = nx*nz*ca1
          nyz' = ny*nz*ca1

rotateXTransformation :: Floating a => a -> Transformer a
rotateXTransformation a = V3 (V4 1 0 0 0) (V4 0 ca (-sa) 0) (V4 0 sa ca 0)
    where ca = cos a
          sa = sin a

rotateYTransformation :: Floating a => a -> Transformer a
rotateYTransformation a = V3 (V4 ca 0 sa 0) (V4 0 1 0 0) (V4 (-sa) 0 ca 0)
    where ca = cos a
          sa = sin a

rotateZTransformation :: Floating a => a -> Transformer a
rotateZTransformation a = V3 (V4 ca (-sa) 0 0) (V4 sa ca 0 0) (V4 0 0 1 0)
    where ca = cos a
          sa = sin a

transformTransformation :: Num a => Transformer a -> Transformer a -> Transformer a
transformTransformation (V3 (V4 xx xy xz dx) (V4 yx yy yz dy) (V4 zx zy zz dz)) (V3 (V4 x1 x2 x3 x4) (V4 y1 y2 y3 y4) (V4 z1 z2 z3 z4)) = V3
    (V4 (xx*x1 + xy*y1 + xz*z1 + dx) (xx*x2 + xy*y2 + xz*z2 + dx) (xx*x3 + xy*y3 + xz*z3 + dx) (xx*x4 + xy*y4 + xz*z4 + dx))
    (V4 (yx*x1 + yy*y1 + yz*z1 + dy) (yx*x2 + yy*y2 + yz*z2 + dy) (yx*x3 + yy*y3 + yz*z3 + dy) (yx*x4 + yy*y4 + yz*z4 + dy))
    (V4 (zx*x1 + zy*y1 + zz*z1 + dz) (zx*x2 + zy*y2 + zz*z2 + dz) (zx*x3 + zy*y3 + zz*z3 + dz) (zx*x4 + zy*y4 + zz*z4 + dz))

class Transformable f where
    transform :: Num a => Transformer a -> f a -> f a

    scale :: Num a => a -> f a -> f a
    scale = transform . scaleTransformation

    scale' :: Num a => a -> a -> a -> f a -> f a
    scale' sx sy = transform . scaleTransformation' sx sy

    shift :: Num a => a -> a -> a -> f a -> f a
    shift dx dy = transform . shiftTransformation dx dy

    shift' :: Num a => P3 a -> f a -> f a
    shift' = transform . shiftTransformation'

    rotate :: Floating a => P3 a -> a -> f a -> f a
    rotate axis = transform . rotateTransformation axis

    rotate' :: Floating a => P3 a -> a -> f a -> f a
    rotate' axis = transform . rotateTransformation' axis

    rotateX :: Floating a => a -> f a -> f a
    rotateX = transform . rotateXTransformation

    rotateY :: Floating a => a -> f a -> f a
    rotateY = transform . rotateYTransformation

    rotateZ :: Floating a => a -> f a -> f a
    rotateZ = transform . rotateZTransformation
    {-# MINIMAL transform #-}

instance Transformable V3 where
    transform (V3 (V4 xx xy xz dx) (V4 yx yy yz dy) (V4 zx zy zz dz)) (V3 x y z) = V3 (xx*x + xy*y + xz*z + dx) (yx*x + yy*y + yz*z + dy) (zx*x + zy*y + zz*z + dz)
    scale = fmap . (*)
    scale' sx sy sz (V3 x y z) = V3 (sx*x) (sy*y) (sz*z)
    shift dx dy dz (V3 x y z) = V3 (x + dx) (y + dy) (z + dz)
    shift' = (+)
    rotateX a (V3 x y z) = (V3 x (y*ca-z*sa) (y*sa+z*ca))
        where ca = cos a
              sa = sin a
    rotateY a (V3 x y z) = (V3 (x*ca+z*sa) y (z*ca-x*sa))
        where ca = cos a
              sa = sin a
    rotateZ a (V3 x y z) = (V3 (x*ca-y*sa) (x*sa+y*ca) z)
        where ca = cos a
              sa = sin a

instance Transformable Triangle where
    transform t (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = transform t
    scale t (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = scale t
    scale' sx sy sz (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = scale' sx sy sz
    shift dx dy dz (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = shift dx dy dz
    shift' d (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = shift' d
    rotate axis a (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotate axis a
    rotate' axis a (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotate' axis a
    rotateX t (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotateX t
    rotateY t (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotateY t
    rotateZ t (Triangle (V3 pa pb pc)) = Triangle (V3 (go pa) (go pb) (go pc))
        where go = rotateZ t
