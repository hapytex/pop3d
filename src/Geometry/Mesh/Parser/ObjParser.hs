{-# LANGUAGE FlexibleContexts, Safe, TupleSections #-}

module Geometry.Mesh.Parser.ObjParser where

import Prelude hiding (head)

import Control.Applicative((<|>))

import Data.List(tails)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(catMaybes)
import Data.Sequence(Seq, (|>), (!?))
import qualified Data.Sequence as Seq

import Geometry.Mesh.Internal(eolf, ignoreLine, rollIndex, spaced, spaceFloating)
--import Geometry.Mesh.Mesh(Mesh(Mesh))
--import Geometry.Mesh.Triangle(Triangle(Triangle))

import Linear.V2(V2(V2))
import Linear.V3(V3(V3))

import Text.Parsec(ParsecT, Stream, getState, many, many1, modifyState, optional, optionMaybe, skipMany, try)
import Text.Parsec.Char(char, string)
import Text.Parsec.Number(int)

type TriangleConstructor1 a b = Maybe (V3 a) -> Maybe (V3 a) -> Maybe (V2 a) -> b
type TriangleConstructor a b = TriangleConstructor1 a (TriangleConstructor1 a (TriangleConstructor1 a b))
type TrianglePoint a = (V3 a, Maybe (V3 a), Maybe (V2 a))
type MTrianglePoint a = Maybe (TrianglePoint a)
type TriaConstr a b = TrianglePoint a -> TrianglePoint a -> TrianglePoint a -> b

_simpleTriangleItem :: (a -> b) -> (v -> a) -> Maybe v -> w -> x -> b
_simpleTriangleItem f c (Just v) _ _ = f (c v)
_simpleTriangleItem f c _ _ _ = f (c undefined)

simpleTriangle :: TriangleConstructor a (V3 (V3 a))
simpleTriangle = _simpleTriangleItem (_simpleTriangleItem (_simpleTriangleItem id)) V3

data ObjParserState a b = ObjParserState {
    vertices :: Seq (V3 a)
  , normals :: Seq (V3 a)
  , textures :: Seq (V2 a)
  , triangles :: [b]
  }

seqIdx :: Seq a -> Int -> Maybe a
seqIdx = rollIndex (!?) Seq.length

objParser :: (Floating a, Stream s m Char) => TriaConstr a b -> ParsecT s (ObjParserState a b) m [b] -- (Mesh [] Triangle a)
objParser f = do
    skipMany (addVertex <|> addNormal <|> addTexture <|> addFaces f <|> comment)
    reverse . triangles <$> getState

addVertex :: (Floating a, Stream s m Char) => ParsecT s (ObjParserState a b) m ()
addVertex = do
    v <- vertex
    modifyState (\o@ObjParserState {vertices=vs} -> o {vertices=vs |> v})

addNormal :: (Floating a, Stream s m Char) => ParsecT s (ObjParserState a b) m ()
addNormal = do
    n <- normal
    modifyState (\o@ObjParserState {normals=ns} -> o {normals=ns |> n})

addTexture :: (Floating a, Stream s m Char) => ParsecT s (ObjParserState a b) m ()
addTexture = do
    t <- texture
    modifyState (\o@ObjParserState {textures=ts} -> o {textures=ts |> t})

facesToTriangles :: TriaConstr a b -> NonEmpty (MTrianglePoint a) -> [b]
facesToTriangles f (i0 :| is) = catMaybes [ go <*> ii <*> ij | (ii : ij : _) <- reverse (tails is) ]
    where go = f <$> i0


addFaces :: (Floating a, Stream s m Char) => TriaConstr a b -> ParsecT s (ObjParserState a b) m ()
addFaces f = do
    tas <- facesToTriangles f <$> faces
    modifyState (\o@ObjParserState {triangles=tbs} -> o {triangles=tas ++ tbs})

vertex :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
vertex = baseCoords (char 'v') g id
    where g go = V3 <$> go <*> go <*> go

texture :: (Floating a, Stream s m Char) => ParsecT s u m (V2 a)
texture = baseCoords (string "vt") g id
    where g go = V2 <$> go <*> go

normal :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
normal = baseCoords (string "vn") g (const (pure ()))
    where g go = V3 <$> go <*> go <*> go

convertFace :: ObjParserState a b -> (Int, [Maybe Int]) -> MTrianglePoint a
convertFace ObjParserState { vertices=vs, normals=ns, textures=ts } = go
    where go (i0, []) = go' i0 Nothing Nothing
          go (i0, [i1]) = go' i0 i1 Nothing
          go (i0, (i1:i2:_)) = go' i0 i1 i2
          go' i0 i1 i2 = (, i1 >>= ni, i2 >>= ti) <$> vi i0
          vi = seqIdx vs
          ni = seqIdx ns
          ti = seqIdx ts

faces :: Stream s m Char => ParsecT s (ObjParserState a b) m (NonEmpty (MTrianglePoint a))
faces = do
    st <- getState
    fmap (convertFace st) <$> faces'

faces' :: (Stream s m Char, Integral i) => ParsecT s u m (NonEmpty (i, [Maybe i]))
faces' = try (char 'f') *> ((:|) <$> go <*> many1 go) <* eolf
    where go = spaced findex

-- triangles :: Stream s m Char => ParsecT s (ObjParserState a b)
--    
--          toTris i0 is = [ V3 i0 ii ij | (ii : ij : _) <- tails is ]

baseCoords :: (Floating b, Stream s m Char) => ParsecT s u m a -> (ParsecT s u m b -> ParsecT s u m d) -> (ParsecT s u m b -> ParsecT s u m c) -> ParsecT s u m d
baseCoords f g h = try f *> g go <* optional (h go) <* eolf
    where go = spaceFloating

comment :: Stream s m Char => ParsecT s u m ()
comment = try (char '#') *> ignoreLine

findex :: (Integral i, Stream s m Char) => ParsecT s u m (i, [Maybe i])
findex = (,) <$> int <*> many (char '/' *> optionMaybe int)
