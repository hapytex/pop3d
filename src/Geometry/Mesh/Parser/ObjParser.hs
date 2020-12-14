{-# LANGUAGE FlexibleContexts, Safe #-}

module Geometry.Mesh.Parser.ObjParser where

import Prelude hiding (head)

--import Control.Applicative((<|>))

import Data.List(tails)
import Data.List.NonEmpty(NonEmpty((:|)), head)
import Data.Sequence(Seq, (|>))

import Geometry.Mesh.Internal(eolf, spaced, spaceFloating, ignoreLine)
--import Geometry.Mesh.Mesh(Mesh(Mesh))
--import Geometry.Mesh.Triangle(Triangle(Triangle))

import Linear.V3(V3(V3))

import Text.Parsec(ParsecT, Stream, many, many1, modifyState, optional, try)
import Text.Parsec.Char(char, string)
import Text.Parsec.Number(int)

data ObjParserState a = ObjParserState {
    vertices :: Seq (V3 a)
  , normals :: Seq (V3 a)
  }

--objParser :: (Floating a, Stream s m Char) => ParsecT s u m (Mesh [] Triangle a)
--objParser = do
    -- vs <- skipMany $ normal <|> texture <|> vertex <|> face <|> ignoreLine
    -- (Mesh . concat) <$> many (faces (vs !!))

addVertex :: (Floating a, Stream s m Char) => ParsecT s (ObjParserState a) m (V3 a)
addVertex = do
    v <- vertex
    modifyState (\o@ObjParserState {vertices=vs} -> o {vertices=vs |> v})
    pure v

vertex :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
vertex = baseCoords (char 'v') g h
    where g go = V3 <$> go <*> go <*> go
          h = id

faces :: (Stream s m Char, Integral i) => ParsecT s u m [V3 i]
faces = try (char 'f') *> (toTris <$> go <*> many1 go) <* eolf
    where go = head <$> spaced findex
          toTris i0 is = [ V3 i0 ii ij | (ii : ij : _) <- tails is ]

normal :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
normal = baseCoords (string "vn") g (const (pure ()))
    where g go = V3 <$> go <*> go <*> go

baseCoords :: (Floating b, Stream s m Char) => ParsecT s u m a -> (ParsecT s u m b -> ParsecT s u m d) -> (ParsecT s u m b -> ParsecT s u m c) -> ParsecT s u m d
baseCoords f g h = try f *> g go <* optional (h go) <* eolf
    where go = spaceFloating

comment :: Stream s m Char => ParsecT s u m ()
comment = try (char '#') *> ignoreLine

findex :: (Integral i, Stream s m Char) => ParsecT s u m (NonEmpty i)
findex = (:|) <$> int <*> many (char '/' *> int)
