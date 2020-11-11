{-# LANGUAGE FlexibleContexts, Safe #-}

module Geometry.Mesh.Parser.ObjParser where

import Prelude hiding (head)

--import Control.Applicative((<|>))

import Data.List(tails)
import Data.List.NonEmpty(NonEmpty((:|)), head)

import Geometry.Mesh.Internal(eolf, spaced, spaceFloating, ignoreLine)
--import Geometry.Mesh.Mesh(Mesh(Mesh))
--import Geometry.Mesh.Triangle(Triangle(Triangle))

import Linear.V3(V3(V3))

import Text.Parsec(ParsecT, Stream, many, many1, optional)
import Text.Parsec.Char(char, string)
import Text.Parsec.Number(int)

--objParser :: (Floating a, Stream s m Char) => ParsecT s u m (Mesh [] Triangle a)
--objParser = do
    -- vs <- skipMany $ normal <|> texture <|> vertex <|> face <|> ignoreLine
    -- (Mesh . concat) <$> many (faces (vs !!))

vertex :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
vertex = baseCoords (char 'v') g h
    where g go = V3 <$> go <*> go <*> go
          h = id

faces :: (Stream s m Char, Integral i) => ParsecT s u m [V3 i]
faces = try (char 'f') *> (toTris <$> go <*> many1 go) <* eolf
    where go = head <$> spaced findex
          toTris i0 is = [ V3 i0 ii ij | (ii : ij : _) <- tails is ]

normal :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
normal = baseCoords (string "vn")

baseCoords :: (Floating b, Stream s m Char) => ParsecT s u m a -> ParsecT s u m (V3 b)
baseCoords f g h = try h *> (g go) <* optional (h go) <* eolf
    where go = spaceFloating

comment :: Stream s m Char => ParsecT s u m ()
comment = try (char '#') *> ignoreLine

findex :: (Integral i, Stream s m Char) => ParsecT s u m (NonEmpty i)
findex = (:|) <$> int <*> many (char '/' *> int)
