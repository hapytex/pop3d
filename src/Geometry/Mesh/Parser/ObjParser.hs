{-# LANGUAGE FlexibleContexts, Safe #-}

module Geometry.Mesh.Parser.ObjParser where

import Prelude hiding (head)

import Data.List(tails)
import Data.List.NonEmpty(NonEmpty((:|)), head)

import Geometry.Mesh.Internal(eolf, spaced, spaceFloating, toEndOfLine)
import Geometry.Mesh.Mesh(Mesh)
import Geometry.Mesh.Triangle(Triangle(Triangle))

import Linear.V3(V3(V3))

import Text.Parsec(ParsecT, Stream, many, many1, optional)
import Text.Parsec.Char(char)
import Text.Parsec.Number(int)

objParser :: Floating a => ParsecT s u m (Mesh [] Triangle a)
objParser = undefined

vertex :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
vertex = char 'v' *> (V3 <$> go <*> go <*> go) <* optional go <* eolf
    where go = spaceFloating

faces :: (Floating a, Stream s m Char, Integral i) => (i -> V3 a) -> ParsecT s u m [Triangle a]
faces g = char 'f' *> (toTris <$> go <*> many1 go)
    where go = g . head <$> spaced findex
          toTris i0 is = [ Triangle (V3 i0 ii ij) | (ii : ij : _) <- tails is ]

findex :: (Integral i, Stream s m Char) => ParsecT s u m (NonEmpty i)
findex = (:|) <$> int <*> many (char '/' *> int)

comment :: Stream s m Char => ParsecT s u m ()
comment = char '#' *> toEndOfLine *> eolf
