{-# LANGUAGE FlexibleContexts, Safe #-}

module Geometry.Mesh.Parser.ObjParser where

import Data.List.NonEmpty(NonEmpty((:|)))

import Geometry.Mesh.Internal(eolf, spaceFloating, toEndOfLine)

import Linear.V3(V3(V3))

import Text.Parsec(ParsecT, Stream, many, optional)
import Text.Parsec.Char(char)
import Text.Parsec.Number(int)

vertex :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
vertex = char 'v' *> (V3 <$> go <*> go <*> go) <* optional go <* eolf
    where go = spaceFloating



findex :: (Integral i, Stream s m Char) => ParsecT s u m (NonEmpty i)
findex = (:|) <$> int <*> many (char '/' *> int)

comment :: Stream s m Char => ParsecT s u m ()
comment = char '#' *> toEndOfLine *> eolf
