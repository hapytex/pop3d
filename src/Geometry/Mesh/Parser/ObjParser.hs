{-# LANGUAGE FlexibleContexts, Safe #-}

module Geometry.Mesh.Parser.ObjParser where

import Geometry.Mesh.Internal(eolf, spaceFloating, toEndOfLine)

import Linear.V3(V3(V3))

import Text.Parsec(ParsecT, Stream, optional)
import Text.Parsec.Char(char)

vertex :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
vertex = char 'v' *> (V3 <$> go <*> go <*> go) <* optional go <* eolf
    where go = spaceFloating

comment :: Stream s m Char => ParsecT s u m ()
comment = char '#' *> toEndOfLine *> eolf
