{-# LANGUAGE FlexibleContexts, Safe #-}

module Geometry.Mesh.Parser.ObjParser where

import Geometry.Mesh.Internal(eolf, spaceFloating, toEndOfLine)

import Linear.V3(V3(V3))

-- import Text.Parsec.ByteString.Lazy()
-- import Text.Parsec.Prim
import Text.Parsec(ParsecT, Stream)
import Text.Parsec.Char(char)
-- import Text.Parsec.Combinator


vertex :: (Floating a, Stream s m Char) => ParsecT s u m (V3 a)
vertex = char 'v' *> (V3 <$> spaceFloating <*> spaceFloating <*> spaceFloating) <* eolf

comment :: Stream s m Char => ParsecT s u m ()
comment = char '#' *> toEndOfLine *> eolf
