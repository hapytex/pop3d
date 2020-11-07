{-# LANGUAGE FlexibleContexts, Safe #-}

module Geometry.Mesh.Parser.Base where

import Text.Parsec(Stream, ParsecT)

class MeshParser f where
    parseStream :: Stream s m Char => ParsecT s () m f
    parseStream = undefined
