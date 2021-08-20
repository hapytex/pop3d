{-# LANGUAGE OverloadedStrings, Safe #-}

module Image where

bmpHeader :: ByteString
bmpHeader = "\x4d\x42"
