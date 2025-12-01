{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
module Utils.Prelude (
    AOCDay (..),
    onePerLine,
    idParse,
    integer,
    spaceChar,
    sepBySpaces,
    manySpaces,
    sepBySpaces1,
    withEof,
    module Text.Parsec.Char,
    module Text.Parsec,
    module Text.Parsec.String,
    module Data.List,
    module Data.Functor
    ) where
import Utils.AOC (AOCDay (..),idParse,onePerLine,integer,spaceChar,sepBySpaces,sepBySpaces1,manySpaces,withEof)
import Text.Parsec hiding (parse,State)
import Text.Parsec.Char
import Text.Parsec.String
import Data.List hiding (uncons)
import Data.Functor hiding (unzip)
