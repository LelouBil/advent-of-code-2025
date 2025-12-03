{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module Utils.Prelude
  ( AOCDay (..),
    onePerLine,
    idParse,
    integer,
    spaceChar,
    sepBySpaces,
    manySpaces,
    sepBySpaces1,
    withEof,
    printf,
    printMap,
    printApply,
    pairs,
    module Text.Parsec.Char,
    module Text.Parsec,
    module Text.Parsec.String,
    module Data.List,
    module Data.Functor,
    module Control.Monad,
    module Data.Bifunctor,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Functor hiding (unzip)
import Data.List hiding (uncons)
import Text.Parsec hiding (State, parse)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Printf
import Utils.AOC (AOCDay (..), idParse, integer, manySpaces, onePerLine, sepBySpaces, sepBySpaces1, spaceChar, withEof)


printMap :: (Show a, Show b) => (a -> b) -> [a] -> IO [b]
printMap fn = mapM $ printApply fn

pairs :: [a] -> [(a,a)]
pairs a  = zip a (drop 1 a)

printApply :: (Show a, Show b) => (a->b) -> a -> IO b
printApply fn inp = res <$ printf "%s -> %s\n" (show inp) (show res)
  where res = fn inp

