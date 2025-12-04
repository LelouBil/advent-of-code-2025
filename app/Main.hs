module Main (main) where

import Criterion.Main
import Day03.Main (day)
import Utils.AOC (runDay , benchDay)
import Control.Monad

doBench :: Bool
doBench = True

main :: IO ()
main = do
  let today = day
  runDay today
  when doBench $ do
    putStrLn "--- Benching ---"
    defaultMain [bench "day" $ benchDay today]
