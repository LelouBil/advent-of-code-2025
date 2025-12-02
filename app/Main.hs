module Main (main) where

import Criterion.Main
import Day02.Main (day)
import Utils.AOC (runDay , benchDay)

main :: IO ()
main = do
  let today = day
  runDay today
--  putStrLn "--- Benching ---"
--  defaultMain [bench "day" $ benchDay today]
