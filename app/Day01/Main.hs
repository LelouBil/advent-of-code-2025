module Day01.Main (day) where

import Text.Printf (printf)
import Utils.Prelude

data Direction = LeftRotation | RightRotation

instance Show Direction where
  show LeftRotation = "L"
  show RightRotation = "R"

data Movement = Movement {direction :: Direction, rotation :: Int}

instance Show Movement where
  show (Movement dir rot) = printf "%s%d" (show dir) rot

type ParsedInput = [Movement]

parse :: Parser ParsedInput
parse = withEof $ onePerLine lineParse
  where
    lineParse = do
      dir <- char 'L' $> LeftRotation <|> string "R" $> RightRotation
      Movement dir <$> integer

doTurn :: Int -> Movement -> Int
doTurn oldPos (Movement dir rot) = newPosition
  where
    newPosition = case dir of
      LeftRotation -> oldPos - rot
      RightRotation -> oldPos + rot

type Result = Int

step1 :: ParsedInput -> IO Result
step1 input = do
  let states = scanl doTurn 50 input
  --  print states
  return $ length [x | x <- states, x `mod` 100 == 0]

--step2 :: ParsedInput -> IO Result
--step2 input = do
--  print input
--  let states = scanl doTurn 50 input
--  print states
--  print $ (`mod` 100) <$> states
--  let pairs = zip states (drop 1 states)
--  print pairs
--  let nbZero :: (Int, Int) -> Int
--      nbZero (old, new) =
--        if ((signum old /= signum new)) && (nnew /= 0 && oold /= 0)
----          then
--          then if n == 0 then 1 else n
--          else 0
--        where
--          diff = new - old
----          negdiff =
--          nnew = (new + 100) `mod` 100
--          oold = (old + 100) `mod` 100
--          n = abs ((nnew - oold) `quot` 100)
--  let vals = nbZero <$> pairs
--  let shown = (\(a, b) -> printf "%s -> %s" (show a) (show b) :: String) <$> zip pairs vals
--  let ls = [if x == 0 then 1 else abs $ x `quot` 100 | x <- states]
--  print shown
--  print ls
--  return $ sum vals + sum ls

step2Ez :: ParsedInput -> IO Result
step2Ez input = do
  let flattenInput = concatMap (\(Movement dir rot) -> replicate rot (Movement dir 1)) input
  print flattenInput
  let vals = scanl doTurn 50 flattenInput
  return $ length [x | x <- vals, abs x `mod` 100 == 0]

day :: AOCDay Result Result ParsedInput ParsedInput
day = AOCDay 1 (parse, step1) (parse, step2Ez) True
