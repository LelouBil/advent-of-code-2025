module Day01.Main (day) where

import Utils.Prelude

import Text.Printf (printf)

data Direction = LeftRotation | RightRotation
instance Show Direction where
    show LeftRotation = "L"
    show RightRotation = "R"

data Movement = Movement {direction :: Direction, rotation :: Int}
instance Show Movement where
    show (Movement dir rot) = printf "%s%d" (show dir) rot

type ParsedInput = [Movement]


parse :: Parser ParsedInput
parse = withEof $ onePerLine lineParse where
    lineParse = do
            dir <- char 'L' $> LeftRotation <|> string "R" $> RightRotation
            Movement dir <$> integer


doTurn :: Int -> Movement -> Int
doTurn oldPos (Movement dir rot) = newPosition  where
    newPosition = case dir of
        LeftRotation -> oldPos - rot
        RightRotation -> oldPos + rot


type Result = Int

step1 :: ParsedInput -> IO Result
step1 input = do
  let states = scanl doTurn 50 input
--  print states
  return $ length [x | x <- states,  x `mod` 100 == 0]

-- does not work :(
step2 :: ParsedInput -> IO Result
step2 input = do
  print input
  let states = scanl doTurn 50 input
  print states
  print $ (`mod` 100) <$> states
  let pairs = zip states (drop 1 states)
  print pairs
  let nbZero :: (Int,Int) -> Int
      nbZero (start,finish) =  divid + loop - ifZero - startZero where
        ifZero = if abs finish `mod` 100 == 0 then 1 else 0
        startZero = if abs start `mod` 100 ==  0 then 1 else 0
        divid = if signum start /= signum finish then 1 else 0
        loop = abs $ abs start `quot` 100 - abs finish `quot` 100
  let vals = nbZero <$> pairs
  let shown = (\(a,b) -> printf "%s -> %s" (show a) (show b) :: String) <$> zip pairs vals
  print shown
  return $ sum vals + length [x | x <- states, abs x `mod` 100 == 0]

step2Ez :: ParsedInput -> IO Result
step2Ez input = do
    let flattenInput = concatMap (\(Movement dir rot) -> replicate rot (Movement dir 1)) input
    print flattenInput
    let vals = scanl doTurn 50 flattenInput
    return $  length [x | x <- vals, abs x `mod` 100 == 0]


day :: AOCDay Result Result ParsedInput ParsedInput
day = AOCDay 1 (parse, step1) (parse, step2Ez) True
