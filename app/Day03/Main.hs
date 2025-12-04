module Day03.Main (day) where

import Utils.Prelude

type Result = Int

type ParsedInput = AllBanks

type Battery = Int

type Bank = [Battery]

type AllBanks = [Bank]



parse :: Parser ParsedInput
step1, step2 :: ParsedInput -> IO Result
day :: AOCDay Result Result ParsedInput ParsedInput
parse = withEof . onePerLine . many1 $ read . singleton <$> digit

bumpElement :: [Int] -> Int -> [Int]
bumpElement originalList e =
  let tuples = pairs $ originalList ++ [e]
      (clean, rest) =
        join bimap (fmap fst) $
          break (\(l, r) -> r > l) tuples
      secondPart = case rest of
        [] -> []
        (_ : xs) -> xs ++ [e]
   in clean ++ secondPart

highestJoltageN :: Int -> Bank -> Int
highestJoltageN n ls =
  let (initialOn, rest) = splitAt n ls
      recurs currentOn [] = currentOn
      recurs currentOn (newElem : xs) = recurs (bumpElement currentOn newElem) xs
   in read $ concatMap show (recurs initialOn rest)

step1 input = sum <$> highestJoltageN 2 `printMap` input

step2 input = sum <$> highestJoltageN 12 `printMap` input

day = AOCDay 3 (parse, step1) (parse, step2) True
