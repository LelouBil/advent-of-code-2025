module Day02.Main (day) where

import Text.Printf (printf)
import Utils.Prelude

type Result = Int
type ParsedInput = [(Int,Int)]

parse :: Parser ParsedInput
step1,step2 :: ParsedInput -> IO Result
day :: AOCDay Result Result ParsedInput ParsedInput


parse = sepBy idRange (char ',')
      where
        idRange :: Parser (Int,Int)
        idRange = do
            start <- integer
            _ <- char '-'
            end <- integer
            return (start,end)

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l
isInvalid :: String -> Bool
isInvalid inp = uncurry (==) splat
    where splat = splitHalf inp

step1 input = do
    let flat = concatMap (\(s,e) -> [s..e] ) input
    let tostr = show <$> flat
    let invalid = filter isInvalid tostr
    print invalid
    return $ sum $ read <$> invalid


isInvalid2 :: String -> Bool
isInvalid2 inp = case runParser (repeatParser $ length inp) () "" inp of
    Left _ -> False
    Right _ -> True

repeatParser :: Int -> Parser ()
repeatParser strlen = do
    let lenMap = [1..(strlen `div` 2)]
    let subParserN :: Int -> Parser ()
        subParserN n = try $ do
            firstn <- replicateM n anyChar
            _ <- manyTill (string firstn) (try eof)
            return ()
    _ <- choice $ subParserN <$> lenMap
    return ()

step2 input = do
    let flat = concatMap (\(s,e) -> [s..e] ) input
    let tostr = show <$> flat
    let invalid = filter isInvalid2 tostr
    print invalid
--    let list = ["1111","1212","1234","55","456456456456"]
--    let invalid = runParser repeatParser () "" <$> list
--    print $ zip list invalid
    return $ sum $ read <$> invalid


day = AOCDay 2 (parse, step1) (parse, step2) True
