module Utils.AOC
  ( AOCDay (..),
    runDay,
    benchDay,
    idParse,
    onePerLine,
    spaceChar,
    manySpaces,
    integer,
    sepBySpaces,
    sepBySpaces1,
    withEof,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Trans.Except
import Criterion.Main (Benchmarkable, whnfIO)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor
import Data.Maybe (maybeToList)
import System.IO.Silently
import Text.Parsec
  ( SourceName,
    anyChar,
    char,
    digit,
    endOfLine,
    eof,
    many,
    many1,
    optionMaybe,
    optional,
    parse,
    sepBy,
    sepBy1,
  )
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.Printf (printf)
import Utils.ColorText (Attribute (..), Color (..), decorate)

type DayParser outType = Parser outType

type AOCStep parsedType result = (DayParser parsedType, parsedType -> IO result)

data AOCDay step1Res step2Res step1Type step2Type = AOCDay {num :: Int, firstStar :: AOCStep step1Type step1Res, secondStar :: AOCStep step2Type step2Res, real :: Bool}

data AOCException = ParseExcept ParseError | IOExcept IOError deriving (Show)

firstStarCol, secondStarCol, errorCol, resultCol :: (Color, Color, Attribute)
firstStarCol = (White, NoColor, Underscore)
secondStarCol = (Yellow, NoColor, Underscore)
errorCol = (Red, NoColor, Bold)
resultCol = (Green, NoColor, Bold)

idParse :: Parser String
idParse = many anyChar

withEof :: Parser a -> Parser a
withEof a = do
  res <- a
  _ <- eof
  return res

integer :: Parser Int
integer = do
  d <- many1 digit
  return $ read d

spaceChar :: Parser ()
spaceChar = char ' ' $> ()

manySpaces :: Parser ()
manySpaces = many1 spaceChar $> ()

sepBySpaces :: Parser a -> Parser [a]
sepBySpaces a = sepBy a manySpaces

sepBySpaces1 :: Parser a -> Parser [a]
sepBySpaces1 a = sepBy1 a manySpaces

onePerLine :: Parser a -> Parser [a]
onePerLine lineParser =
  do
    fs <- many $ lineParser <* endOfLine
    rest <- optionMaybe lineParser <&> maybeToList
    optional $ many endOfLine
    return $ fs ++ rest

-- only run real
benchDay :: (Show a) => (Show b) => AOCDay a b i ii -> Benchmarkable
benchDay day = whnfIO $ do
  real_res <- silence $ runExceptT $ runDayInput day (printf "app/Day%02d/input.txt" (num day)) True
  case real_res of
    Left e -> putStrLn $ printf "Error : %s" (show e)
    Right _ -> return ()

parseAndRun :: (DayParser i, i -> IO a) -> SourceName -> Bool -> ExceptT AOCException IO a
parseAndRun (parser, runner) filepath silent =
  let parserFull = parser
      withSilence = if silent then silence else id
   in do
        unparsedResult <- liftIO $ try (readFile filepath)
        unparsed <- liftEither $ first IOExcept unparsedResult
        input <- liftEither $ first ParseExcept $ parse parserFull filepath unparsed
        result <- lift $ withSilence $ runner input
        lift $ return result

runDayInput :: (Show a) => (Show b) => AOCDay a b i ii -> String -> Bool -> ExceptT AOCException IO ()
runDayInput day filePath silent = do
  liftIO $ putStrLn $ decorate "- First Star" firstStarCol
  a <- parseAndRun (firstStar day) filePath silent
  liftIO $ putStrLn $ flip decorate resultCol $ printf "Result : %s" (show a)
  liftIO $ putStrLn $ decorate "- Second Star" secondStarCol
  b <- parseAndRun (secondStar day) filePath silent
  liftIO $ putStrLn $ flip decorate resultCol $ printf "Result : %s" (show b)
  return ()

runDay :: (Show a) => (Show b) => AOCDay a b i ii -> IO ()
runDay day =
  let test_file = printf "app/Day%02d/input_test.txt" (num day)
      real_file = printf "app/Day%02d/input.txt" (num day)
   in do
        putStrLn ""
        putStrLn ""
        putStrLn $ flip decorate (Blue, NoColor, Null) $ printf " ------ Day %d ------ " (num day)
        putStrLn $ printf "--> Test File (%s)" test_file
        test_res <- runExceptT $ runDayInput day test_file False
        case test_res of
          Left e -> putStrLn $ decorate (printf "Error : %s" (show e)) errorCol
          Right _ -> return ()
        Control.Monad.when (real day) $ do
          putStrLn $ printf "--> Real File (silent) (%s)" real_file
          real_res <- runExceptT $ runDayInput day real_file True
          case real_res of
            Left e -> putStrLn $ decorate (printf "Error : %s" (show e)) errorCol
            Right _ -> return ()
