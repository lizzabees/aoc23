module Main where

import Control.Arrow ((&&&))
import Data.Char (digitToInt, isDigit)
import Data.List (tails)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Parsec

main :: IO ()
main = do
  input <- fmap lines . readFile . head =<< getArgs
  putStrLn $ mconcat ["part 1: ", show $ part1 input]
  putStrLn $ mconcat ["part 2: ", show $ part2 input]

-- part 1: first valid digit is tens place, last is ones
-- we'll count on laziness and list fusion to be kind to us
-- the first sneaky little trick: there may be only one digit
-- on the line. but the rules still apply eg "abc1def" -> 11
calibration1 :: String -> Int
calibration1 = uncurry (+) . (tens &&& ones)
  where digi = digitToInt . head . filter isDigit
        tens = (10 *) . digi
        ones = digi . reverse

part1 :: [String] -> Int
part1 = sum . map calibration1

-- part 2: the calibration values *may* be spelled out eg "one" "two"
-- note the phrasing, they may still be a digit!
-- the sneaky part: longest match rule in effect! abc1threeight -> 18
type Parser = Parsec String ()

decimal :: Parser Int
decimal = choice $ zipWith value [1..] names
  where value n s = n <$ try (string s)
        names = [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

calibration :: Parser (Maybe Int)
calibration = choice
  [ Just . digitToInt <$> digit
  , Just <$> try decimal
  , Nothing <$ anyChar
  , Nothing <$ eof
  ]

calibrations :: String -> [Int]
calibrations = mapMaybe (mustParse calibration) . tails

mustParse :: Parser a -> String -> a
mustParse p = either (error . show) id . runParser p () "string"

calibration2 :: String -> Int
calibration2 = uncurry (+) . (tens &&& last) . calibrations
  where tens = (10 *) . head

part2 :: [String] -> Int
part2 = sum . map calibration2
