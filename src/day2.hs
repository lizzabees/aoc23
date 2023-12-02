module Main where

import Prelude hiding (round)
import Data.Char (digitToInt)
import System.Environment (getArgs)
import Text.Parsec

type Parser = Parsec String ()

type Color = (Int, Int, Int)

natural :: Parser Int
natural = foldl go 0 <$> many1 digit
  where go acc c = acc * 10 + digitToInt c

color :: Parser Color
color = do
  n <- natural <* char ' '
  choice
    [ (n, 0, 0) <$ string "red"
    , (0, n, 0) <$ string "green"
    , (0, 0, n) <$ string "blue"
    ]

trimax :: Color -> Color -> Color
trimax (lr, lg, lb) (rr, rg, rb) = (max lr rr, max lg rg, max lb rb)

around :: Color -> Color -> Bool
around (lr, lg, lb) (rr, rg, rb) = lr >= rr && lg >= rg && lb >= rb

type Round = [Color]

round :: Parser Round
round = sepBy1 color (string ", ")

type Game = (Int, [Round])

game :: Parser Game
game = do
  number <- string "Game " *> natural <* string ": "
  rounds <- sepBy1 round $ string "; "
  return (number, rounds)

maxcol :: [Color] -> Color
maxcol = foldr trimax (0, 0, 0)

maxgame :: [Round] -> Color
maxgame = maxcol . map maxcol

possible :: Game -> Bool
possible = around (12, 13, 14) . maxgame . snd

part1 :: [Game] -> Int
part1 = sum . map fst . filter possible

power :: Color -> Int
power (r, g, b) = r * g * b

part2 :: [Game] -> Int
part2 = sum . map (power . maxgame . snd)

readInput :: FilePath -> IO [Game]
readInput = fmap right . parse 
  where games = sepEndBy1 game (char '\n') <* eof
        right = either (error . show) id
        parse path = runParser games () path <$> readFile path

main :: IO ()
main = do
  input <- readInput . head =<< getArgs
  putStrLn $ mconcat ["part 1: ", show $ part1 input]
  putStrLn $ mconcat ["part 2: ", show $ part2 input]
