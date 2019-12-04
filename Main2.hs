{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad              ((<=<))
import           Data.Maybe
import           Data.Sequence              ((!?))
import qualified Data.Sequence              as S
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Text.Megaparsec            hiding (State)
import qualified Text.Megaparsec            as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

main = do
  contents <- TIO.readFile "input2"
  vals <- parseIt (decimal `sepBy` char ',') contents
  let parsedsops = S.fromList vals
  print "output from day1 input1"
  print $ (opcode 0 (S.fromList day1input1)) !. 0
  let allPossible = [opcode 0 $ S.fromList $ day2input2 a b | a <- [0..99], b <- [0..99]]
      (_,noun,verb) = head $ filter (\(a,_,_) -> a == 19690720) $ (\x -> (x !. 0,x !. 1, x !. 2)) <$> allPossible
  print "output from day1 part 2"
  print $ 100 * noun + verb


test:: [Int]
test = [1,9,10,3,2,3,11,0,99,30,40,50]

testops = S.fromList test

tests = and [ opcode 0 (S.fromList [1,0,0,0,99]) == S.fromList [2,0,0,0,99]
            , opcode 0 (S.fromList [2,3,0,3,99]) == S.fromList [2,3,0,6,99]
            , opcode 0 (S.fromList [2,4,4,5,99,0]) == S.fromList [2,4,4,5,99,9801]
            , opcode 0 (S.fromList [1,1,1,4,99,5,6,0,99]) == S.fromList [30,1,1,4,2,5,6,0,99]
            , opcode 0 (S.fromList [1,9,10,3,2,3,11,0,99,30,40,50]) == S.fromList [3500,9,10,70,2,3,11,0,99,30,40,50]
            ]

(!.) :: S.Seq a -> Int -> a
s !. i = fromJust $ s !? i -- ugh

opcode n ops =
    case fromJust $ ops !? n of
      1  -> let ops' = S.update (ops !. (n+3)) (sum [ops !. (ops !. (n+1)),ops !. (ops !. (n+2))]) ops
            in opcode (n+4) ops'
      2  -> let ops' = S.update (ops !. (n+3)) (product [ops !. (ops !. (n+1)),ops !. (ops !. (n+2))]) ops
            in opcode (n+4) ops'
      99 -> ops

day1input1 = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0]
day2input2 a b = [1,a,b,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0]

decimal :: Parser Int
decimal = do
  cs <- try (M.some digitChar)
  pure (read cs :: Int)

parseIt p t =
    case runParser p "" t of
      Left e  -> fail (errorBundlePretty e)
      Right a -> pure a
