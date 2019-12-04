{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad              ((<=<))
import qualified Data.HashSet               as HS
import           Data.List                  (elemIndex, sort)
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
  contents <- TIO.readFile "input3"
  vals <- parseIt pWire contents
  -- print vals
  let wire1 = sew (0,0) (vals !! 0)
      wire2 = sew (0,0) (vals !! 1)
      (wire1hs, wire2hs) = (HS.fromList wire1,HS.fromList wire2)
  -- print $ "wire 1 first few is" <> (T.pack . show $ take 5 wire1)
  -- print $ "wire 1 is size " <> (T.pack . show $ HS.size wire1hs)
  -- print $ "wire 2 is size " <> (T.pack . show $ HS.size wire2hs)
  let commoncoords = HS.toList $ HS.intersection wire1hs wire2hs
  -- print $ commoncoords
  print "day3 part1"
  print $ fst3 $ head $ take 1 $ sort $ (\(x,y) -> (abs x + abs y ,x,y)) <$> commoncoords
  let find1 = flip elemIndex wire1
      find2 = flip elemIndex wire2
  print "day3 part2"
  print . (2 +) $ fst . head . sort $ (\pair -> ((fromJust $ find1 pair) + (fromJust $ find2 pair),pair)) <$> commoncoords

fst3 (a,_,_) = a

data Dir = R Int | U Int | L Int | D Int deriving (Show, Ord, Eq)

parseIt p t =
    case runParser p "" t of
      Left e  -> fail (errorBundlePretty e)
      Right a -> pure a

decimal :: Parser Int
decimal = do
  cs <- try (M.some digitChar)
  pure (read cs :: Int)

pWire :: Parser [[Dir]]
pWire = (pDir `sepBy` char ',') `sepBy` char '\n'
pDir :: Parser Dir
pDir = R <$ char 'R' <*> decimal
       <|> U <$ char 'U' <*> decimal
       <|> L <$ char 'L' <*> decimal
       <|> D <$ char 'D' <*> decimal

{- how to convert into coords? convert each dir into a list of coords, feed in the origin point?
take the last of the list, feed that into the next part -}
convert (x,y) (R n) = [(x',y) | x' <- [x+1     .. x+n]]
convert (x,y) (L n) = [(x',y) | x' <- [x-1,x-2 .. x-n]]
convert (x,y) (U n) = [(x,y') | y' <- [y+1     .. y+n]]
convert (x,y) (D n) = [(x,y') | y' <- [y-1,y-2 .. y-n]]

sew :: (Int,Int) -> [Dir] -> [(Int,Int)]
sew _ []     = []
sew p (d:ds) = let chunk = convert p d in chunk <> sew (last chunk) ds
