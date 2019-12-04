module Main where

import           Data.List

main :: IO ()
main = do
  print $ "part1 tests pass? " <> show tests1
  print "day4 part1"
  print $ length meetCriteria
  print $ "part2 tests pass? " <> show tests2
  print $ "day4 part2"
  print $ length meetCriteria'

meetCriteria = filter checkN allPossible

meetCriteria' = filter checkN' allPossible

allPossible = [265275..781584]

tests1 = and [ (checkN 111111) == True
             , (checkN 223450) == False
             , (checkN 123789) == False
            ]

tests2 = and [ (checkN' 112233) == True
             , (checkN' 112444) == False
             , (checkN' 111122) == True
            ]

checkN n = twoDigits n && increase n

checkN' n = increase n && atLeastOnePair n

sixDigits n = (length $ show n) == 6

twoDigits n = not $ null (filter ((1 <) . length) (group $ show n))

atLeastOnePair n = not $ null $ filter ((2 ==) .  length) (group $ show n)

increase n = let cs = show n in cs == sort cs
