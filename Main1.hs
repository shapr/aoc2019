module Main where

main :: IO ()
main = do
  contents <- readFile "input1"
  let strings = lines contents
      nums = map (read :: String -> Int) strings
      part1 = sum $ launch <$> nums
      part2 = sum $ concatMap (\x -> drop 1 $ takeWhile (> 0) $ iterate launch x) nums
  print part1
  print part2

launch n = (floor $ fromIntegral (n `div` 3)) - 2
