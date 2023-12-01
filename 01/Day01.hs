module Day01 where

part1 :: String -> Int
part1 = getSum . lines
  where
    filterToNums :: String -> String
    filterToNums s = [n | n <- s, n `elem` ['0' .. '9']]

    getCalibrationValue :: String -> String
    getCalibrationValue ns
      | null ns = "0"
      | length ns == 1 = ns ++ ns
      | otherwise = head ns : [last ns]

    convertNumsToInt :: String -> Int
    convertNumsToInt = read . getCalibrationValue . filterToNums

    getSum :: [String] -> Int
    getSum = sum . map convertNumsToInt


solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c

main = solve "input.txt"
