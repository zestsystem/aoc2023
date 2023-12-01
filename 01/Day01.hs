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

part2 :: String -> Int
part2 = getSum . lines
  where
    replaceWordsToDigit :: String -> String
    replaceWordsToDigit ('z' : 'e' : 'r' : 'o' : xs) = '0' : replaceWordsToDigit xs
    replaceWordsToDigit ('o' : 'n' : 'e' : xs) = '1' : replaceWordsToDigit xs
    replaceWordsToDigit ('t' : 'w' : 'o' : xs) = '2' : replaceWordsToDigit xs
    replaceWordsToDigit ('t' : 'h' : 'r' : 'e' : 'e' : xs) = '3' : replaceWordsToDigit xs
    replaceWordsToDigit ('f' : 'o' : 'u' : 'r' : xs) = '4' : replaceWordsToDigit xs
    replaceWordsToDigit ('f' : 'i' : 'v' : 'e' : xs) = '5' : replaceWordsToDigit xs
    replaceWordsToDigit ('s' : 'i' : 'x' : xs) = '6' : replaceWordsToDigit xs
    replaceWordsToDigit ('s' : 'e' : 'v' : 'e' : 'n' : xs) = '7' : replaceWordsToDigit xs
    replaceWordsToDigit ('e' : 'i' : 'g' : 'h' : 't' : xs) = '8' : replaceWordsToDigit xs
    replaceWordsToDigit ('n' : 'i' : 'n' : 'e' : xs) = '9' : replaceWordsToDigit xs
    replaceWordsToDigit (x : xs) = x : replaceWordsToDigit xs
    replaceWordsToDigit "" = ""

    filterToNums :: String -> String
    filterToNums s = [n | n <- replaceWordsToDigit s, n `elem` ['0' .. '9']]

    getCalibrationValue :: String -> String
    getCalibrationValue ns
      | null ns = "0"
      | length ns == 1 = ns ++ ns
      | otherwise = head ns : [last ns]

    convertNumsToInt :: String -> Int
    convertNumsToInt = read . getCalibrationValue . filterToNums

    getSum :: [String] -> Int
    -- getSum = foldl (\acc line -> acc + convertNumsToInt line) 0
    getSum = sum . map convertNumsToInt

solve :: String -> IO ()
solve filename = do
  c <- readFile filename
  print $ part1 c
  print $ part2 c

main = solve "input.txt"
