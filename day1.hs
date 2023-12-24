import Data.Char (ord)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)

lineSplit :: String -> [String]
lineSplit "\n" = []
lineSplit [] = []
lineSplit s = takeWhile (/= '\n') s : (lineSplit . tail) (dropWhile (/= '\n') s)

toResult digits = 10 * head digits + last digits

getNumber :: String -> Int
getNumber s = toResult digits
  where
    digits :: [Int]
    digits = (filter (\i -> i >= 0 && i <= 9) . map (\c -> ord c - ord '0')) s

parse :: String -> Int
parse ('0' : xs) = 0
parse ('1' : xs) = 1
parse ('2' : xs) = 2
parse ('3' : xs) = 3
parse ('4' : xs) = 4
parse ('5' : xs) = 5
parse ('6' : xs) = 6
parse ('7' : xs) = 7
parse ('8' : xs) = 8
parse ('9' : xs) = 9
parse ('o' : 'n' : 'e' : xs) = 1
parse ('t' : 'w' : 'o' : xs) = 2
parse ('t' : 'h' : 'r' : 'e' : 'e' : xs) = 3
parse ('f' : 'o' : 'u' : 'r' : xs) = 4
parse ('f' : 'i' : 'v' : 'e' : xs) = 5
parse ('s' : 'i' : 'x' : xs) = 6
parse ('s' : 'e' : 'v' : 'e' : 'n' : xs) = 7
parse ('e' : 'i' : 'g' : 'h' : 't' : xs) = 8
parse ('n' : 'i' : 'n' : 'e' : xs) = 9
parse (c : xs) = parse xs

parse' :: String -> Int
parse' ('0' : xs) = 0
parse' ('1' : xs) = 1
parse' ('2' : xs) = 2
parse' ('3' : xs) = 3
parse' ('4' : xs) = 4
parse' ('5' : xs) = 5
parse' ('6' : xs) = 6
parse' ('7' : xs) = 7
parse' ('8' : xs) = 8
parse' ('9' : xs) = 9
parse' ('e' : 'n' : 'o' : xs) = 1
parse' ('o' : 'w' : 't' : xs) = 2
parse' ('e' : 'e' : 'r' : 'h' : 't' : xs) = 3
parse' ('r' : 'u' : 'o' : 'f' : xs) = 4
parse' ('e' : 'v' : 'i' : 'f' : xs) = 5
parse' ('x' : 'i' : 's' : xs) = 6
parse' ('n' : 'e' : 'v' : 'e' : 's' : xs) = 7
parse' ('t' : 'h' : 'g' : 'i' : 'e' : xs) = 8
parse' ('e' : 'n' : 'i' : 'n' : xs) = 9
parse' (c : xs) = parse' xs

getNumber' s = 10 * parse s + parse' (reverse s)

main = do
  input <- readFile "input/day1.txt"
  -- part I
  print . show $ sum (map getNumber (lineSplit input))
  -- part II
  print . show $ sum (map getNumber' (lineSplit input))
