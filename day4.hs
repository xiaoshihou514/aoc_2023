import Data.Array
import Data.Char
import Debug.Trace

lineSplit :: String -> [String]
lineSplit "\n" = []
lineSplit [] = []
lineSplit s = takeWhile (/= '\n') s : (lineSplit . tail) (dropWhile (/= '\n') s)

dbg :: (Show a) => String -> a -> a
dbg s x = trace (s ++ " " ++ show x) x

toNumber :: [Char] -> Int
toNumber s = read s :: Int

digitSplit :: String -> [String]
digitSplit [] = []
digitSplit s =
  let (h, t) = break (== ' ') s
   in case t of
        (' ' : xs) -> h : digitSplit (dropWhile (not . isDigit) xs)
        _ -> [h]

getMatch :: String -> Int
-- length + filter traverses the list twice, not ideal ...
getMatch line = foldr (\x acc -> if x `elem` winning then acc + 1 else acc) 0 nums
  where
    -- get rid of Cards x:, and split, then split again
    (winningStr, numStr) = (break (== '|') . tail . dropWhile (/= ':')) line
    parse str = map toNumber $ (digitSplit . dropWhile (not . isDigit)) str
    winning = parse winningStr
    nums = parse numStr

solvePartOne :: [String] -> Int
solvePartOne input = sum $ map calculate input
  where
    calculate :: String -> Int
    calculate line
      | match == 0 = 0
      | otherwise = 2 ^ (match - 1)
      where
        match = getMatch line

solvePartTwo :: [String] -> Int
solvePartTwo input = sum (build input)

build :: [String] -> [Int]
build [] = []
build (l : ls) = 1 + sum (take match rest) : rest
  where
    match = getMatch l
    rest = build ls

main = do
  input <- readFile "input/day4.txt"
  -- part I
  -- print . show $ solvePartOne (lineSplit input)
  -- part II
  print . show $ solvePartTwo (lineSplit input)
  return ()
