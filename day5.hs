-- This answer is broken
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import Debug.Trace

lineSplit :: String -> [String]
lineSplit ls = split ls '\n'

dbg x = trace (show x ++ "\n") x

mdbg s x = trace (s ++ show x ++ "\n") x

-- Man, I just found out there's a builtin function called `words`
split :: Eq a => [a] -> a -> [[a]]
split [] _ = []
split (x : xs) x'
  | x == x' = [] : split xs x'
  | otherwise =
      case split xs x' of
        (s : ss) -> (x : s) : ss
        [] -> [[x]]

type Index = ((Int, Int), Int)

parseMaps :: [String] -> [[Index]]
parseMaps [] = []
parseMaps ls = map parse ls'
  where
    ls' = split ls ""
    parse (_ : input) = map parse' input
    parse' :: String -> Index
    parse' input = ((src, src + range - 1), dest)
      where
        (dest', input') = span isNumber input
        (src', range') = span isNumber (tail input')
        range = read (tail range') :: Int
        dest = read dest' :: Int
        src = read src' :: Int

findAll :: [Int] -> [Index] -> [Int]
findAll input index = map lookUp input
  where
    lookUp x = case [dest + x - srcs | ((srcs, srce), dest) <- index, srcs <= x, srce >= x] of
      [] -> x
      (res : _) -> res

parseSeeds :: String -> [Int]
parseSeeds input = go input'
  where
    input' = dropWhile (/= ' ') input
    go [] = []
    go cs = let (x, rest) = break (== ' ') (tail cs) in (read x :: Int) : go rest

solvePartOne :: [String] -> Int
solvePartOne (l : ls) = minimum $ foldl findAll (parseSeeds l) (parseMaps (tail ls))

-- Part II

transform :: [(Int, Int)] -> [Index] -> [(Int, Int)]
-- we could also try to merge before lookups, but I 'm not sure if it's worth it
-- ... or that I can do it
transform input index = concatMap lookUp input
  where
    lookUp :: (Int, Int) -> [(Int, Int)]
    lookUp iv = dbg $ go iv index

    go iv [] = [iv]
    go (s, e) (((ss, se), d) : is)
      -- inside target
      | s >= ss && e <= se = [(d + s - ss, d + e - ss)]
      -- contains target
      | s < ss && e > se = go (s, ss - 1) is ++ [(d, d + se - ss)] ++ go (se + 1, e) is
      -- starts before target
      | s < ss = go (s, ss - 1) is ++ [(d, d + e - ss)]
      -- ends after target
      | e > se = [(d + s - ss, d + se - ss)] ++ go (se + 1, e) is
      | otherwise = []

parseSeeds' :: String -> [(Int, Int)]
parseSeeds' input = go (parseSeeds input)
  where
    go [] = []
    go (x : x' : xs) = (x, x + x') : go xs

solvePartTwo :: [String] -> Int
solvePartTwo (l : ls) = minimum $ dbg $ map fst $ sort $ (foldl transform seeds maps)
  where
    seeds = parseSeeds' l
    maps = parseMaps (tail ls)

main = do
  input <- readFile "test/day5.txt"
  -- part I
  -- print . show $ solvePartOne (lineSplit input)
  -- part II
  print . show $ solvePartTwo (lineSplit input)
  return ()
