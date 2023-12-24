import Data.Array
import Data.Char
import Data.List
import Debug.Trace

lineSplit :: String -> [String]
lineSplit "\n" = []
lineSplit [] = []
lineSplit s = takeWhile (/= '\n') s : (lineSplit . tail) (dropWhile (/= '\n') s)

dbg :: (Show a) => String -> a -> a
dbg s x = trace (s ++ show x) x

-- Was going to use array for better perf but not gonna make myself more uncomfortable
-- toArray :: [a] -> Array Int a
-- toArray xs = array (1, length xs) $ zip [1 ..] xs
-- toMatrix :: [[a]] -> Array Int (Array Int a)
-- toMatrix xss = array (1, length xss) $ zip [1 ..] (map toArray xss)

-- Part One
data PartOneState = POState
  { row :: Int,
    col :: Int,
    match :: Bool,
    accumulator :: [Char],
    totalSum :: Int
  }

toNumber :: [Char] -> Int
toNumber s = read s :: Int

notNull :: [a] -> Bool
notNull = not . null

solvePartOne :: [String] -> Int
solvePartOne input = totalSum $ foldl solveLine (POState 0 0 False [] 0) input
  where
    rmax = length input - 1
    cmax = length (head input) - 1

    around :: (Int, Int) -> [(Int, Int)]
    around (r, c) = [(r', c') | r' <- [max (r - 1) 0 .. min (r + 1) rmax], c' <- [max (c - 1) 0 .. min (c + 1) cmax]]

    peek :: (Int, Int) -> Char
    peek (r, c) = input !! r !! c

    hasSymAround :: (Int, Int) -> Bool
    hasSymAround (r, c) = notNull [c | ch <- map peek (around (r, c)), (not . isDigit) ch, ch /= '.']

    solveLine :: PartOneState -> String -> PartOneState
    -- time for next row
    solveLine (POState r c m acc s) []
      | m && notNull acc = POState (r + 1) 0 False [] (s + toNumber acc)
      | otherwise = POState (r + 1) 0 False [] s
    -- in middle of line
    solveLine (POState r c m acc s) (x : xs)
      | not (isDigit x) =
          -- check whether we have to incr the total sum
          if m && notNull acc
            then solveLine (POState r (c + 1) False [] (s + toNumber acc)) xs
            else solveLine (POState r (c + 1) False [] s) xs
      -- look around for non-dot symbols
      | otherwise = solveLine (POState r (c + 1) (m || hasSymAround (r, c)) (acc ++ [x]) s) xs -- match!

-- Part Two
type PartTwoState = (Int, Int, Int)

type Coord = (Int, Int)

getRow (x, _, _) = x

getCol (_, y, _) = y

getSum (_, _, s) = s

solvePartTwo :: [String] -> Int
solvePartTwo input = getSum $ foldl scanLine (0, 0, 0) input
  where
    rmax = length input - 1
    cmax = length (head input) - 1

    around :: Coord -> [Coord]
    around (r, c) = [(r', c') | r' <- [max (r - 1) 0 .. min (r + 1) rmax], c' <- [max (c - 1) 0 .. min (c + 1) cmax]]

    peek :: Coord -> Char
    peek (r, c) = input !! r !! c

    scanLine :: PartTwoState -> String -> PartTwoState
    scanLine (r, c, s) xs = case xs of
      -- end of line
      [] -> (r + 1, 0, s)
      -- look around *s
      ('*' : rest) -> scanLine (r, c + 1, s + lookForTwo (r, c)) rest
      (_ : rest) -> scanLine (r, c + 1, s) rest

    lookForTwo :: Coord -> Int
    lookForTwo cd = case numsAround (filter (isDigit . peek) (around cd)) of
      [x, y] -> x * y
      _ -> 0

    -- Too complicated for a fold unfourtunately
    numsAround :: [Coord] -> [Int]
    numsAround [] = []
    numsAround (cd : cds) = num : numsAround (cds \\ traversed)
      where
        (num, traversed) = numAround cd

    -- returns the number and the coordinates we have visited - or we might count this number twice!
    numAround :: Coord -> (Int, [Coord])
    numAround (r, c) = (toNumber (h ++ t), path_h `union` path_t)
      where
        (h, path_h) = lookBack (r, c) ([], [])
        (t, path_t) = lookForward (r, c + 1) ([], [])

    lookBack :: Coord -> ([Char], [Coord]) -> ([Char], [Coord])
    lookBack cd@(r, c) res@(cs, cds)
      | c < 0 || (not . isDigit . peek) cd = res
      | otherwise = lookBack (r, c - 1) (peek cd : cs, cd : cds)

    lookForward :: Coord -> ([Char], [Coord]) -> ([Char], [Coord])
    lookForward cd@(r, c) res@(cs, cds)
      | c > cmax || (not . isDigit . peek) cd = res
      | otherwise = lookForward (r, c + 1) (cs ++ [peek cd], cd : cds)

main = do
  input <- readFile "input/day3.txt"
  -- part I
  -- print . show $ solvePartOne (lineSplit input)
  -- part II
  print . show $ solvePartTwo (lineSplit input)
