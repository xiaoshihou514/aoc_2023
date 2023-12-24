import Data.Char (ord)

lineSplit :: String -> [String]
lineSplit "\n" = []
lineSplit [] = []
lineSplit s = takeWhile (/= '\n') s : (lineSplit . tail) (dropWhile (/= '\n') s)

isInt :: Char -> Bool
isInt c = let i = ord c - ord '0' in (i >= 0) && (i <= 9)

getCount :: String -> (Int, String)
getCount s = (read intStr :: Int, tail rest)
  where
    s' = dropWhile (not . isInt) s
    (intStr, rest) = span isInt s'

-- 12 red, 13 green, 14 blue
overlimit :: String -> (Bool, String)
overlimit [] = (False, [])
overlimit s = case color of
  "red" -> (count > 12, rest)
  "green" -> (count > 13, rest)
  "blue" -> (count > 14, rest)
  where
    (count, part) = getCount s
    (color, rest) = break (\c -> c == ',' || c == ';') part

impossible :: String -> Int
impossible s = go games
  where
    (idstr, games) = break (== ':') s
    (id, _) = getCount idstr

    go :: String -> Int
    go [] = id
    go s = if over then 0 else go rest
      where
        (over, rest) = overlimit s

type Result = (Int, Int, Int)

getMax :: Result -> String -> (Result, String)
getMax max [] = (max, [])
getMax (r, g, b) s = case color of
  "red" -> ((max count r, g, b), rest)
  "green" -> ((r, max count g, b), rest)
  "blue" -> ((r, g, max count b), rest)
  where
    (count, part) = getCount s
    (color, rest) = break (\c -> c == ',' || c == ';') part

power :: String -> Int
power s = go (0, 0, 0) games
  where
    (_, games) = break (== ':') s
    go :: Result -> String -> Int
    go (r, g, b) [] = r * g * b
    go res s = go res' rest
      where
        (res', rest) = getMax res s

main = do
  input <- readFile "input/day2.txt"
  -- part I
  print . show $ sum $ map impossible (lineSplit input)
  -- part II
  print . show $ sum $ map power (lineSplit input)
