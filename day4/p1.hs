import Data.Char

findDup :: [Char] -> Bool
findDup (x:y:[]) = if x == y
  then True
  else False
findDup (x:y:xs) = if x == y
  then True
  else findDup ([y] ++ xs)

findAsc2 :: [Char] ->Int
findAsc2 (x:y:[]) = if ((digitToInt x) <= (digitToInt y))
  then (digitToInt x)
  else (-1)

findAsc :: [Char] -> Bool
findAsc (x:y:[]) = if ((digitToInt x) <= (digitToInt y))
  then True
  else False
findAsc (x:y:xs) = if ((digitToInt x) <= (digitToInt y))
  then findAsc ([y] ++ xs)
  else False

countC :: [Char] -> [String] -> Integer
countC x = toInteger . length . filter (x ==)

findEAsc :: [String] -> [String] -> [Integer]
findEAsc [] _ = []
findEAsc (x:xs) ys = [countC x ys] ++ findEAsc xs ys

findE :: [String] -> [Integer]
findE x = findEAsc x x 

main = do
  print $ length . filter (elem 2) . findE <$> filter findAsc $ filter findDup $ show <$> x
  where x = [138241..674034]
      
