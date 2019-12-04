import Data.List.Split
import System.Environment
import Data.List
import Data.Maybe
--import Data.Set

data Vec = V Int Int deriving (Show, Eq)

createVec :: (Int, Int) -> Vec
createVec (x, y) = V x y

w1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
w2 = "U62,R66,U55,R34,D71,R55,D58,R83"
a = [w1, w2]
moveDir :: Vec -- origin
        -> Vec -- direction
        -> [(Int,Int)] -- results
moveDir (V x1 y1) (V x2 0) =
  if x2 < 0
  then zip [(x * (-1)) + x1 | x <- [1..lim]]
       (Prelude.take lim $ repeat y1)
  else zip [x + x1 | x <- [1..lim]]
       (Prelude.take lim $ repeat y1)
  where lim = abs x2
moveDir (V x1 y1) (V 0 y2) =
  if y2 < 0
  then zip (Prelude.take lim $ repeat x1)
       [(x * (-1))+y1 | x <- [1..lim]]
  else zip (Prelude.take lim $ repeat x1)
       [x+y1 | x <- [1..lim]]
  where lim = abs y2

md :: Vec -> [Vec] -> [(Int, Int)]
md _ [] = []
md v (x : xs) = y ++ md (createVec $ y !! (length y - 1)) xs
  where y = moveDir v x 

parseDir :: String -> Vec
parseDir ('L' : xs) = V ((-1) * (read xs)) 0 
parseDir ('R' : xs) = V (read xs) 0
parseDir ('U' : xs) = V 0 (read xs)
parseDir ('D' : xs) = V 0 ((-1) * (read xs))
parseDir _ = undefined

pd :: [String] -> [Vec]
pd [] = []
pd (x : xs) = [parseDir x] ++ pd xs

findCom :: [(Int, Int)] -> [(Int, Int)] -> Int -> [Int]
findCom [] _ _ = []
findCom (x:xs) ys p = if (t > 0)
  then [(p+1) + (t+1)]   ++ findCom xs ys (p+1)
  else findCom xs ys (p+1)
       where t = fromMaybe 0 (elemIndex x ys)

fc :: [[(Int, Int)]] -> [Int]
fc (x : xs) = findCom x (head xs) 0 

{-
manDis :: Set (Int, Int) -> Set Int
manDis xs
  | Data.Set.null xs = fromList []
  | otherwise = if x == 0 && y == 0
    then manDis $ Data.Set.drop 1 xs
    else insert (abs x + abs y) (manDis $ Data.Set.drop 1 xs)
  where x = fst $ elemAt 0 xs
        y = snd $ elemAt 0 xs
-}
wireDis :: [(Int,Int)] -> Int
wireDis a = undefined

main :: IO()
main = do
  args <- getArgs
  content <- readFile (args !! 0 )
  let lof = lines content
  print $
    minimum $
    fc $
    md (V 0 0) <$>
    pd <$>
    splitOn "," <$> lof
