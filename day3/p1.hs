--import Linear.V2
import Data.List.Split
import System.Environment
import Data.Set

data Vec = V Int Int deriving (Show, Eq)

createVec :: (Int, Int) -> Vec
createVec (x, y) = V x y

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
{-
findCom :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
findCom empty _ = empty
findCom xs ys = if (member x ys)
  then [x] ++ findCom xs ys
  else findCom xs ys
-}
fc :: [[(Int, Int)]] -> Set (Int, Int)
fc (x : xs) = intersection (fromList x) (fromList $ head xs)

manDis :: Set (Int, Int) -> Set Int
manDis xs
  | Data.Set.null xs = fromList []
  | otherwise = if x == 0 && y == 0
    then manDis $ Data.Set.drop 1 xs
    else insert (abs x + abs y) (manDis $ Data.Set.drop 1 xs)
  where x = fst $ elemAt 0 xs
        y = snd $ elemAt 0 xs
 
main :: IO()
main = do
  args <- getArgs
  content <- readFile (args !! 0 )
  let lof = lines content
  print $ findMin $ manDis <$> fc $ md (V 0 0) <$> pd <$> splitOn "," <$> lof
