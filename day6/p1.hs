import System.Environment
import Data.List.Split
import Data.Map as M

parse :: String -> (String, String)
parse x = (head y, head $ tail y)
  where y = splitOn ")" x

findPar :: String -> -- Child
  Map String String -> -- input
  Int
findPar child inp =
  case M.lookup child inp of
    Nothing -> 1
    Just tp -> 1 + findPar tp inp

findPars :: Int -> Map String String -> Int
findPars i xs = if ((length xs) > i)
  then findPar (fst (elemAt i xs)) xs + findPars (i+1) xs
  else 0

main :: IO()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let lof = lines content
  print $
    findPars 0 $ 
    M.fromList $
    parse <$> lof
