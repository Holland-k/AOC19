import System.Environment

fuel :: String -> Integer
fuel mass = floor (fromIntegral $ a `div` 3) - 2
  where a = read mass :: Integer

addFuel :: Integer -> Integer
addFuel cf = if cf > 0
  then cf + addFuel (fuel scf)
  else 0
  where scf = show cf
  
main :: IO()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let lof = lines content
  print $ sum $ addFuel . fuel <$> lof

  
