-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


import System.Environment
import Control.Monad.State
import Control.Monad.Loops (untilJust)
import Data.Sequence as S
import Data.List.Split
import Data.Foldable as F

type Addr = Int
type Mem = Seq Int

parse :: [String] -> [Int]
parse [] = []
parse (x:xs) = [read x] ++ parse xs

readAddr :: Addr -> Mem -> Int
readAddr x xs = S.index xs x

writeAddr :: Addr -> Int -> Mem -> Mem
writeAddr i v m = update i v m

runComp :: State (Addr, Mem) [Int]
runComp = fmap F.toList <$> untilJust $ do
  (i, m) <- get
  case (S.index m i) of
    1 -> put (i + 4, (writeAddr (S.index m (i+3))
              ((S.index m (S.index m (i+1))) +
              (S.index m (S.index m (i+2)))) m))
         >> pure Nothing
    2 -> put (i+4, (writeAddr (S.index m (i+3))
              ((S.index m (S.index m (i+1))) *
              (S.index m (S.index m (i+2)))) m))
         >> pure Nothing
    99 -> pure $ Just m
    op -> put (0, S.fromList [1,1,1,1]) >> pure Nothing

fixInput :: Seq Int -> Seq Int
fixInput m = update 2 2 $ update 1 12 m

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let lof = lines content
  print $
    Prelude.take 1 .
    fst .
    runState runComp $
    (0,) .
    fixInput $ 
    fromList $
    parse $
    splitOn "," <$>
    head $ lof
