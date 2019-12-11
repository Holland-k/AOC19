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
type Input = Int

parse :: [String] -> [Int]
parse [] = []
parse (x:y:[]) =
  if x == "-"
  then [(-1) * read y] 
  else [read x] ++ [read y]
parse (x:y:xs) =
  if x == "-"
  then [(-1) * read y] ++ parse xs
  else [read x] ++ parse (y:xs)

readAddr :: Int -> Addr -> Mem -> Int
readAddr 0 x xs = S.index xs $ S.index xs x -- position mode
readAddr 1 x xs = S.index xs x -- immediate mode
readAddr _ _ _ = 99

writeAddr :: Addr -> Int -> Mem -> Mem
writeAddr i v m = update i v m
--writeAddr 1 i v m = update 

parseOp :: Int -> -- Original Opcode
  (Int, Int, Int, Int) -- (Opcode, PM1, PM2, PM3)
  -- 2 => 0 0 0 021
  -- 11102
parseOp x
  -- 1, 3, 4, 99
  | x < 100 = (x `mod` 100, 0, 0, 0)
  -- 199 
  | x < 1000 = (x `mod` 100, 1, 0, 0)
  -- 1002
  | x < 10000 = (x `mod` 100, (x `div` 100) `mod` 10, 1, 0)
  -- 10102
  | x < 100000 = (x `mod` 100, (x `div` 100) `mod` 10,
                  (x `div` 1000) `mod` 10, 1)
  | otherwise = (0,0,0,0)

getSnd :: (Addr, Input, Mem) -> Input
getSnd (_, x, _) = x

runComp :: State (Addr, Input, Mem) [Input]
runComp = fmap toList <$> untilJust $ do
  (ind, inp, m) <- get
  let (output, p1, p2, p3) = parseOp $ S.index m ind
  case output of
    1 -> put (ind + 4, -- add
              inp,
              writeAddr
               (S.index m $ ind+3)      -- index
               ((readAddr p1 (ind + 1) m) +
                (readAddr p2 (ind + 2) m))
               m) -- memory
         >> pure Nothing
    2 -> put (ind+4, -- multiply
              inp,
              writeAddr
               (S.index m $ ind+3)
               ((readAddr p1 (ind + 1) m) *
                 (readAddr p2 (ind + 2) m))
               m)     
         >> pure Nothing
    3 -> put (ind+2, -- save
              inp,
              writeAddr
               (S.index m $ ind+1)
               inp
               m)
         >> pure Nothing
    4 -> put (ind+2, -- read
              S.index m $ S.index m $ ind+1,
              m)
         >> pure Nothing
    5 -> if((readAddr p1 (ind + 1) m) /= 0)
         then put (readAddr p2 (ind+2) m, inp, writeAddr ind inp m)
              >> pure Nothing
         else put (ind + 3, inp, writeAddr ind inp m) >> pure Nothing
    6 -> if((readAddr p1 (ind + 1) m) == 0)
         then put (readAddr p2 (ind + 2) m, inp, writeAddr ind inp m)
              >> pure Nothing
         else put (ind + 3, inp, writeAddr ind inp m) >> pure Nothing
         >> pure Nothing
    7 -> put (ind+4,
              inp,
              if ((readAddr p1 (ind + 1) m) < (readAddr p2 (ind + 2) m))
               then writeAddr (readAddr 1 (ind + 3) m) 1 m
              else writeAddr (readAddr 1 (ind + 3) m) 0 m)
         >> pure Nothing
    8 -> put (ind+4,
              inp,
              if ((readAddr p1 (ind + 1) m) == (readAddr p2 (ind +2) m))
              then writeAddr (readAddr 1 (ind + 3) m) 1 m
              else writeAddr (readAddr 1 (ind + 3) m) 0 m)
         >> pure Nothing
    99 -> pure $ Just m -- terminate1
    op -> put (0, inp, S.fromList [1,1,1,1]) >> pure Nothing

fixInput :: Seq Int -> Seq Int
fixInput m = update 2 2 $ update 1 12 m

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let lof = lines content
  print $
    --Prelude.take 1 . -- Needed for Day 2
    getSnd $  -- Needed for Day 5
    snd .  -- Needed for Day 5; use fst for Day 2
    runState runComp $
    (0,(read (args !! 1)),) .
    --fixInput $ --Needed for Day 2
    fromList $
    parse $
    splitOn "," <$>
    head $ lof
