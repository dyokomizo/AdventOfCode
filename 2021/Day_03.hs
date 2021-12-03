module Day_03 where

import Data.List as L

powerConsumption xs = (dec $ map fst bs) * (dec $ map snd bs)
  where bs = map split $ bits xs

dec :: String -> Integer
dec = foldl (\d b -> (if b == '0' then 0 else 1) + 2 * d) 0

split :: Ord a => [a] -> (a,a)
split = splitWith pick
  where pick [(_,e),(_,g)] = (g,e) 

splitWith pick = pick . sort . map (\b -> (length b, head b)) . L.group . sort

bits ([]:_) = []
bits  xs    = map head xs : bits (map tail xs)

consider bc = loop 0
  where loop n [x] = x
        loop n  xs = loop (n + 1) $ filter (bc n $ bits xs) xs

oxygen n bs x = x !! n == keep (bs !! n)
  where keep = splitWith pick
        pick [(n,e),(m,g)] | n == m    = '1'
                           | otherwise = g 

co2 n bs x = x !! n == keep (bs !! n)
  where keep = splitWith pick
        pick [(n,e),(m,g)] | n == m    = '0'
                           | otherwise = e 

lifeSupport xs = dec (consider oxygen xs) * dec (consider co2 xs)


sample = ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]

puzzle :: IO [String]
puzzle = lines <$> readFile "Day_03_input.txt"
