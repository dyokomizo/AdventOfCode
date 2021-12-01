module Day_01 where

increments xs = length $ filter id $ zipWith (<) xs (tail xs)

slide n [] = []
slide n xs | length xs >= n = sum (take n xs) : slide n (tail xs)
           | otherwise      = []


sample = [199,200,208,210,200,207,240,269,260,263]

puzzle :: IO [Int]
puzzle = map read <$> lines <$> readFile "Day_01_input.txt"

solution = (increments . slide 3) <$> puzzle
