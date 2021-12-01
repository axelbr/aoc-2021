module Lib (detectMeasurementEdge, convolve, countIncreases) where
import GHC.Natural (Natural)

detectMeasurementEdge :: [Integer] -> [[Char]]
detectMeasurementEdge (x:y:xs) = (if x < y then "increased" else "decreased") : detectMeasurementEdge (y:xs)
detectMeasurementEdge xs = []

convolve :: Int -> [Integer] -> [Integer]
convolve 0 xs = xs
convolve 1 xs = xs
convolve windowSize xs 
                    | length xs >= windowSize = sum (take windowSize xs) : convolve windowSize (tail xs)
                    | otherwise = []

countIncreases :: [[Char]] -> Int
countIncreases xs = length (filter (== "increased") xs)
