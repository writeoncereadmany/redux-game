module Examples.Pong.Util.Random where

import System.Random
import Data.List

-- Takes a sorted list of floats in the range [0-1], and returns the
-- two values with the largest distance between them
largestGap :: [Float] -> (Float, Float)
largestGap [] = (0, 1)
largestGap (x:xs) = largestGap (0, x, x, x) (xs ++ [1]) where
  largestGap (min, max, _, _) [] = (min, max)
  largestGap (min, max, max_gap, last) (x:xs) = if max_gap > x - last
    then largestGap (min, max, max_gap, x) xs
    else largestGap (last, x, x - last, x) xs

-- Generates n numbers in the range 0-1, in such a way as to
-- make large gaps less likely. It does this by always making sure the
-- next random number is placed randomly in the largest gap in the sequence
evenIsh :: Int -> IO [Float]
evenIsh n = evenIsh n [] where
  evenIsh 0 acc = return acc
  evenIsh n xs = do
    let (min, max) = largestGap xs
    next <- randomIO
    evenIsh (n-1) (sort (next:xs))
