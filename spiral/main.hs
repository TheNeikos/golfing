--
--       1  2  3 4
--
--   1   1  2  3 4
--   2  12 13 14 5
--   3  11 16 15 6
--   4  10  9  8 7
--
--
--
--       1 2 3
--
--   1   1 2 3
--   2   8 9 4
--   3   7 6 5
--
--       1  2  3
--
--   1   1  2  3
--   2  14 15  4
--   3  13 16  5
--   4  12 17  6
--   5  11 18  7
--   6  10  9  8
--
--
--     x1  2  3  4  5
--   y
--   1  1  2  3  4  5
--   2 16 17 18 19  6
--   3 15 24 25 20  7
--   4 14 23 22 21  8
--   5 13 12 11 10  9
--
--
--     1  2 3
--    11 12 4
--    10 13 5
--     9 14 6
--     8 15 7
--    10  9 8
--
--
--

-- makeBox p w h x y
--     | x >= y = p + x-1 + y-1
--     | x  < y = (makeBox (h+w-1) w h (w-x+1) (h-y+1))
--
-- getPointInSpiral w h x y = makeBox


-- makeBox w h x y
--     | y == 1 = x
--     | x == w = x+y-1
--     | y == h = 2*w+h-1-x
--     | x == 1 = 2*w+h-2+h-y
--     | otherwise = 2*w+h-4+h + makeBox (w-2) (h-2) (x-1) (y-1)

import Data.List
import Data.List.Split
import System.Environment

makeBox w h x y
    | y == 1 = x
    | x == w = x+y-1
    | y == h = 2*w+h-1-x
    | x == 1 = 2*w+2*h-y-2
    | otherwise = 2*w+h-4+h + makeBox (w-2) (h-2) (x-1) (y-1)

main = do
    args <- getArgs
    let width = read $ head args
    let height = read $ last args
    putStrLn $ unlines (map unwords $ stuff width height)
    where
        stuff w h = chunksOf w $ [p | y <- [1..h], x <- [1..w], let p = show (makeBox w h x y)]

