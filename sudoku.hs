import System.IO
import Data.List
import Data.Char
import Control.Applicative
import Data.List.Split


main = do
    contents <- readFile "input.txt"
    -- putStr $ unlines (map (intersperse ' ') ([(" " ++ ['1'..'9'])] ++ (attachHead (pred 'A') (lines contents))))
    putStrLn $ unlines $ map (intersperse ' ') $ map (\(x, y) -> x:y) $ zip ['A'..] $ lines contents 

    print $ valid $ (lines contents)

    putStrLn $ unlines $ map (intersperse ' ') $ map (\(x, y) -> x:y) $ zip ['A'..] $ (doInput 1 1 '1' (lines contents))

doInput :: Int -> Int -> Char -> [[Char]] -> [[Char]]
doInput x y c b = (take x b) ++ [(take y (b !! x)) ++ [c] ++ (drop (y + 1) (b !! x))] ++ (drop (x + 1) b)

generate f = chunksOf 9 $ liftA2 f [0..8] [0..8]
validChunk d c = (\x -> (length x) == (length $ nub x) ) $ filter (/= '_') $ map (\(i, j) -> d !! i !! j) c
validChunks d cs = foldl1 (&&) $ map (validChunk d) cs
valid1 d = validChunks d $ generate (,)  
valid2 d = validChunks d $ generate (flip (,))
valid3 d = validChunks d $ generate (\i j -> ((i `div` 3) * 3 + (j `div` 3), (i `mod` 3) * 3 + (j `mod` 3)))
valid d = valid1 d && valid2 d && valid3 d

    -- can row 
    --print $ input `elem` (filter (/='_') ((lines contents) !! row))
    -- can column
    --print $ input `elem` ((filter (/='_')) ((transpose $ lines contents) !! row))
    -- can 3by3
    --print $ input `elem` (filter (/='_') (foldl (\acc x -> acc ++ x) [] (map (take 3) (take 3 (map (drop (row - (row `mod` 3))) (drop (column - (column `mod` 3)) (lines contents)))))))
-- attachHead :: Char -> [[Char]] -> [[Char]]
-- attachHead _ [] = []
-- attach/Head prefix (x:xs) = [succ prefix : x] ++ (attachHead (succ prefix) xs)