import System.IO
import Data.List
import Data.Char

main = do
    contents <- readFile "input.txt"
    -- putStr $ unlines (map (intersperse ' ') ([(" " ++ ['1'..'9'])] ++ (attachHead (pred 'A') (lines contents))))
    putStrLn $ unlines $ map (intersperse ' ') $ map (\(x, y) -> x:y) $ zip ['A'..] $ lines contents

    let row = 0
    let column = 0
    let input = '7'
    let board = lines contents

    print $ (map (take 3) (take 3 (map (drop (row - (row `mod` 3))) (drop (column - (column `mod` 3)) (lines contents)))))

    -- can row 
    print $ input `elem` (filter (/='_') ((lines contents) !! row))
    -- can column
    print $ input `elem` ((filter (/='_')) ((transpose $ lines contents) !! row))
    -- can 3by3
    print $ input `elem` (filter (/='_') (foldl (\acc x -> acc ++ x) [] (map (take 3) (take 3 (map (drop (row - (row `mod` 3))) (drop (column - (column `mod` 3)) (lines contents)))))))

-- attachHead :: Char -> [[Char]] -> [[Char]]
-- attachHead _ [] = []
-- attach/Head prefix (x:xs) = [succ prefix : x] ++ (attachHead (succ prefix) xs)