import System.IO
import Data.List
import Data.Char

main = do
    contents <- readFile "input.txt"
    -- putStr $ unlines (map (intersperse ' ') ([(" " ++ ['1'..'9'])] ++ (attachHead (pred 'A') (lines contents))))
    putStrLn $ unlines $ map (intersperse ' ') $ map (\(x, y) -> x:y) $ zip ['A'..] $ lines contents

    let row = 4
    let column = 0
    let input = '4'
    let board = lines contents

    print $ [foldl (\acc x -> acc ++ x) [] (map ((take 3)) (take 3 board))]
    print $ drop 1 ["1"]
    --print $ maek3X3Board board
    print $ (row `div` 3)
    -- can row 
    print $ input `elem` (filter (/='_') ((lines contents) !! row))
    -- can column
    print $ input `elem` ((filter (/='_')) ((transpose $ lines contents) !! row))

-- attachHead :: Char -> [[Char]] -> [[Char]]
-- attachHead _ [] = []
-- attach/Head prefix (x:xs) = [succ prefix : x] ++ (attachHead (succ prefix) xs)

maek3X3Board :: [[Char]] -> [[Char]]
maek3X3Board [] = []
maek3X3Board [[]] = [[]]
maek3X3Board board = make3x3Piece $ (take 3 board) ++ (maek3X3Board $ (drop 3 board))

make3x3Piece :: [[Char]] -> [[Char]]
make3x3Piece [] = []
make3x3Piece [[]] = [[]]
make3x3Piece piece = [foldl (\acc x -> acc ++ x) [] (map ((take 3)) (take 3 piece))] ++ (make3x3Piece $ map (drop 3) piece)