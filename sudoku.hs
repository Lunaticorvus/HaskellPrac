import System.IO
import Data.List
import Data.Char
import Control.Applicative
import Data.List.Split
import Data.Maybe

main = do
    contents <- readFile "input.txt"

    let board = lines contents

    prettyPrint board
    prettyPrint $ fromJust $ solve (filter (\(i, j) -> (lines contents) !! i !! j == '_') $ concat $ generate (,)) (Just $ lines contents)

prettyPrint b = putStrLn $ unlines $ map (intersperse ' ') $ (' ':['1'..'9']):(map (\(x, y) -> x:y) $ zip ['A'..] $ b)

doInput :: Int -> Int -> Char -> [[Char]] -> [[Char]]
doInput x y c b = generate (\i j -> if i == x && j == y then c else b !! i !! j)
-- doInput x y c b = case b !! x !! y of
    -- '_' -> (take x b) ++ [(take y (b !! x)) ++ [c] ++ (drop (y + 1) (b !! x))] ++ (drop (x + 1) b)
    -- otherwise -> b

solve :: [(Int, Int)] -> Maybe [[Char]] -> Maybe [[Char]]
solve [] b = b
solve _ Nothing = Nothing
solve (x:xs) (Just b) = listToMaybe $ catMaybes $ map (solve xs) $ map Just $ filter valid $ map (\c -> doInput (fst x) (snd x) c b) ['1'..'9']

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