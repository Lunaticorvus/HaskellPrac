import System.IO
import Data.List

main = do
    contents <- readFile "input.txt"
    -- putStr $ unlines (map (intersperse ' ') ([(" " ++ ['1'..'9'])] ++ (attachHead (pred 'A') (lines contents))))
    putStrLn $ unlines $ map (intersperse ' ') $ map (\(x, y) -> x:y) $ zip ['A'..] $ lines contents

-- attachHead :: Char -> [[Char]] -> [[Char]]
-- attachHead _ [] = []
-- attach/Head prefix (x:xs) = [succ prefix : x] ++ (attachHead (succ prefix) xs)