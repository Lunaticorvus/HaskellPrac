import System.IO
import Data.List

main = do
    contents <- readFile "input.txt"
    putStr $ unlines (map (intersperse ' ') ([(" " ++ ['1'..'9'])] ++ (attachHead (pred 'A') (lines contents))))
    
attachHead :: Char -> [[Char]] -> [[Char]]
attachHead _ [] = []
attachHead prefix (x:xs) = [succ prefix : x] ++ (attachHead (succ prefix) xs)