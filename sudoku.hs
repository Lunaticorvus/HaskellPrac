import System.IO

main = do
    contents <- readFile "input.txt"
    putStr $ show $ makeBoard $ filter (/= '\n') contents

makeBoard :: String -> [[String]]
makeBoard [] = []
makeBoard plain = [take 9 plain]:(makeBoard $ drop 9 plain)