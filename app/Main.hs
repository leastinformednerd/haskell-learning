module Main where
import SexprParser (parseSexpr)
import System.Environment (getArgs)

processFile :: String -> IO ()
processFile path = do
    contents <- readFile path
    putStrLn $ show $ parseSexpr contents

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [] -> putStrLn "Please provide a path"
        x -> processFile $ head x
