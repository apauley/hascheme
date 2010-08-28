module Main where
import System.Environment
import SchemeParser

main :: IO ()
main =
  do args <- getArgs
     case length args of
       0 -> runRepl
       1 -> evalAndPrint (args !! 0)
       otherwise ->
         putStrLn "Run the program with 0 args for a repl or 1 lisp expression"

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Hascheme>>> ") evalAndPrint
