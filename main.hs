module Main where
import System.Environment
import Control.Monad
import SchemeParser

main :: IO ()
main =
  do args <- getArgs
     case length args of
       0 -> runRepl
       1 -> evalAndPrint (args !! 0)
       otherwise ->
         putStrLn "Run the program with 0 args for a repl or 1 lisp expression"

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Hascheme>>> ") evalAndPrint

until_ :: Monad m => (a->Bool) -> m a -> (a->m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action