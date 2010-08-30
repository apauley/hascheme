module Main where
import System.Environment
import Control.Monad
import IO hiding (try)
import EvalApply
import LispData

main :: IO ()
main =
  do args <- getArgs
     case length args of
       0 -> runRepl
       1 -> runOne $ args !! 0
       otherwise ->
         putStrLn "Run the program with 0 args for a repl or 1 lisp expression"

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl =
  do
    putStrLn "Welcome to hacheme!"
    putStrLn "It is simply a simple Scheme interpreter written in Haskell."
    putStrLn "This is a toy project done for my own amusement, don't try to"
    putStrLn "use it for anything that matters."
    putStrLn "\nHappy Hacking!\n"
    putStrLn "Type \":q\" to exit the interpreter."
    nullEnv >>= until_ (== ":q") (readPrompt "hascheme>>> ") . evalAndPrint

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a->Bool) -> m a -> (a->m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action
