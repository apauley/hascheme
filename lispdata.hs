module LispData where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom x) = x
showVal (List x) = "(" ++ unwordsList x ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Number x) = show x
showVal (String x) = show x
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

