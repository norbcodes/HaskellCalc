module Main where

import System.Exit (exitSuccess)
import Data.Fixed (mod')
import Text.Regex.TDFA ((=~))

data TokType = Literal | BinOp | OpenParen | CloseParen | EOF | Error deriving Show
data Token = Token TokType (Either Double String) deriving Show

getInput :: IO String
getInput = do
    getLine

strToDouble :: String -> Either Double String
strToDouble s = Left (read s :: Double)

makeToken :: String -> Token
makeToken str
    | str =~ "\\-?[0-9]+(\\.[0-9]+)?" :: Bool = Token Literal (strToDouble str)
    | str == "+" = Token BinOp (Right "+")
    | str == "-" = Token BinOp (Right "-")
    | str == "*" = Token BinOp (Right "*")
    | str == "/" = Token BinOp (Right "/")
    | str == "%" = Token BinOp (Right "%")
    | str == "**" = Token BinOp (Right "**")
    | str == "(" = Token OpenParen (Right "(")
    | str == ")" = Token CloseParen (Right ")")
    | str == "==" = Token BinOp (Right "==")
    | str == "!=" = Token BinOp (Right "!=")
    | str == ">=" = Token BinOp (Right ">=")
    | str == "<=" = Token BinOp (Right "<=")
    | str == ">" = Token BinOp (Right ">")
    | str == "<" = Token BinOp (Right "<")
    | str == " " = Token Literal (Right "space")
    | otherwise = Token EOF (Right "EOF")

globalPattern :: String
globalPattern = "\\-?[0-9]+(\\.[0-9]+)?|(\\(|\\))| |(\\*\\*|\\+|\\-|/|%|\\*|>|<|>=|<=|==|!=)"

regexLine :: String -> [[String]]
regexLine s = s =~ globalPattern

convertLine :: [String] -> String
convertLine [] = ""
convertLine l = head l

tokenize :: String -> [Token]
tokenize line = map (makeToken . convertLine) (regexLine line)

getParenBonus :: Token -> Int -> Int
getParenBonus (Token OpenParen (Right "(")) p = p + 10
getParenBonus (Token CloseParen (Right ")")) p = p - 10
getParenBonus _ p = p

-- Operator precedence inspired by C++
-- https://en.cppreference.com/w/cpp/language/operator_precedence.html
getTokenPrec :: Token -> Int -> Int
getTokenPrec (Token Literal (Right "space")) _ = -3
getTokenPrec (Token Literal _) _ = -1
getTokenPrec (Token BinOp (Right "==")) parenBonus = 1 + parenBonus
getTokenPrec (Token BinOp (Right "!=")) parenBonus = 1 + parenBonus
getTokenPrec (Token BinOp (Right ">")) parenBonus = 2 + parenBonus
getTokenPrec (Token BinOp (Right "<")) parenBonus = 2 + parenBonus
getTokenPrec (Token BinOp (Right ">=")) parenBonus = 2 + parenBonus
getTokenPrec (Token BinOp (Right "<=")) parenBonus = 2 + parenBonus
getTokenPrec (Token BinOp (Right "+")) parenBonus = 3 + parenBonus
getTokenPrec (Token BinOp (Right "-")) parenBonus = 3 + parenBonus
getTokenPrec (Token BinOp (Right "*")) parenBonus = 4 + parenBonus
getTokenPrec (Token BinOp (Right "/")) parenBonus = 4 + parenBonus
getTokenPrec (Token BinOp (Right "%")) parenBonus = 4 + parenBonus
getTokenPrec (Token BinOp (Right "**")) parenBonus = 5 + parenBonus
getTokenPrec (Token OpenParen (Right "(")) _ = -3
getTokenPrec (Token CloseParen (Right ")")) _ = -3
getTokenPrec _ _ = -3

calcPrec :: [Token] -> [Int] -> Int -> [Int]
calcPrec [] p _ = p
calcPrec tokens precList parenBonus =
    calcPrec    (tail tokens)
                (precList <> [getTokenPrec (head tokens) parenBonus])
                (getParenBonus (head tokens) parenBonus)

findInList :: [Int] -> Int -> Int -> Int
findInList [] _ _ = -1
findInList list what indx
    | head list == what = indx
    | otherwise = findInList (tail list) what (indx + 1)

getIndxOfMaxPrec :: [Int] -> Int
getIndxOfMaxPrec precList = findInList precList (maximum precList) 0

makeDouble :: Bool -> Double
makeDouble True = 1.0
makeDouble False = 0.0

collapseBinOp :: [Token] -> [Token]
collapseBinOp [Token BinOp (Right _),Token BinOp (Right _),Token Literal (Left _)] = [Token Error (Right "Parse Error: 2 BinOps next to each other")]
collapseBinOp [Token Literal (Left _),Token BinOp (Right _),Token BinOp (Right _)] = [Token Error (Right "Parse Error: 2 BinOps next to each other")]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "+"),Token Literal (Left r)] = [Token Literal (Left (l + r))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "-"),Token Literal (Left r)] = [Token Literal (Left (l - r))]
collapseBinOp [Token BinOp (Right "-"),Token Literal (Left r)] = [Token Literal (Left (negate r))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "*"),Token Literal (Left r)] = [Token Literal (Left (l * r))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "/"),Token Literal (Left r)] = [Token Literal (Left (l / r))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "%"),Token Literal (Left r)] = [Token Literal (Left (mod' l r))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "**"),Token Literal (Left r)] = [Token Literal (Left (l ** r))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "=="),Token Literal (Left r)] = [Token Literal (Left (makeDouble (l == r)))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "!="),Token Literal (Left r)] = [Token Literal (Left (makeDouble (l /= r)))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right ">="),Token Literal (Left r)] = [Token Literal (Left (makeDouble (l >= r)))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "<="),Token Literal (Left r)] = [Token Literal (Left (makeDouble (l <= r)))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right ">"),Token Literal (Left r)] = [Token Literal (Left (makeDouble (l > r)))]
collapseBinOp [Token Literal (Left l),Token BinOp (Right "<"),Token Literal (Left r)] = [Token Literal (Left (makeDouble (l < r)))]
collapseBinOp _ = []

gLeft :: [Token] -> [Int] -> [Token]
gLeft tokens precList = take (getIndxOfMaxPrec precList - 1) tokens

gRight :: [Token] -> [Int] -> [Token]
gRight tokens precList = drop (getIndxOfMaxPrec precList + 2) tokens

gMiddle :: [Token] -> [Int] -> [Token]
gMiddle tokens precList = drop (getIndxOfMaxPrec precList - 1) (take (getIndxOfMaxPrec precList + 2) tokens)

gLeft1 :: [Int] -> [Int]
gLeft1 precList = take (getIndxOfMaxPrec precList - 1) precList

gRight1 :: [Int] -> [Int]
gRight1 precList = drop (getIndxOfMaxPrec precList + 2) precList

precPass :: [Int] -> Int -> Int
precPass precs n
    | n == maximum precs = -1
    | otherwise = n

collapseBinOps :: [Token] -> [Int] -> [Token]
collapseBinOps tokens precs
    | maximum precs == -1 = tokens
    | otherwise = collapseBinOps    (gLeft tokens precs <> collapseBinOp (gMiddle tokens precs) <> gRight tokens precs)
                                    (gLeft1 precs <> [-1] <> gRight1 precs)

tokenFilter :: Token -> Bool
tokenFilter (Token OpenParen _) = False
tokenFilter (Token CloseParen _) = False
tokenFilter (Token Literal (Right "space")) = False
tokenFilter _ = True

printResult :: Token -> IO ()
printResult (Token Literal (Left v)) = putStrLn ("... " ++ show v :: String)
printResult _ = print "Something went wrong..."

isErrorToken :: Token -> Bool
isErrorToken (Token Error _) = True
isErrorToken (Token _ _) = False

getErrorMsg :: Token -> String
getErrorMsg (Token Error (Right s)) = s
getErrorMsg (Token _ _) = ""

errorCheck :: [Token] -> (Bool, String)
errorCheck [] = (False, "")
errorCheck t
    | isErrorToken (head t) = (True, getErrorMsg (head t))
    | otherwise = errorCheck (tail t)

mainLoop :: String -> IO Int
mainLoop "q" = do
    putStrLn "Goodbye!"
    exitSuccess

mainLoop str = do
    let tokens = tokenize str
    let precedenceList = calcPrec tokens [] 0
    let precedenceListCleaned = filter (/= -3) precedenceList
    let tokensCleaned = filter tokenFilter tokens
    let result = collapseBinOps tokensCleaned precedenceListCleaned
    let hasErrored = errorCheck result

    if fst hasErrored then do
        putStrLn (snd hasErrored)
    else do
        printResult (head result)

    input <- getInput
    mainLoop input

main :: IO Int
main = do
    putStrLn "Hello! Use +,-,/,*,%,** to calculate stuff\nUse ==,!=,>=,<=,>,< to compare stuff\nUse 'q' to leave\n"
    input <- getInput
    mainLoop input