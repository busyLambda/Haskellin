module Main where

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

-- TODO: Note we should create individual Enumurations for the operators rather than using only one.
data Span = Span
    { start :: Int
    , end :: Int
    }

data Token = Operator String
           | Identifier String
           | Number Int
           | OpenParen
           | CloseParen
           | Comma
           | Aestrisk
           | EndOfInput
           | InvalidToken String
           deriving (Eq, Show)

nextToken :: String -> [Token]
nextToken [] = [EndOfInput]
nextToken (c:cs) | isSpace c = nextToken cs
nextToken (c:cs) | isDigit c = let (digits, rest) = span isDigit (c:cs)
                           in Number (read digits) : nextToken rest
nextToken (c:cs) | isAlpha c = let (chars, rest) = span isAlphaNum (c:cs)
                           in Identifier chars : nextToken rest
nextToken ('(':cs) = OpenParen : nextToken cs
nextToken (')':cs) = CloseParen : nextToken cs
nextToken (',':cs) = Comma : nextToken cs
nextToken (c:d:cs) | [c, d] `elem` ["==", "!=", ">=", "<=", "--", "++", "=>", "->", "<-"] =
    Operator [c, d] : nextToken cs
nextToken (c:cs)   = Operator [c] : nextToken cs

main :: IO ()
main = do
    let input = "2 * 2 => <- -> ++ -- |"
    print $ nextToken input