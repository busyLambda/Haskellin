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
           | InvalidChar Char
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer [] = [EndOfInput]
lexer (c:cs) | isSpace c = lexer cs
lexer (c:cs) | isDigit c = let (digits, rest) = span isDigit (c:cs)
                           in Number (read digits) : lexer rest
lexer (c:cs) | isAlpha c = let (chars, rest) = span isAlphaNum (c:cs)
                           in Identifier chars : lexer rest
lexer ('(':cs) = OpenParen : lexer cs
lexer (')':cs) = CloseParen : lexer cs
lexer (',':cs) = Comma : lexer cs
lexer (c:d:cs) | [c, d] `elem` ["==", "/=", ">=", "<=", "->"] =
    Operator [c, d] : lexer cs
lexer (c:cs)   = Operator [c] : lexer cs

main :: IO ()
main = do
    let input = "2 * 2"
    print $ lexer input