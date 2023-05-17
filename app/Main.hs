module Main where

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

-- TODO: Note we should create individual Enumurations for the operators rather than using only one.
data Span = Span
    { start :: Int
    , end :: Int
    }

data Token = Operator String
            -- Operators
           | Add -- +
           | Ext -- -
           | Mult -- *
           | Div -- /
           | Eq -- =
           | RightThickArrow -- =->
           | RightArrow -- ->
           | LeftArrow -- <-
           | EqEq -- ==
           | NotEq -- !=
           | GtEq -- >=
           | LtEq -- =<
           | AAdd -- ++
           | EExt -- --
           | Identifier String
           | Number Int
           | OpenParen
           | CloseParen
           | Comma
           | Aestrisk
           | EndOfInput
           | UnknownToken
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
nextToken (c:d:cs) | [c, d] `elem` ["==", "!=", ">=", "<=", "--", "++", "=>", "->", "<-"] = case [c, d] of
    "==" -> EqEq : nextToken cs
    "!=" -> NotEq : nextToken cs
    ">=" -> GtEq : nextToken cs
    "<=" -> LtEq : nextToken cs
    "--" -> EExt : nextToken cs
    "++" -> AAdd : nextToken cs
    "=>" -> RightThickArrow : nextToken cs
    "->" -> RightArrow : nextToken cs
    "<-" -> LeftArrow : nextToken cs
    _ -> UnknownToken : nextToken cs

nextToken (c:cs)   = case c of
    '=' -> Eq : nextToken cs
    '+' -> Add : nextToken cs
    '-' -> Ext : nextToken cs
    '/' -> Div : nextToken cs
    '*' -> Mult : nextToken cs
    _ -> UnknownToken : nextToken cs

main :: IO ()
main = do
    let input = "== != >= <= -- ++ => -> <-"
    print $ nextToken input