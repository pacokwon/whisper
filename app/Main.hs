module Main (main) where

import Parser (Sexpr(..))

main :: IO ()
main = do
    print $ Number 3 -- Output: Number 3
    print $ Ident "foo" -- Output: Ident "foo"
    print $ Paren [Ident "+", Number 1, Number 4] -- Output: Paren [Ident "+",Number 1,Number 4]

