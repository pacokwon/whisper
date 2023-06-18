{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Parser (Sexpr(..), parse)
import System.IO (hFlush, stdout)
import Eval (eval)
import qualified Data.Text.IO as TIO

repl :: IO ()
repl = do
    putStr ">> "
    hFlush stdout
    input <- TIO.getLine
    if input == "exit" then
        putStrLn "Bye!"
    else do
        print . eval . parse $ input
        repl

main :: IO ()
main = repl
