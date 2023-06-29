{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Parser (Sexpr(..), parse)
import System.IO (hFlush, stdout)
import Eval (evalSexpr, Env)
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Control.Monad.State (runState)

repl :: Env -> IO ()
repl env = do
    putStr ">> "
    hFlush stdout
    input <- TIO.getLine
    if input == "exit" then
        putStrLn "Bye!"
    else do
        let (result, env') = flip runState env . evalSexpr . parse $ input
        print result
        repl env'

main :: IO ()
main = repl Map.empty
