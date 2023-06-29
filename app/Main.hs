{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Parser (Sexpr(..), parse)
import System.IO (hFlush, stdout)
import Eval (evalSexpr, Env)
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Control.Monad.State (runStateT)

repl :: Env -> IO ()
repl env = do
    putStr ">> "
    hFlush stdout
    input <- TIO.getLine
    if input == "exit" then
        putStrLn "Bye!"
    else
        case flip runStateT env . evalSexpr . parse $ input of
            Left err -> print ("Error: " ++ err) >> repl env
            Right ((result, env')) -> print result >> repl env'

main :: IO ()
main = repl Map.empty
