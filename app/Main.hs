{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Parser (parse, parseProgram)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Eval (evalSexpr, Env, globalEnvironment)
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Control.Monad.State (runStateT, evalStateT)

repl :: IO ()
repl = repl' globalEnvironment

repl' :: Env -> IO ()
repl' env = do
    putStr ">> "
    hFlush stdout
    input <- TIO.getLine
    if input == "exit" then
        putStrLn "Bye!"
    else
        case flip runStateT env . evalSexpr . parse $ input of
            Left err -> putStrLn ("Error: " ++ err) >> repl' env
            Right (result, env') -> print result >> repl' env'

runFile :: String -> IO ()
runFile filename = do
    input <- TIO.readFile filename
    case flip evalStateT globalEnvironment . mapM evalSexpr . parseProgram $ input of
        Left err -> putStrLn $ "Error: " ++ err
        Right result -> mapM_ print result

main :: IO ()
main = do
    args <- getArgs
    if null args then
        repl
    else do
        let filename = head args
        exists <- doesFileExist filename
        if exists then
            runFile filename
        else
            putStrLn $ "File named \"" ++ filename ++ "\" does not exist!"
