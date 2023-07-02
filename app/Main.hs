{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Parser (parse, parseProgram)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Eval (evalSexpr, Env, globalEnvironment)
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Control.Monad.State (runStateT)
import Control.Monad (void)
import Data.Foldable (forM_)

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

runFile :: String -> IO (Maybe Env)
runFile filename = do
    input <- TIO.readFile filename
    case flip runStateT globalEnvironment . mapM evalSexpr . parseProgram $ input of
        Left err -> do
            putStrLn $ "Error: " ++ err
            return Nothing
        Right (values, env) -> do
            mapM_ print values
            return . Just $ env

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [filename] -> do
            exists <- doesFileExist filename
            if exists then void (runFile filename)
            else
                putStrLn $ "File named \"" ++ filename ++ "\" does not exist!"
        ["-i", filename] -> do
            exists <- doesFileExist filename
            if exists then do
                menv <- runFile filename
                forM_ menv repl'
            else
                putStrLn $ "File named \"" ++ filename ++ "\" does not exist!"
        [_, _] -> putStrLn "Invalid Flag. The only available flag is -i!"
        _ -> putStrLn "Too many arguments!"
