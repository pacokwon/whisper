module Eval (evalSexpr, Env) where

import Control.Monad.State (State, get, gets, modify, put)
import Data.Map (Map)
-- import qualified Data.Map as Map
import qualified Data.Map as Map
import Parser (Sexpr (..))

data Value
  = NumV Int
  | Func ([Value] -> Value)
  | Unit
  | Error String

type Env = Map String Value

instance Show Value where
  show (NumV n) = show n
  show (Func _) = "<function>"
  show Unit = "()"
  show (Error msg) = msg

getNum :: Value -> Int
getNum (NumV n) = n
getNum x = error $ "Value must be a NumV value. Got: " ++ show x

evalSexpr :: Sexpr -> State Env Value
evalSexpr (Number n) = return $ NumV n
evalSexpr (Ident x) = do
  val <- gets $ Map.lookup x
  return $ case val of
    Just v -> v
    Nothing -> Error $ "Variable named \"" ++ x ++ "\" does not exist!"
evalSexpr (Paren []) = return $ Error "List must not be empty!"
evalSexpr (Paren xs) = applyFunction xs

applyFunction :: [Sexpr] -> State Env Value
applyFunction ((Ident "define") : args) = applyDefine args
applyFunction ((Ident "let") : args) = applyLet args
applyFunction ((Ident "+") : args) = fmap (NumV . sum . map getNum) . mapM evalSexpr $ args
applyFunction ((Ident "*") : args) = fmap (NumV . product . map getNum) . mapM evalSexpr $ args
applyFunction _ = return $ Error "Not Implemented"

applyDefine :: [Sexpr] -> State Env Value
applyDefine ((Ident name) : expr : _) = do
  val <- evalSexpr expr
  modify (Map.insert name val)
  return Unit
applyDefine _ = return $ Error "Not Implemented"

applyLet :: [Sexpr] -> State Env Value
applyLet (decls:expr:_) = do
    ogEnv <- get
    extendWithDecls decls
    val <- evalSexpr expr
    put ogEnv
    return val
applyLet _ = return $ Error "Not Implemented"

extendWithDecls :: Sexpr -> State Env ()
extendWithDecls (Paren decls) = mapM_ extendWithDecl decls
  where
    extendWithDecl (Paren ((Ident name) : expr : _)) = do
      val <- evalSexpr expr
      modify (Map.insert name val)
