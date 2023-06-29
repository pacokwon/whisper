module Eval (evalSexpr, Env) where

import Control.Monad.State (State, StateT, get, gets, modify, put)

import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
-- import qualified Data.Map as Map
import qualified Data.Map as Map
import Parser (Sexpr (..))

data Value
  = NumV Int
  | Func ([Value] -> Value)
  | Unit

type Env = Map String Value

type ExecState = StateT Env (Either String)

instance Show Value where
  show (NumV n) = show n
  show (Func _) = "<function>"
  show Unit = "()"

getNum :: Value -> Int
getNum (NumV n) = n
getNum x = error $ "Value must be a NumV value. Got: " ++ show x

evalSexpr :: Sexpr -> ExecState Value
evalSexpr (Number n) = return $ NumV n
evalSexpr (Ident x) = do
  val <- gets $ Map.lookup x
  case val of
    Just v -> return v
    Nothing -> lift . Left $ "Variable named \"" ++ x ++ "\" does not exist!"
evalSexpr (Paren []) = lift . Left $ "Parenthesized list must not be empty!"
evalSexpr (Paren xs) = applyFunction xs

applyFunction :: [Sexpr] -> ExecState Value
applyFunction ((Ident "define") : args) = applyDefine args
applyFunction ((Ident "let") : args) = applyLet args
applyFunction ((Ident "+") : args) = fmap (NumV . sum . map getNum) . mapM evalSexpr $ args
applyFunction ((Ident "*") : args) = fmap (NumV . product . map getNum) . mapM evalSexpr $ args
applyFunction _ = lift . Left $ "Invalid function application!"

applyDefine :: [Sexpr] -> ExecState Value
applyDefine ((Ident name) : expr : _) = do
  val <- evalSexpr expr
  modify (Map.insert name val)
  return Unit
applyDefine _ = lift . Left $ "Define construct must have an identifier first, and then an expression!"

applyLet :: [Sexpr] -> ExecState Value
applyLet (decls : expr : _) = do
  ogEnv <- get
  extendWithDecls decls
  val <- evalSexpr expr
  put ogEnv
  return val
applyLet _ = lift . Left $ "Let construct must include two elements. A declaration and an expression!"

extendWithDecls :: Sexpr -> ExecState ()
extendWithDecls (Paren decls) = mapM_ extendWithDecl decls
  where
    extendWithDecl (Paren ((Ident name) : expr : _)) = do
      val <- evalSexpr expr
      modify (Map.insert name val)
    extendWithDecl _ = lift . Left $ "A declaration must be a parenthesized form, like (a 1)"
extendWithDecls _ = lift . Left $ "Declarations must be a parenthesized form, like ((a 1) (b 2))"
