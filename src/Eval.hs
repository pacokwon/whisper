module Eval (evalSexpr, Env) where

import Control.Monad.State (StateT, get, gets, modify, put)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
-- import qualified Data.Map as Map
import qualified Data.Map as Map
import Parser (Sexpr (..))

data Value
  = NumV Int
  | NativeFunc ([Value] -> Value)
  | Func [String] Sexpr
  | Unit

type Env = Map String Value

type ExecState = StateT Env (Either String)

instance Show Value where
  show (NumV n) = show n
  show (NativeFunc _) = "<function>"
  show (Func _args _) = "<function>"
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
applyFunction ((Ident name) : args) = do
    func <- gets (Map.lookup name)
    case func of
        Nothing -> lift . Left $ "Function named \"" ++ name ++ "\" does not exist!"
        Just (Func argNames body) -> do
            argVals <- mapM evalSexpr args
            if length argNames /= length argVals then
                lift . Left $ "Function arity doesn't match! Arity is: " ++ (show . length $ argNames)
            else do
                ogEnv <- get
                let newEnv = Map.union ogEnv $ Map.fromList (zip argNames argVals)
                put newEnv
                val <- evalSexpr body
                put ogEnv
                return val
        Just _ -> lift . Left $ "\"" ++ name ++ "\" is not a function!"
applyFunction _ = lift . Left $ "Invalid function application!"

applyDefine :: [Sexpr] -> ExecState Value
applyDefine ((Ident name) : expr : _) = do
  val <- evalSexpr expr
  modify (Map.insert name val)
  return Unit
applyDefine ((Paren ((Ident funcName) : args)) : expr : _) = do
  argStr <- mapM identToString args
  let func = Func argStr expr
  modify (Map.insert funcName func)
  return Unit
  where
    identToString (Ident i) = return i
    identToString _ = lift . Left $ "Function argument must be an identifier!"
applyDefine ((Paren _) : _) = lift . Left $ "To define functions, use a syntax like (define (f arg1 arg2) (+ arg1 arg2))"
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
