module Eval (evalSexpr, Env, globalEnvironment) where

import Control.Monad.State (get, gets, modify, put)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
-- import qualified Data.Map as Map
import qualified Data.Map as Map
import EvalTypes
import Parser (Sexpr (..))

globalEnvironment :: Env
globalEnvironment =
  Map.fromList
    [ ("+", NativeFunc nativeSum),
      ("*", NativeFunc nativeProduct),
      ("&&", NativeFunc nativeLogicalAnd),
      ("||", NativeFunc nativeLogicalOr),
      ("~", NativeFunc nativeLogicalNot),
      ("=", NativeFunc nativeEquals),
      ("<", NativeFunc nativeLesserThan),
      ("if", NativeFunc nativeIf)
    ]

nativeSum :: [Sexpr] -> ExecState Value
nativeSum = fmap (NumV . sum . map getNum) . mapM evalSexpr

nativeProduct :: [Sexpr] -> ExecState Value
nativeProduct = fmap (NumV . product . map getNum) . mapM evalSexpr

nativeLogicalAnd :: [Sexpr] -> ExecState Value
nativeLogicalAnd = fmap (BoolV . all getBool) . mapM evalSexpr

nativeLogicalOr :: [Sexpr] -> ExecState Value
nativeLogicalOr = fmap (BoolV . any getBool) . mapM evalSexpr

nativeLogicalNot :: [Sexpr] -> ExecState Value
nativeLogicalNot [] = lift . Left $ "~ function requires a single argument!"
nativeLogicalNot args = fmap (BoolV . not . getBool) . evalSexpr . head $ args

nativeEquals :: [Sexpr] -> ExecState Value
nativeEquals [arg1, arg2] = do
  v1 <- evalSexpr arg1
  v2 <- evalSexpr arg2
  return . BoolV $ getNum v1 == getNum v2
nativeEquals _ = lift . Left $ "= function requires two arguments!"

nativeLesserThan :: [Sexpr] -> ExecState Value
nativeLesserThan [arg1, arg2] = do
  v1 <- evalSexpr arg1
  v2 <- evalSexpr arg2
  return . BoolV $ getNum v1 < getNum v2
nativeLesserThan _ = lift . Left $ "< function requires two arguments!"

nativeIf :: [Sexpr] -> ExecState Value
nativeIf [cond, ifTrue, ifFalse] = do
  cv <- evalSexpr cond
  case cv of
    BoolV True -> evalSexpr ifTrue
    BoolV False -> evalSexpr ifFalse
    _ -> lift . Left $ "Condition expression must be a boolean value!"
nativeIf _ = lift . Left $ "If function requires 3 arguments!"

getNum :: Value -> Int
getNum (NumV n) = n
getNum x = error $ "Value must be a NumV value. Got: " ++ show x

getBool :: Value -> Bool
getBool (BoolV b) = b
getBool x = error $ "Value must be a BoolV value. Got: " ++ show x

evalSexpr :: Sexpr -> ExecState Value
evalSexpr (Number n) = return $ NumV n
evalSexpr (Ident x) = do
  val <- gets $ Map.lookup x
  case val of
    Just v -> return v
    Nothing -> lift . Left $ "Variable named \"" ++ x ++ "\" does not exist!"
evalSexpr (Paren []) = lift . Left $ "Parenthesized list must not be empty!"
evalSexpr (Paren xs) = applyFunction xs
evalSexpr (Boolean b) = return . BoolV $ b

applyFunction :: [Sexpr] -> ExecState Value
applyFunction ((Ident "define") : args) = applyDefine args
applyFunction ((Ident "let") : args) = applyLet args
applyFunction ((Ident name) : args) = do
  func <- gets (Map.lookup name)
  case func of
    Nothing -> lift . Left $ "Function named \"" ++ name ++ "\" does not exist!"
    Just (Func argNames body) -> do
      argVals <- mapM evalSexpr args
      if length argNames /= length argVals
        then lift . Left $ "Function arity doesn't match! Arity is: " ++ (show . length $ argNames)
        else do
          ogEnv <- get
          let newEnv = Map.union (Map.fromList (zip argNames argVals)) ogEnv
          put newEnv
          val <- evalSexpr body
          put ogEnv
          return val
    Just (NativeFunc nfunc) -> nfunc args
    _ -> lift . Left $ "\"" ++ name ++ "\" is not a function!"
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
