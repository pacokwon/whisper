module EvalTypes where

import Parser (Sexpr)
import Data.Map (Map)
import Control.Monad.State (StateT)

data Value
  = NumV Int
  | BoolV Bool
  | NativeFunc ([Sexpr] -> ExecState Value)
  | Func [String] Sexpr
  | Unit

type Env = Map String Value
type ExecState = StateT Env (Either String)

instance Show Value where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (NativeFunc _) = "<native function>"
  show (Func _args _) = "<function>"
  show Unit = ""
