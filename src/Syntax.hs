{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Text as T

type Program = [Expr]

type Frame = M.Map T.Text Expr
type Env = [Frame]
type Error = T.Text
type Eval t = ExceptT Error (State Env) t

data Expr = Atom       T.Text
          | Str        T.Text
          | IntExpr    Int
          | DoubleExpr Double
          | Quote      Expr
          | NativeFunc ([Expr] -> Eval Expr)
          | List       [Expr]

true :: Expr
true = Atom "#t"

nil :: Expr
nil  = List []

instance Show Expr where
  show (Atom t)       = T.unpack $ "Atom " <> t
  show (Str t)        = T.unpack $ "Str " <> "\"" <> t <> "\""
  show (IntExpr x)    = "IntExpr " ++ show x
  show (DoubleExpr x) = "DoubleExpr " ++ show x
  show (Quote x)      = "Quote " ++ show x
  show (NativeFunc _) = "<native function>"
  show (List xs)      = "List " ++ show xs

display :: Expr -> T.Text
display (Atom t)       = t
display (Str t)        = "\"" <> t <> "\""
display (IntExpr x)    = T.pack $ show x
display (DoubleExpr x) = T.pack $ show x
display (Quote t)      = "'" <> display t
display (NativeFunc _) = "<native function>"
display (List xs)      = "(" <> T.unwords (map display xs) <> ")"

instance Eq Expr where
  (Atom x)       == (Atom y)       = x == y
  (Str  x)       == (Str  y)       = x == y
  (IntExpr x)    == (IntExpr y)    = x == y
  (DoubleExpr x) == (DoubleExpr y) = x == y
  (Quote x)      == (Quote y)      = x == y
  (List xs)      == (List ys)      = xs == ys
  _              == _              = False
