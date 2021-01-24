{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import Data.Generics
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Text as T

type Program = [Expr]

type Env = M.Map T.Text Expr
type Error = T.Text
type Eval t = ExceptT Error (State Env) t

data Expr = Atom       T.Text
          | Str        T.Text
          | IntExpr    Int
          | DoubleExpr Double
          | Quote      Expr
          | NativeFunc ([Expr] -> Eval Expr)
          | List       [Expr]

instance Show Expr where
  show (Atom t)       = T.unpack $ "Atom " <> t
  show (Str t)        = T.unpack $ "Str " <> "\"" <> t <> "\""
  show (IntExpr x)    = "IntExpr " ++ show x
  show (DoubleExpr x) = "DoubleExpr " ++ show x
  show (Quote x)      = "Quote " ++ show x
  show (NativeFunc x) = "<native function>"
  show (List xs)      = "List " ++ show xs
  -- show (List xs) = show $
  --   "("
  --   <> foldr (\x acc -> x <> " " <> acc) "" $ map show xs
  --   <> ")"


