{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import qualified Data.Text as T
import Data.Generics

type Program = [Expr]

data Expr = Atom       T.Text
          | Str        T.Text
          | IntExpr    Int
          | DoubleExpr Double
          | List       [Expr]
          | Quote      Expr
          deriving (Show, Data, Typeable)
