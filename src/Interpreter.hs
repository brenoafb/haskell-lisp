{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import Syntax
import Parser
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Env as E
import qualified Data.Map as M
import qualified Data.Text as T

eval :: Expr -> Eval Expr

eval e@(Str _) = return e
eval e@(IntExpr _) = return e
eval e@(DoubleExpr _) = return e

eval (Atom t) = do
  env <- get
  case E.lookup t env of
    Nothing -> throwError $ "Undefined symbol " <> t
    Just e -> return e

eval (Quote e) = return e

eval e@(List []) = return e

eval l@(List ((Atom "lambda"):xs)) = return l

eval (List [Atom "define", Atom n, e]) = do
  e' <- eval e
  modify (E.insert n e')
  return true

eval (List ((Atom x):xs)) = do
  env <- get
  case E.lookup x env of
    Nothing -> throwError $ "Undefined function " <> x
    Just (NativeFunc f) -> f xs
    Just (List [Atom "lambda", args, body]) -> undefined -- TODO: apply lambda
    _ -> throwError $ "Not a function: " <> x

eval e = throwError $ "Invalid expression " <> display e
