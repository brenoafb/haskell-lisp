{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import Syntax
import Parser
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

eval l@(List [Atom "lambda", List args, body]) = return l

eval (List (Atom "cond" : [])) = return nil

eval (List (Atom "cond" : List [cond, conseq] : xs)) = do
  cond' <- eval cond
  if cond' == true
     then eval conseq
     else eval $ List ((Atom "cond") : xs)

eval (List [Atom "define", Atom n, e]) = do
  e' <- eval e
  modify (E.insert n e')
  return true

eval (List [Atom "let", List args, body]) = do
  frame <- getLetBindings args
  modify (E.push frame)
  result <- eval body
  modify E.pop
  return result

eval (List (x:xs)) = do
  env <- get
  x' <- eval x
  case x' of
    NativeFunc f -> do
      args <- mapM eval xs
      f args
    List [Atom "lambda", List args, body] -> do
      xs' <- mapM eval xs
      names <- getArgNames args
      let frame = M.fromList $ zip names xs'
      modify (E.push frame)
      result <- eval body
      modify E.pop
      return result
    _ -> throwError $ "Not a function: " <> display x'

eval e = throwError $ "Invalid expression " <> T.pack (show e)

getArgNames :: [Expr] -> Eval [T.Text]
getArgNames = mapM f
  where f :: Expr -> Eval T.Text
        f (Atom t) = return t
        f x        = throwError $ "Invalid argument " <> display x

getLetBindings :: [Expr] -> Eval Frame
getLetBindings xs = M.fromList <$> mapM f xs
  where f :: Expr -> Eval (T.Text, Expr)
        f (List [Atom name, e]) = do
          e' <- eval e
          return (name, e')
        f x = throwError $ "Invalid let binding " <> display x
