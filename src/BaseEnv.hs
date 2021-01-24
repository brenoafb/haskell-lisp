{-# LANGUAGE OverloadedStrings #-}

module BaseEnv
  ( baseEnv
  ) where

import Syntax
import Interpreter
import Control.Monad.State
import Control.Monad.Except
import qualified Env as E
import qualified Data.Map as M
import qualified Data.Text as T

baseEnv :: Env
baseEnv = [M.fromList
  [ ("cons",
     NativeFunc (\args -> do  -- TODO check if arguments are valid right here (avoid unneeded computation)
       args' <- mapM eval args
       case args' of
         [x, List xs] -> return $ List (x:xs)
         _ -> throwError "cons: invalid arguments"
     ))
  , ("car",
     NativeFunc (\args -> do
       args' <- mapM eval args
       case args' of
         [List (x:xs)] -> return x
         _ -> throwError "car: invalid arguments"
     ))
  , ("cdr",
     NativeFunc (\args -> do
       args' <- mapM eval args
       case args' of
         [List (_:xs)] -> return $ List xs
         _ -> throwError "car: invalid arguments"
     ))
  , ("list",
     NativeFunc (\args -> do
       args' <- mapM eval args
       return $ List args'
     ))
  , ("type",
    NativeFunc (\args -> do
      args' <- mapM eval args
      case args' of
        [Atom _]       -> return $ Atom "atom"
        [List _]       -> return $ Atom "list"
        [IntExpr _]    -> return $ Atom "int"
        [DoubleExpr _] -> return $ Atom "double"
        [Str _]        -> return $ Atom "string"
        [NativeFunc _] -> return $ Atom "native"
        [Quote _]      -> return $ Atom "quote"
        _              -> throwError "type: invalid arguments"
     ))
  , ("null?",
    NativeFunc (\args -> do
      args' <- mapM eval args
      case args' of
        [List []] -> return true
        [_]       -> return nil
        _         -> throwError "type: invalid arguments"
     ))
  ]]
