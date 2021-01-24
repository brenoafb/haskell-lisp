{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax
import Parser
import Interpreter
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Map as M

runProgram :: Program -> IO ()
runProgram [] = putStrLn ""
runProgram (x:xs) = do
  putStrLn $ "> " ++ show x
  case evalState (runExceptT (eval x)) baseEnv of
    Left err -> print err
    Right r -> print r >> runProgram xs

main :: IO ()
main = do
  input <- T.pack <$> getContents
  case parseStr input of
    Left err -> print err
    Right p -> do
      runProgram p

run :: Program -> IO ()
run = undefined

baseEnv = M.fromList
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
  ]
