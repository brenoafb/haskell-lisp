{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax
import Parser
import Interpreter
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import BaseEnv
import qualified Env as E
import qualified Data.Text as T
import qualified Data.Map as M

runProgram :: Program -> IO ()
runProgram [] = putStrLn ""
runProgram (x:xs) = do
  putStrLn . T.unpack $ "> " <> display x
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
