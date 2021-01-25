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

runProgram :: Env -> Program -> IO ()
runProgram env [] = putStrLn ""
runProgram env (x:xs) = do
  putStrLn . T.unpack $ "> " <> display x
  case runState (runExceptT (eval x)) env of
    (Left err, env') -> print err >> runProgram env' xs
    (Right r, env')  -> print r >> runProgram env' xs

main :: IO ()
main = do
  input <- T.pack <$> getContents
  case parseStr input of
    Left err -> print err
    Right p -> do
      runProgram baseEnv p
