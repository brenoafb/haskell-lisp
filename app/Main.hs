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

loadFile :: Env -> FilePath -> IO (Either Error Env)
loadFile env f = do
  input <- T.pack <$> readFile f
  case parseStr input of
    Left err -> return $ Left $ T.pack $ show err
    Right p ->
      return $ loadProgram env p

loadProgram :: Env -> Program -> Either Error Env
loadProgram env [] = return env
loadProgram env (x:xs) =
  case runState (runExceptT (eval x)) env of
    (Left err, _)    -> Left err
    (Right _, env')  -> pure env'


runProgram :: Env -> Program -> IO Env
runProgram env [] = putStrLn "" >> return env
runProgram env (x:xs) = do
  putStrLn . T.unpack $ "> " <> display x
  case runState (runExceptT (eval x)) env of
    (Left err, env') -> print err >> runProgram env' xs
    (Right r, env')  -> putStrLn (T.unpack (display r)) >> runProgram env' xs

main :: IO ()
main = do
  input <- T.pack <$> getContents
  case parseStr input of
    Left err -> print err
    Right p -> do
      env <- runProgram baseEnv p
      return ()
