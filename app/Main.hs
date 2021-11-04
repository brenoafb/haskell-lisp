{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax
import Parser
import Sugar
import Interpreter
import Typechecker
import BaseEnv

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import System.Environment

import qualified Env as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
runProgram env [] = putStrLn "" >> pure env
runProgram env (x:xs) = do
  TIO.putStrLn $ "> " <> display x
  case runState (runExceptT (eval x)) env of
    (Left err, env') -> print err >> runProgram env' xs
    (Right r, env')  -> putStrLn (T.unpack (display r)) >> runProgram env' xs

typecheckProgram :: Ctx -> Program -> IO Ctx
typecheckProgram ctx [] = putStrLn "" >> pure ctx
typecheckProgram ctx (x:xs) = do
  putStrLn . T.unpack $ "> " <> display x
  case runState (runExceptT (typecheck x)) ctx of
    (Left err, ctx') -> TIO.putStrLn err >> typecheckProgram ctx' xs
    (Right (), ctx')  -> putStrLn "Ok" >> typecheckProgram ctx' xs

main :: IO ()
main = getArgs >>= parse

parse ["-t"] = do
  input <- TIO.getContents
  case parseStr input of
    Left err -> print err
    Right p -> do
      env <- typecheckProgram baseCtx $ map desugar p
      return ()
parse _= do
  input <- TIO.getContents
  case parseStr input of
    Left err -> print err
    Right p -> do
      env <- runProgram baseEnv $ map (removeTypes . desugar) p
      return ()

removeTypes :: Expr -> Expr
removeTypes (List [Atom "define", List [name, typ], expr]) =
  List [Atom "define", name, expr]
removeTypes (List [Atom "lambda", typ, List args, expr]) =
  List [Atom "lambda", List args', expr]
    where args' = map (\(List [n, t]) -> n) args
removeTypes (List xs) = List $ map removeTypes xs
removeTypes (Quote q) = Quote $ removeTypes q
removeTypes x = x

