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
import System.Console.Haskeline hiding (display)
import qualified Data.Text as T

loadFile :: Env -> Ctx -> FilePath -> IO (Env, Ctx)
loadFile env ctx f = do
  input <- T.pack <$> readFile f
  case parseStr input of
    Left err -> putStrLn (show err) >> pure (env, ctx)
    Right p  -> loadProgram env ctx (map desugar p)

loadProgram :: Env -> Ctx -> Program -> IO (Env, Ctx)
loadProgram env ctx [] = pure (env, ctx)
loadProgram env ctx (x:xs) = do
  putStr $ (T.unpack $ display x) <> " // " <> (T.unpack . display $ removeTypes x) <> " // "
  case runState (runExceptT (getType x)) ctx of
    (Left err, ctx')  -> putStrLn (show err) >> pure (env, ctx)
    (Right typ, ctx') -> do
      putStr $ (show typ) <> " "
      case runState (runExceptT (eval (removeTypes x))) env of
        (Left err, _)    -> putStrLn (show err) >> pure (env, ctx)
        (Right result, env')  -> putStrLn (show result) >> loadProgram env' ctx' xs


runProgram :: Env -> Program -> InputT IO Env
runProgram env [] = outputStrLn "" >> pure env
runProgram env (x:xs) = do
  case runState (runExceptT (eval x)) env of
    (Left err, env') -> outputStr (show err) >> runProgram env' xs
    (Right r, env')  -> outputStr (T.unpack (display r)) >> runProgram env' xs

typecheckProgram :: Ctx -> Program -> InputT IO Ctx
typecheckProgram ctx [] = outputStrLn "" >> pure ctx
typecheckProgram ctx (x:xs) = do
  case runState (runExceptT (getType x)) ctx of
    (Left err, ctx')  -> outputStr (show err) >> typecheckProgram ctx' xs
    (Right typ, ctx') -> outputStr (show typ) >> typecheckProgram ctx' xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r"] -> runInputT defaultSettings (repl baseEnv baseCtx)
    ["-l"] -> undefined -- TODO load file then start repl
    [filename] -> loadFile baseEnv baseCtx filename >> pure ()
    [] -> undefined

repl :: Env -> Ctx -> InputT IO ()
repl env ctx = do
  input <- getInputLine "> "
  case input of
    Nothing -> pure ()
    Just input' ->
      case parseStr (T.pack input') of
        Left err -> liftIO (print err) >> repl env ctx
        Right p  -> do
          let p' = map desugar p
              p'' = map removeTypes p'
          outputStrLn $ "desugared: " <> (T.unpack $ mconcat $ map display p')
          outputStrLn $ "detyped: " <> (T.unpack $ mconcat $ map display p'')
          ctx' <- typecheckProgram ctx p'
          env' <- runProgram env p''
          repl env' ctx'

removeTypes :: Expr -> Expr
removeTypes (List [Atom "define", List [name, _], e]) =
  List [Atom "define", name, removeTypes e]
removeTypes (List [Atom "lambda", _, List args, e]) =
  List [Atom "lambda", List args', e]
    where args' = map (\(List [n, _]) -> n) args
removeTypes (List xs) = List $ map removeTypes xs
removeTypes (Quote q) = Quote $ removeTypes q
removeTypes x = x
