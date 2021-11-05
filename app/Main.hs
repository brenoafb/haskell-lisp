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
import System.Console.Haskeline hiding (display)
import qualified Data.Text as T

loadFile :: Env -> FilePath -> IO (Either Error Env)
loadFile env f = do
  input <- T.pack <$> readFile f
  case parseStr input of
    Left err -> return $ Left $ T.pack $ show err
    Right p ->
      return $ loadProgram env p

loadProgram :: Env -> Program -> Either Error Env
loadProgram env [] = return env
loadProgram env (x:_) =
  case runState (runExceptT (eval x)) env of
    (Left err, _)    -> Left err
    (Right _, env')  -> pure env'


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
    (Left err, ctx') -> outputStr (show err) >> typecheckProgram ctx' xs
    (Right typ, ctx')  -> outputStr (show typ) >> typecheckProgram ctx' xs

main :: IO ()
main = runInputT defaultSettings (repl baseEnv baseCtx)
  where repl :: Env -> Ctx -> InputT IO ()
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
