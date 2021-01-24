{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax
import Parser
import qualified Data.Text as T

main :: IO ()
main = do
  putStr "> "
  line <- T.pack <$> getLine
  case parseStr line of
    Left err -> print err
    Right p -> print p
