{-# LANGUAGE OverloadedStrings #-}

module Sugar where

import Syntax

desugarFunDef :: Expr -> Expr
desugarFunDef (List [Atom "define", List (name:args), body]) =
  List [ Atom "define", name
       , List [ Atom "lambda"
              , List args
              , body
              ]
       ]
desugarFunDef (Quote e) = Quote $ desugarFunDef e
desugarFunDef (List xs) = List $ map desugarFunDef xs
desugarFunDef x = x
