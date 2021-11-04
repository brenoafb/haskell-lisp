{-# LANGUAGE OverloadedStrings #-}

module Sugar (desugar) where

import Syntax

desugar :: Expr -> Expr
desugar = desugarFunDef

desugarFunDef :: Expr -> Expr
desugarFunDef (List [ Atom "define"
                    , List ((List [Atom name, Atom typ]):args)
                    , body
                    ]) =
  List [ Atom "define", List [Atom name, typ']
       , List [ Atom "lambda"
              , Atom typ
              , List args
              , body
              ]
       ]
         where typ' = List $ (map (\(List [_, y]) -> y) args) ++ [Atom typ]

desugarFunDef (Quote e) = Quote $ desugarFunDef e
desugarFunDef (List xs) = List $ map desugarFunDef xs
desugarFunDef x = x
