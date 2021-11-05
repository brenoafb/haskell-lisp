{-# LANGUAGE OverloadedStrings #-}
module Typechecker
  ( getType
  ) where

import Syntax
import Control.Monad.State
import Control.Monad.Except
import qualified Env as E
import qualified Data.Map as M
import Data.List (group)

type TypeC t = ExceptT Error (State Ctx) t

getType :: Expr -> TypeC Type
getType (Str _) = pure StrT
getType (BoolExpr _) = pure BoolT
getType (IntExpr _) = pure IntT
getType (DoubleExpr _) = pure DoubleT
getType (Atom a) = E.lookupM a
getType (NativeFunc _) = pure NativeT
getType (Quote e) =
  case e of
    (Atom _) -> pure AtomT
    (List _) -> pure ListT
    _ -> throwError $ "Cannot get type of " <> (display (Quote e))

getType (List [ Atom "lambda", Atom retType
              , List args, expr
              ]
        ) = do
  retType' <- getTypeFromIdent retType
  argTypes <- traverse getPairType args
  pure . FuncT $ argTypes ++ [retType']

getType l@(List [ Atom "lambda", retType
                  , List args, body
                  ]
            ) = do
  ret      <- getTypeFromIdentifier retType
  argTypes <- mapM getPairType args
  argNames <- mapM getPairName args
  let frame = M.fromList $ zip argNames argTypes
  modify (E.push frame)
  bodyType <- getType body
  modify E.pop
  if ret == bodyType || ret == AnyT || bodyType == AnyT
     then pure $ getMostSpecificType ret bodyType
     else throwError $ "Type error in lambda abstraction: "
                       <> (display l)
                       <> "\n"
                       <> "Return type " <> (display retType)
                       <> " and body type " <> (displayT bodyType)
                       <> " do not match"

getType l@(List [ Atom "define"
                  , List [Atom name, typ]
                  , expr
                  ]) = do
  typ' <- getTypeFromIdentifier typ
  exprTyp <- getType expr
  if typ' == exprTyp
     then modify (E.insert name typ') >> pure typ'
     else throwError $ "Type error in definition "
                        <> (display l)
                        <> "\nType signature " <> display typ
                        <> " does not match actual type "
                        <> displayT exprTyp

getType (List (Atom "cond" : clauses)) = do
  conds <- mapM (\x -> getFirst x >>= getType) clauses
  results <- mapM (\x -> getSecond x >>= getType) clauses
  if all (== BoolT) conds && allEqual results
     then case results of
            [] -> pure AnyT
            (x:_) -> pure x
     else throwError "Type error in cond clause"

getType l@(List (op:args)) = do
  fType <- getType op
  case fType of
    FuncT [] -> throwError $ "Invalid empty function type for " <> (display op)
    FuncT fType' -> do
      let argTypes = init fType'
      argTypes' <- mapM getType args
      if matchTypes argTypes argTypes' || argTypes == [AnyT]
         then pure $ last fType'
         else throwError $ "Type error in function call: "
                            <> (display l)
                            <> "\n"
                            <> "Types " <> (displayT $ FuncT argTypes)
                            <> " and  " <> (displayT $ FuncT argTypes')
                            <> " do not match"
    t -> throwError $ "Invalid function type " <> (displayT t)

getType (List []) = pure ListT

matchTypes :: [Type] -> [Type] -> Bool
matchTypes [] [] = True
matchTypes (x:xs) (y:ys)
  | x == y || x == AnyT || y == AnyT = matchTypes xs ys
  | otherwise = False

getPairName :: Expr -> TypeC Ident
getPairName (List [Atom n, _]) = pure n
getPairName x =
  throwError $ "Invalid typed identifier " <> (display x)

allEqual xs = (length $ group xs) == 1

getFirst  (List [x, _]) = pure x
getFirst _ = throwError "getFirst: invalid expression"

getSecond (List [_, x]) = pure x
getSecond _ = throwError "getSecond: invalid expression"

getPairType :: Expr -> TypeC Type
getPairType (List [_, t]) = getTypeFromIdentifier t
getPairType x =
  throwError $ "Invalid typed identifier " <> (display x)

getTypeFromIdentifier :: Expr -> TypeC Type
getTypeFromIdentifier (Atom t) = getTypeFromIdent t
getTypeFromIdentifier (List ts) = FuncT <$> traverse getTypeFromIdentifier ts
getTypeFromIdentifier x =
  throwError $ "Invalid type identifier " <> (display x)

getTypeFromIdent :: Ident -> TypeC Type
getTypeFromIdent "int" = pure IntT
getTypeFromIdent "bool" = pure BoolT
getTypeFromIdent "string" = pure StrT
getTypeFromIdent "double" = pure DoubleT
getTypeFromIdent "atom" = pure AtomT
getTypeFromIdent "list" = pure ListT
getTypeFromIdent "native" = pure ListT
getTypeFromIdent "any" = pure AnyT
getTypeFromIdent t = throwError $ "Unknown type identifier " <> t

getMostSpecificType :: Type -> Type -> Type
getMostSpecificType AnyT t = t
getMostSpecificType t AnyT = t
getMostSpecificType t _ = t
