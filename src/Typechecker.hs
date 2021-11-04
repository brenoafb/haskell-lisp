{-# LANGUAGE OverloadedStrings #-}
module Typechecker
  ( typecheck
  ) where

import Syntax
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Env as E
import qualified Data.Map as M
import qualified Data.Text as T

type TypeC t = ExceptT Error (State Ctx) t

getType :: Expr -> TypeC Type
getType (Str _) = pure StrT
getType (IntExpr _) = pure IntT
getType (DoubleExpr _) = pure DoubleT
getType (Atom a) = E.lookupM a
getType (NativeFunc _) = pure NativeT
getType (Quote e) =
  case e of
    (Atom _) -> pure AtomT
    (List xs) -> pure ListT
    _ -> throwError $ "Cannot get type of " <> (display (Quote e))
getType (List [ Atom "lambda", Atom retType
              , List args, _
              ]
        ) = do
  retType' <- getTypeFromIdent retType
  argTypes <- traverse getPairType args
  pure . FuncT $ argTypes ++ [retType']
getType l@(List (op:args)) = do
  fType <- getType op
  case fType of
    FuncT [] -> throwError $ "Invalid empty function type for " <> (display op)
    FuncT fType' -> do
      let argTypes = init fType'
      argTypes' <- mapM getType args
      if argTypes == argTypes' || argTypes == [AnyT]
         then pure $ last fType'
         else throwError $ "Type error in function call: "
                            <> (display l)
                            <> "\n"
                            <> "Types " <> (displayT $ FuncT argTypes)
                            <> " and  " <> (displayT $ FuncT argTypes')
                            <> " do not match"
    t -> throwError $ "Invalid function type " <> (displayT t)

typecheck :: Expr -> TypeC ()
typecheck (Str _) = pure ()
typecheck (IntExpr _) = pure ()
typecheck (DoubleExpr _) = pure ()
typecheck (Quote _) = pure ()
typecheck (NativeFunc _) = pure ()
typecheck (Atom _) = pure ()
typecheck l@(List [ Atom "lambda", retType
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
     then pure ()
     else throwError $ "Type error in lambda abstraction: "
                       <> (display l)
                       <> "\n"
                       <> "Return type " <> (display retType)
                       <> " and body type " <> (displayT bodyType)
                       <> " do not match"
typecheck l@(List [ Atom "define"
                  , List [Atom name, typ]
                  , expr
                  ]) = do
  typ' <- getTypeFromIdentifier typ
  exprTyp <- getType expr
  if typ' == exprTyp
     then modify (E.insert name typ')
     else throwError $ "Type error in definition "
                        <> (display l)
                        <> "\nType signature " <> display typ
                        <> " does not match actual type "
                        <> displayT exprTyp

typecheck l@(List (x:xs)) = getType l >> pure ()
typecheck (List []) = pure ()

getPairName :: Expr -> TypeC Ident
getPairName (List [Atom n, t]) = pure n
getPairName x =
  throwError $ "Invalid typed identifier " <> (display x)

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
getTypeFromIdent "Int" = pure IntT
getTypeFromIdent "String" = pure StrT
getTypeFromIdent "Double" = pure DoubleT
getTypeFromIdent "Atom" = pure AtomT
getTypeFromIdent "List" = pure ListT
getTypeFromIdent "Native" = pure ListT
getTypeFromIdent "Any" = pure AnyT
getTypeFromIdent t = throwError $ "Unknown type identifier " <> t

