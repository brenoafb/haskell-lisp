{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Env where

import Prelude hiding (lookup)
import Syntax
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.State

lookupM :: (Ord t, Show t, MonadState [M.Map t a] m, MonadError T.Text m)
        => t -> m a
lookupM k = do
  env <- get
  case lookup k env of
    Nothing -> throwError $ "Undefined symbol " <> (T.pack $ show k)
    Just x  -> pure x

lookup :: Ord t => t -> [M.Map t a] -> Maybe a
lookup _ [] = Nothing
lookup k (x:xs) =
  case M.lookup k x of
    Nothing -> lookup k xs
    Just e -> return e

insert :: Ord k => k -> a -> [M.Map k a] -> [M.Map k a]
insert k e (x:xs) =
  let x' = M.insert k e x
   in (x':xs)
insert k e [] = [M.fromList [(k, e)]]

push :: a -> [a] -> [a]
push x xs = x:xs

pop :: [a] -> [a]
pop [] = error "popping empty stack"  -- TODO find a bettew way of handling this case
pop (_:xs) = xs
