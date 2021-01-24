module Env where

import Prelude hiding (lookup)
import Syntax
import qualified Data.Map as M
import qualified Data.Text as T

lookup :: T.Text -> Env -> Maybe Expr
lookup _ [] = Nothing
lookup k (x:xs) =
  case M.lookup k x of
    Nothing -> lookup k xs
    Just e -> return e

insert :: T.Text -> Expr -> Env -> Env
insert k e (x:xs) =
  let x' = M.insert k e x
   in (x':xs)
insert k e [] = [M.fromList [(k, e)]]

push :: Frame -> Env -> Env
push x xs = x:xs

pop :: Env -> Env
pop [] = error "popping empty stack"  -- TODO find a bettew way of handling this case
pop (_:xs) = xs
