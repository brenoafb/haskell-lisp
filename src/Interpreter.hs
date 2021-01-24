module Interpreter where

import Syntax
import Parser
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

