module Kuljet.InterpreterType where

import Control.Monad.Reader
import qualified Database.SQLite3 as DB


data InterpreterState
  = InterpreterState { isDatabase :: DB.Database }


type Interpreter a
  = ReaderT InterpreterState IO a
