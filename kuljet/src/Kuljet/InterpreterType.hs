module Kuljet.InterpreterType where

import qualified Network.Wai as Wai
import Control.Monad.Reader
import qualified Database.SQLite3 as DB


data InterpreterState
  = InterpreterState
    { isDatabase :: DB.Database
    , isRequest :: Wai.Request
    }


type Interpreter a
  = ReaderT InterpreterState IO a
