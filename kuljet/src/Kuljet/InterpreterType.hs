module Kuljet.InterpreterType where

import qualified Data.Time.Clock as Time
import qualified Network.Wai as Wai
import Control.Monad.Reader
import qualified Database.SQLite3 as DB


data InterpreterState
  = InterpreterState
    { isHashCost :: Int
    , isDatabase :: DB.Database
    , isRequest :: Wai.Request
    , isTimestamp :: Time.UTCTime
    }


type Interpreter a
  = ReaderT InterpreterState IO a
