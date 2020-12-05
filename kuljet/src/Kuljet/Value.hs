module Kuljet.Value where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as Time

import Kuljet.Symbol
import Kuljet.InterpreterType
import qualified Database.QueryBuilder as Query


data Value
  = VHtml HtmlEmitter
  | VText { valueAsText :: T.Text }
  | VInt { valueAsInteger :: Integer }
  | VBool { valueAsBool :: Bool }
  | VQuery { valueAsQuery :: Query.Query }
  | VResponse { valueAsResponse :: Response }
  | VTimestamp { valueAsTimestamp :: Time.UTCTime }
  | VRecord { valueAsRecord :: (M.Map Symbol Value) }
  | VList { valueAsList :: [Value] }
  | VFn { valueAsFn :: Value -> Interpreter Value }
  | VAction { valueAsAction :: Interpreter Value }
  | VMaybe { valueAsMaybe :: Maybe Value }
  | VUnit


data HtmlEmitter
  = HtmlEmitStr T.Text
  | HtmlEmitTag T.Text
  | HtmlEmitTagWithAttrs T.Text (M.Map Symbol Value)


data Response =
  Response { responseStatus :: HTTP.Status
           , responseHeaders :: HTTP.ResponseHeaders
           , responseBody :: LBS.ByteString
           }


partialEq :: Value -> Value -> Bool
partialEq a b =
  case (a, b) of
    (VText t1, VText t2) ->
      t1 == t2
    (VList l1, VList l2) ->
      length l1 == length l2 && all (uncurry partialEq) (zip l1 l2)
    (VRecord r1, VRecord r2) ->
      M.keysSet r1 == M.keysSet r2 && all id (M.elems (M.intersectionWith partialEq r1 r2))
    (VInt i1, VInt i2) ->
      i1 == i2
    (VTimestamp t1, VTimestamp t2) ->
      t1 == t2
    (VBool b1, VBool b2) ->
      b1 == b2
    _ ->
      undefined


partialCompare :: Value -> Value -> Ordering
partialCompare a b =
  case (a, b) of
    (VText t1, VText t2) ->
      compare t1 t2
    (VInt i1, VInt i2) ->
      compare i1 i2
    (VTimestamp t1, VTimestamp t2) ->
      compare t1 t2
    (VBool b1, VBool b2) ->
      compare b1 b2
    _ ->
      undefined
