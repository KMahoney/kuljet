module Kuljet.Env where

import qualified System.IO.Error as IO
import qualified Data.Map as M
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Kuljet.Symbol
import Kuljet.Type
import Kuljet.Value


htmlTags :: [Symbol]
htmlTags =
  map Symbol
  [ "body", "html", "head", "link", "nav"
  , "p", "div", "span", "a"
  , "strong", "em"
  , "form", "input", "textarea", "label"
  , "ul", "ol", "li"
  , "h1", "h2", "h3", "h4", "h5"
  ]


stdEnv :: M.Map Symbol (Value, Type)
stdEnv =
  M.fromList
  [ (Symbol "redirect", (fn1 fRedirect, TFn TText TResponse))
  , (Symbol "file", (fn2 fFile, TFn TText (TFn TText TResponse)))
  , (Symbol "getTimestamp", (VAction fNow, TIO TTimestamp))
  , (Symbol "docType", (VHtml (HtmlEmitStr "<!DOCTYPE html>"), THtml))
  , (Symbol "genUUID", (VAction fUUID, TIO TText))
  , (Symbol "addCookie", (fn3 fAddCookie, TFn TResponse (TFn TText (TFn TText TResponse))))
  ]


fn1 :: (Value -> IO Value) -> Value
fn1 = VFn


fn2 :: (Value -> Value -> IO Value) -> Value
fn2 f = VFn (\arg1 -> return (VFn (\arg2 -> f arg1 arg2)))


fn3 :: (Value -> Value -> Value -> IO Value) -> Value
fn3 f = VFn (\arg1 -> return (VFn (\arg2 -> return (VFn (\arg3 -> f arg1 arg2 arg3)))))


fRedirect :: Value -> IO Value
fRedirect locationValue =
  return $ VResponse (Response { responseStatus = HTTP.found302
                               , responseHeaders = [(HTTP.hLocation, T.encodeUtf8 location)]
                               , responseBody = ""
                               })
  where
    location =
      valueAsText locationValue


fFile :: Value -> Value -> IO Value
fFile contentTypeValue filenameValue =
  IO.catchIOError okRead errRead

  where
    contentType =
      valueAsText contentTypeValue

    filename =
      valueAsText filenameValue
      
    okRead =
      okResponse <$> LBS.readFile (T.unpack filename)

    okResponse content =
      VResponse $ Response
        { responseStatus = HTTP.ok200
        , responseHeaders = [(HTTP.hContentType, T.encodeUtf8 contentType)]
        , responseBody = content
        }

    errRead err =
      if IO.isDoesNotExistError err
      then return notFound
      else return internalError
        
    notFound =
      VResponse $ Response
        { responseStatus = HTTP.notFound404
        , responseHeaders = []
        , responseBody = "File not found"
        }

    internalError =
      VResponse $ Response
        { responseStatus = HTTP.internalServerError500
        , responseHeaders = []
        , responseBody = "Internal server error"
        }


fNow :: IO Value
fNow =
  VTimestamp <$> Time.getCurrentTime


fUUID :: IO Value
fUUID =
  (VText . UUID.toText) <$> UUID.nextRandom


fAddCookie :: Value -> Value -> Value -> IO Value
fAddCookie responseValue nameValue valueValue =
  return $ VResponse $ response { responseHeaders = headers }

  where
    response =
      valueAsResponse responseValue

    cookie =
      T.encodeUtf8 (valueAsText nameValue <> "=" <> valueAsText valueValue <> "; Path=/;")

    headers =
      (HTTP.hSetCookie, cookie) : responseHeaders response
