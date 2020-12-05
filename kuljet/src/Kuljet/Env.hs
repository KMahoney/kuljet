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
  [ (Symbol "redirect", (fn1 fRedirect, tText --> tResponse))
  , (Symbol "file", (fn2 fFile, tText --> tText --> tResponse))
  , (Symbol "getTimestamp", (VAction fNow, tIO tTimestamp))
  , (Symbol "docType", (VHtml (HtmlEmitStr "<!DOCTYPE html>"), tHtml))
  , (Symbol "genUUID", (VAction fUUID, tIO tText))
  , (Symbol "addCookie", (fn3 fAddCookie, tResponse --> tText --> tText --> tResponse))
  , (Symbol "maybe", (fn3 fMaybe, tMaybe v1 --> v0 --> (v1 --> v0) --> v0))
  ]

  where
    infixr -->
    (-->) = tFn

    v0 = TVar 0
    v1 = TVar 1


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


fMaybe :: Value -> Value -> Value -> IO Value
fMaybe maybeValue defaultValue fValue =
  case valueAsMaybe maybeValue of
    Just v -> (valueAsFn fValue) v
    Nothing -> return defaultValue
