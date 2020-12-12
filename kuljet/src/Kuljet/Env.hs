module Kuljet.Env where

import qualified System.IO.Error as IO
import qualified Data.Map as M
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Control.Monad.Reader
import qualified Web.Cookie as Cookie
import qualified System.Entropy as Entropy
import qualified Data.ByteString.Base64 as Base64

import Kuljet.Symbol
import Kuljet.Type
import Kuljet.Value
import Kuljet.InterpreterType


htmlTags :: [Symbol]
htmlTags =
  map Symbol
  [ "body", "html", "head", "link", "nav"
  , "meta"
  , "p", "div", "span", "a"
  , "strong", "em"
  , "form", "input", "textarea", "label"
  , "ul", "ol", "li"
  , "h1", "h2", "h3", "h4", "h5"
  , "main"
  ]


type EnvValue =
  Reader InterpreterState Value


stdEnv :: M.Map Symbol (EnvValue, Type)
stdEnv =
  M.fromList (tagEnvList ++ stdEnvList)

  where
    tagEnvList =
      map (\tag -> (tag, (return $ VHtml $ HtmlEmitTag $ symbolName tag, tHtmlTag))) htmlTags

    stdEnvList =
      [ (Symbol "redirect", (fn1 fRedirect, tText --> tResponse))
      , (Symbol "file", (fn2 fFile, tText --> tText --> tResponse))
      , (Symbol "now", (fNow, tTimestamp))
      , (Symbol "docType", (return $ VHtml $ HtmlEmitStr "<!DOCTYPE html>", tHtml))
      , (Symbol "emptyHtml", (return $ VHtml $ HtmlEmitStr "", tHtml))
      , (Symbol "genUUID", (return (VAction fUUID), tIO tText))
      , (Symbol "randomBytes", (fn1 fRandomBytes, tInt --> tIO tText))
      , (Symbol "addCookie", (fn3 fAddCookie, tResponse --> tText --> tText --> tResponse))
      , (Symbol "cookie", (fn1 fCookie, tText --> tMaybe tText))
      , (Symbol "maybe", (fn3 fMaybe, tMaybe v1 --> v0 --> (v1 --> v0) --> v0))
      , (Symbol "bindMaybe", (fn2 fBindMaybe, tMaybe v0 --> (v0 --> tMaybe v1) --> tMaybe v1))
      , (Symbol "listHead", (fn1 fListHead, tList v0 --> tMaybe v0))
      , (Symbol "liftIO", (fn1 fIdentity, v0 --> tIO v0))
      ]

    infixr -->
    (-->) = tFn

    v0 = TVar 0
    v1 = TVar 1

    fn1 :: (Value -> Interpreter Value) -> EnvValue
    fn1 = return . VFn

    fn2 :: (Value -> Value -> Interpreter Value) -> EnvValue
    fn2 f = return $ VFn (\arg1 -> return (VFn (\arg2 -> f arg1 arg2)))

    fn3 :: (Value -> Value -> Value -> Interpreter Value) -> EnvValue
    fn3 f = return $ VFn (\arg1 -> return (VFn (\arg2 -> return (VFn (\arg3 -> f arg1 arg2 arg3)))))


fRedirect :: Value -> Interpreter Value
fRedirect locationValue =
  return $ VResponse (Response { responseStatus = HTTP.found302
                               , responseHeaders = [(HTTP.hLocation, T.encodeUtf8 location)]
                               , responseBody = ""
                               })
  where
    location =
      valueAsText locationValue


fFile :: Value -> Value -> Interpreter Value
fFile contentTypeValue filenameValue =
  liftIO (IO.catchIOError okRead errRead)

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


fNow :: EnvValue
fNow =
  VTimestamp <$> asks isTimestamp


fUUID :: Interpreter Value
fUUID =
  liftIO ((VText . UUID.toText) <$> UUID.nextRandom)


fRandomBytes :: Value -> Interpreter Value
fRandomBytes nValue = return $ VAction $ do
  bytes <- liftIO $ Entropy.getEntropy $ fromInteger $ valueAsInteger nValue
  return $ VText $ Base64.encodeBase64 bytes


fAddCookie :: Value -> Value -> Value -> Interpreter Value
fAddCookie responseValue nameValue valueValue =
  return $ VResponse $ response { responseHeaders = headers }

  where
    response =
      valueAsResponse responseValue

    cookie =
      T.encodeUtf8 (valueAsText nameValue <> "=" <> valueAsText valueValue <> "; Path=/;")

    headers =
      (HTTP.hSetCookie, cookie) : responseHeaders response


fCookie :: Value -> Interpreter Value
fCookie nameValue = do
  request <- asks isRequest
  return $ VMaybe $ VText <$> findCookie request (valueAsText nameValue)

  where
    findCookie :: Wai.Request -> T.Text -> Maybe T.Text
    findCookie request name =
      (snd <$> List.find (\(k, _) -> safeDecode k == Just name) (cookies request)) >>= safeDecode

    cookies :: Wai.Request -> Cookie.Cookies
    cookies request =
      concatMap (\(hName, hValue) -> if hName == HTTP.hCookie then Cookie.parseCookies hValue else []) (Wai.requestHeaders request)

    safeDecode :: BS.ByteString -> Maybe T.Text
    safeDecode bs =
      either (const Nothing) Just (T.decodeUtf8' bs)


fMaybe :: Value -> Value -> Value -> Interpreter Value
fMaybe maybeValue defaultValue fValue =
  case valueAsMaybe maybeValue of
    Just v -> (valueAsFn fValue) v
    Nothing -> return defaultValue


fBindMaybe :: Value -> Value -> Interpreter Value
fBindMaybe maybeValue fValue =
  case valueAsMaybe maybeValue of
    Just v -> (valueAsFn fValue) v
    Nothing -> return (VMaybe Nothing)


fListHead :: Value -> Interpreter Value
fListHead listValue =
  return $ VMaybe $ Maybe.listToMaybe $ valueAsList listValue


fIdentity :: Value -> Interpreter Value
fIdentity v =
  return v
