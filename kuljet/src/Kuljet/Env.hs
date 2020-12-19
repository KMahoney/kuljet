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
import qualified Data.Time.Clock as Time
import Control.Monad.Reader
import qualified Web.Cookie as Cookie
import qualified System.Entropy as Entropy
import qualified Data.ByteString.Base64 as Base64
import qualified Crypto.KDF.BCrypt as Password
import qualified Text.Regex.PCRE as RE
import Text.Regex.PCRE.Text()
import qualified CMark

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
      , (Symbol "relativeTime", (fn1 fRelativeTime, tTimestamp --> tText))
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
      , (Symbol "hashPassword", (fn1 fHashPassword, tText --> tPassword))
      , (Symbol "validatePassword", (fn2 fValidatePassword, tText --> tPassword --> tBool))
      , (Symbol "textLength", (fn1 fTextLength, tText --> tInt))
      , (Symbol "regexpMatch", (fn2 fRegexpMatch, tText --> tText --> tBool))
      , (Symbol "true", (return (VBool True), tBool))
      , (Symbol "false", (return (VBool False), tBool))
      , (Symbol "not", (fn1 fNot, tBool --> tBool))
      , (Symbol "commonMark", (fn1 fCommonMark, tText --> tHtml))
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


fRelativeTime :: Value -> Interpreter Value
fRelativeTime timestampValue = do
  ts <- asks isTimestamp
  return $ VText $ timeDiff ts (valueAsTimestamp timestampValue)

  where
    timeDiff :: Time.UTCTime -> Time.UTCTime -> T.Text
    timeDiff now ts =
      let secs = Time.diffUTCTime now ts
          minute = 60
          hour = minute * 60
          day = hour * 24
          num = T.pack . show . (floor :: RealFrac a => a -> Integer)
      in
        if secs < minute * 2
        then "a few seconds ago"
        else if secs < hour
        then num (secs / minute) <> " minutes ago"
        else if secs < 30 * hour
        then num (secs / hour) <> " hours ago"
        else num (secs / day) <> " days ago"


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


fHashPassword :: Value -> Interpreter Value
fHashPassword passValue = do
  cost <- asks isHashCost
  (VText . T.decodeUtf8) <$> liftIO (Password.hashPassword cost (valueAsBS passValue))


fValidatePassword :: Value -> Value -> Interpreter Value
fValidatePassword passValue hashValue =
  return $ VBool $ Password.validatePassword (valueAsBS passValue) (valueAsBS hashValue)


fTextLength :: Value -> Interpreter Value
fTextLength textValue =
  return $ VInt $ toInteger $ T.length $ valueAsText textValue


fRegexpMatch :: Value -> Value -> Interpreter Value
fRegexpMatch regexpValue textValue = do
  regexp <- liftIO (RE.makeRegexM (valueAsText regexpValue)) :: Interpreter RE.Regex
  return $ VBool $ RE.matchTest regexp (valueAsText textValue)


fNot :: Value -> Interpreter Value
fNot boolValue =
  return $ VBool $ not $ valueAsBool boolValue


fCommonMark :: Value -> Interpreter Value
fCommonMark textValue =
  return $ VHtml $ HtmlEmitStr $ CMark.commonmarkToHtml [CMark.optSafe, CMark.optSmart] $ valueAsText textValue
