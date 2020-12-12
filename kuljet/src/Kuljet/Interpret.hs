module Kuljet.Interpret where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as Time
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Method as Method
import Network.HTTP.Types.Method (Method)
import RangedParsec (Located(..))
import Control.Monad.Reader
import Data.Functor.Identity (runIdentity)

import qualified Database.SQLite3 as DB

import qualified Kuljet.Stage.CompileSql as AST
import qualified Kuljet.Query as Query
import qualified Database.QueryBuilder as Query
import qualified Database.Sql as Query

import Kuljet.Symbol
import qualified Kuljet.PathPattern as PathPattern
import Kuljet.Type
import Kuljet.Value
import Kuljet.InterpreterType


data InterpretedRoute
  = InterpretedRoute
    { routeMethod :: Method
    , routePath :: PathPattern.Path
    , routeRun :: DB.Database -> PathPattern.PathVars -> Wai.Application
    }


moduleInterpreter :: Env -> AST.Module -> [InterpretedRoute]
moduleInterpreter stdEnv typecheckedModule =
  map (interpretEndpoint stdEnv (AST.moduleTables typecheckedModule)) (AST.moduleEndpoints typecheckedModule)


interpretEndpoint :: Env -> [AST.Table] -> AST.Endpoint -> InterpretedRoute
interpretEndpoint stdEnv tables (AST.Serve {AST.serveMethod, AST.servePath, AST.serveExp, AST.serveType}) =
    InterpretedRoute { routeMethod = serveMethod
                     , routePath = servePath
                     , routeRun = interpretServeBody stdEnv serveMethod serveType tables (discardLocation serveExp)
                     }


type Env
  = M.Map Symbol (Reader InterpreterState Value)


typeCheckFailure :: a
typeCheckFailure =
  error "The type checker has failed - this is a bug."


interpretServeBody :: Env -> Method -> Type -> [AST.Table] -> AST.Exp -> DB.Database -> PathPattern.PathVars -> Wai.Application
interpretServeBody stdEnv method bodyType tables body db pathVars request respond = do
  state <- interpreterState
  value <- runReaderT (interpret initialEnv body) state
  if method == Method.methodPost
    then do
      case bodyType of
        TCons "->" [TRecord fields, _] -> do
          (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd request
          case paramsToRecord fields params of
            Right r -> do
              value' <- runReaderT (valueAsFn value r) state
              sendResponse (valueToResponse value')
  
            Left err ->
              respond $ Wai.responseLBS HTTP.badRequest400 [] ("Invalid form input: " <> err)
                  
        _ ->
          sendResponse (valueToResponse value)

    else
      sendResponse (valueToResponse value)

  where
    interpreterState :: IO InterpreterState
    interpreterState =
      InterpreterState db request <$> Time.getCurrentTime

    pathVarEnv :: Env
    pathVarEnv =
      M.map (return . VText) pathVars

    tableEnv :: Env
    tableEnv =
      M.fromList (map (\table -> (AST.tableName table, return $ VQuery $ tableQuery table)) tables)

    tableQuery table =
      Query.queryTable
      (symbolName (AST.tableName table))
      (map (\(Symbol name, _) -> name) (AST.tableFields table))

    initialEnv =
      stdEnv <> tableEnv <> pathVarEnv

    valueToResponse =
      \case
        VResponse response ->
          response
        value ->
          Response { responseStatus = HTTP.ok200
                   , responseHeaders = []
                   , responseBody = valueToBody value
                   }

    sendResponse (Response { responseStatus, responseHeaders, responseBody }) =
      respond $ Wai.responseLBS responseStatus responseHeaders responseBody

    toLBS =
      LBS.fromStrict . T.encodeUtf8

    valueToBody =
      toLBS . valueHtmlEmit

    paramsToRecord :: [(Symbol, Type)] -> [Wai.Param] -> Either LBS.ByteString Value
    paramsToRecord fields params =
      let valueMap = M.fromList (map encodeParam params) in
        VRecord . M.fromList <$>
        mapM (\(key, _) ->
                 Maybe.maybe
                   (Left ("missing '" <> LBS.fromStrict (T.encodeUtf8 (symbolName key)) <> "'"))
                   (Right . (key,))
                   (M.lookup key valueMap)) fields

    encodeParam :: Wai.Param -> (Symbol, Value)
    encodeParam (name, value) =
      (Symbol (T.decodeUtf8 name), VText (T.decodeUtf8 value))


htmlEmit :: HtmlEmitter -> T.Text
htmlEmit (HtmlEmitStr str) = str
htmlEmit (HtmlEmitTag tag) = "<" <> tag <> "></" <> tag <> ">"
htmlEmit (HtmlEmitTagWithAttrs tag attrs) = "<" <> tag <> " " <> htmlEmitAttrs attrs <> "></" <> tag <> ">"


htmlEmitAttrs :: M.Map Symbol Value -> T.Text
htmlEmitAttrs attrs =
  T.intercalate " " (map (\(Symbol attr, value) -> attr <> "=\"" <> valueAsText value <> "\"") (M.toList attrs))


valueHtmlEmit :: Value -> T.Text
valueHtmlEmit =
  \case
    VHtml emit -> htmlEmit emit
    VText t -> htmlEscape t
    VInt i -> T.pack (show i)
    VList values -> mconcat (map valueHtmlEmit values)
    _ -> typeCheckFailure

  where
    htmlEscape s =
      case T.uncons s of
        Just (ch, s') ->
          case M.lookup ch htmlEntities of
            Just entity -> entity <> htmlEscape s'
            Nothing -> T.cons ch (htmlEscape s')
        Nothing -> T.empty

    htmlEntities =
      M.fromList [('&', "&amp;"), ('<', "&lt;"), ('>', "&gt;"), ('"', "&quot;")]


interpret :: Env -> AST.Exp -> Interpreter Value
interpret env =
  \case
    AST.ExpLiteral lit ->
      case lit of
        AST.LitStr str -> return (VText str)
        AST.LitInt i -> return (VInt i)

    AST.ExpVar (At _ var) ->
      case M.lookup var env of
        Just value -> mapReaderT (return . runIdentity) value
        Nothing -> error ("Type check error - missing '" <> T.unpack (symbolName var) <> "'")

    AST.ExpApp (At _ fExp) (At _ argExp) -> do
      fExp' <- interpret env fExp
      argExp' <- interpret env argExp
      case fExp' of
        VHtml (HtmlEmitTag tag) ->
          case argExp' of
            VRecord attrs ->
              return $ VHtml $ HtmlEmitTagWithAttrs tag attrs

            value ->
              let body = valueHtmlEmit value in
                return $ VHtml $ HtmlEmitStr $ "<" <> tag <> ">" <> body <> "</" <> tag <> ">"

        VHtml (HtmlEmitTagWithAttrs tag attrs) ->
          let
            body = valueHtmlEmit argExp'
            tagAttrs = htmlEmitAttrs attrs
          in
            return $ VHtml $ HtmlEmitStr $ "<" <> tag <> " " <> tagAttrs <> ">" <> body <> "</" <> tag <> ">"

        VFn f ->
          f argExp'
              
        _ ->
          typeCheckFailure

    AST.ExpAbs (AST.Annotated { AST.discardAnnotation = argName }) (At _ bodyExp) ->
      return $ VFn (\value -> interpret (M.insert argName (return value) env) bodyExp)

    AST.ExpThen sym (At _ ioExp) (At _ bodyExp) -> do
      ioValue <- interpret env ioExp
      value <- valueAsAction ioValue
      interpret (maybe env (\s -> M.insert s (return value) env) sym) bodyExp

    AST.ExpList exps ->
      VList <$> mapM (interpret env . discardLocation) exps

    AST.ExpRecord fields ->
      VRecord . M.fromList <$> mapM (\(key, value) -> (key,) <$> interpret env (discardLocation value)) fields
        
    AST.ExpDot (At _ r) (At _ fieldName) -> do
      fields <- valueAsRecord <$> interpret env r
      case M.lookup fieldName fields of
        Just value -> return value
        Nothing -> typeCheckFailure

    AST.ExpBinOp op a b -> do
      a' <- interpret env (discardLocation a)
      b' <- interpret env (discardLocation b)
      case op of
        AST.OpEq -> return $ VBool $ partialEq a' b'
        AST.OpPlus -> return $ VInt $ valueAsInteger a' + valueAsInteger b'
        AST.OpMinus -> return $ VInt $ valueAsInteger a' - valueAsInteger b'
        AST.OpMul -> return $ VInt $ valueAsInteger a' * valueAsInteger b'
        AST.OpDiv -> return $ VInt $ valueAsInteger a' `div` valueAsInteger b'
        AST.OpLt -> return $ VBool $ partialCompare a' b' == LT
        AST.OpGt -> return $ VBool $ partialCompare a' b' == GT
        AST.OpLtEq -> return $ VBool $ partialCompare a' b' /= GT
        AST.OpGtEq -> return $ VBool $ partialCompare a' b' /= LT
        AST.OpAnd -> return $ VBool $ valueAsBool a' && valueAsBool b'
        AST.OpOr -> return $ VBool $ valueAsBool a' || valueAsBool b'
        AST.OpConcat -> return $ VText $ valueAsText a' <> valueAsText b'

    AST.ExpIf a b c -> do
      a' <- interpret env (discardLocation a)
      if valueAsBool a'
        then interpret env (discardLocation b)
        else interpret env (discardLocation c)
      
    AST.ExpYield (query, queryArgs) yieldExp -> do
      db <- asks isDatabase
      queryArgs' <- mapM (interpret env) queryArgs
      result <- liftIO (Query.execute db query queryArgs')
      VList <$> mapM (\row -> interpret (M.union (rowToEnv row) env) (discardLocation yieldExp)) result

      where
        rowToEnv :: [(T.Text, DB.SQLData)] -> Env
        rowToEnv =
          M.fromList . map (\(name, value) -> (Symbol name, return $ dbValue value))

        -- FIXME: decode based on expected type
        dbValue :: DB.SQLData -> Value
        dbValue =
          \case
            DB.SQLText text -> VText text
            DB.SQLInteger i -> VInt (toInteger i)
            _ -> undefined -- TODO

    AST.ExpInsert (At _ tableName) (At _ value) -> do
      value' <- interpret env value
      return $ VAction $ insert tableName $ M.toList $ valueAsRecord value'

      where
        insert :: Symbol -> [(Symbol, Value)] -> Interpreter Value
        insert (Symbol name) fields = do
          db <- asks isDatabase
          liftIO $ do
            stmt <- DB.prepare db sql
            DB.bindNamed stmt fieldValues
            _ <- DB.step stmt
            return VUnit

          where
            sql =
              "INSERT INTO " <> name <> "(" <> fieldNames <> ") VALUES (" <> fieldParams <> ")"
              
            fieldNames =
              T.intercalate "," (map (symbolName . fst) fields)

            fieldParams =
              T.intercalate "," (map ((":" <>) . symbolName . fst) fields)

            fieldValues =
              map (\(key, fieldValue) -> (":" <> symbolName key, Query.valueToSql fieldValue)) fields

    AST.ExpDelete (At _ tableName) (queryExp, queryArgs) -> do
      queryArgs' <- mapM (interpret env) queryArgs
      return $ VAction $ delete tableName queryArgs'

      where
        delete :: Symbol -> M.Map Integer Value -> Interpreter Value
        delete (Symbol name) args = do
          db <- asks isDatabase
          liftIO $ do
            stmt <- DB.prepare db (Query.toText sql)
            DB.bindNamed stmt sqlArgs
            _ <- DB.step stmt
            return VUnit

          where
            sql :: Query.Sql
            sql =
              "DELETE FROM " <> Query.quoteName name <> " WHERE " <> Query.buildSqLiteExpression queryExp

            sqlArgs =
              map (\(k, v) -> ("?" <> T.pack (show k), Query.valueToSql v)) (M.toList args)
