module Kuljet.Stage.TypeCheck
  ( Module
  , moduleEndpoints
  , moduleTables

    -- mimic the AST interface
  , Endpoint(..)
  , Norm.Exp(..)
  , Norm.Literal(..)
  , Norm.QOrder(..)
  , Norm.Annotated(..)
  , Table(..)
  , Norm.BinOp(..)
  
  , typeCheckModule
  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Maybe as Maybe
import RangedParsec (Located(..), SourceSpan)
import Network.HTTP.Types.Method (Method)
import qualified Network.HTTP.Types.Method as Method
import Control.Monad.Reader

import qualified Kuljet.Stage.Norm as Norm
import qualified Kuljet.SourceError as Error
import Kuljet.SourceError (Error(..))


-- Types common to all ASTs

import Kuljet.Symbol
import Kuljet.PathPattern
import Kuljet.Type


-- Typechecked AST

data Module =
  Module { moduleEndpoints :: [Endpoint]
         , moduleTables :: [Table]
         }
  deriving (Show)


data Endpoint =
  Serve { serveMethod :: Method
        , servePath :: Path
        , serveExp :: Located Norm.Exp
        , serveType :: Type
        }
  deriving (Show)


-- Tables

data Table =
  Table { tableName :: Symbol
        , tableFields :: [(Symbol, Type)]
        }
  deriving (Show)


tableRowType :: Table -> Type
tableRowType = TRecord . tableFields


-- Type predicates

data TypePred
  = PredPostResponse
  | PredResponse
  | PredExact Type
  | PredQuery
  | PredOrd
  | PredProject
  | PredCompare
  deriving (Eq)


-- TypeCheck Monad

data Env
  = Env { typeEnv :: M.Map Symbol Type
        , tableEnv :: M.Map Symbol Table
        }

type TypeCheck a = ReaderT Env (Either Error) a


locatedFail :: SourceSpan -> T.Text -> TypeCheck a
locatedFail loc msg =
  lift $ Left $ Error.mkError $ At loc msg


introduce :: Symbol -> Type -> TypeCheck a -> TypeCheck a
introduce name t =
  withReaderT (\env -> env { typeEnv = M.insert name t (typeEnv env) })


withTypeEnv :: (M.Map Symbol Type -> M.Map Symbol Type) -> TypeCheck a -> TypeCheck a
withTypeEnv f =
  withReaderT (\env -> env { typeEnv = f (typeEnv env) })


lookupType :: Symbol -> TypeCheck (Maybe Type)
lookupType name =
  M.lookup name <$> asks typeEnv


lookupTable :: Symbol -> TypeCheck (Maybe Table)
lookupTable name =
  M.lookup name <$> asks tableEnv


-- Type checking


isDbType :: Type -> Bool
isDbType t = t `elem` [tText, tInt, tTimestamp]


typeCheckModule :: M.Map Symbol Type -> Norm.Module -> Either Error Module
typeCheckModule stdEnv normalisedModule = do
  tables <- mapM typeCheckTable normTables
  flip Module tables <$> mapM (typeCheckDecl tables) (Norm.moduleEndpoints normalisedModule)

  where
    normTables =
      Norm.moduleTables normalisedModule
      
    typeCheckDecl tables (Norm.Serve { Norm.serveMethod, Norm.servePath, Norm.serveExp }) = do
      serveType <- runReaderT (typeCheck (declType serveMethod) serveExp) (initialEnv tables servePath)
      return $ Serve { serveMethod, servePath, serveExp, serveType }

    typeCheckTable :: Norm.Table -> Either Error Table
    typeCheckTable table =
      case Norm.tableType table of
        At tSpan (TRecord fields) -> do
          mapM_ (typeCheckField tSpan) (map snd fields)
          return $ Table (Norm.tableName table) fields
        At typeSpan _ ->
          Left $ Error.mkError $ At typeSpan "Tables should be a record type"

    typeCheckField :: SourceSpan -> Type -> Either Error ()
    typeCheckField tSpan t =
      if isDbType t
      then return ()
      else Left $ Error.mkError $ At tSpan "Only 'text' and 'int' fields are supported for tables"

    initialEnv :: [Table] -> Path -> Env
    initialEnv tables servePath =
      Env { typeEnv = (<>) stdEnv $
                      M.fromList $
                      map (\table -> (tableName table, tQuery (tableRowType table))) tables ++
                      map (\pathVar -> (pathVar, tText)) (pathVars servePath)
          , tableEnv = M.fromList (map (\t -> (tableName t, t)) tables)
          }

    declType serveMethod =
      if serveMethod == Method.methodPost
      then PredPostResponse
      else PredResponse


typeCheck :: TypePred -> Located Norm.Exp -> TypeCheck Type
typeCheck p (At eSpan e) = do
  eType <- infer e
  case eType of
    Just t ->
      if applyPred t
      then return t
      else locatedFail eSpan ("Expression is " <> typeName t <> ", but expected " <> predExpected)
      
    Nothing ->
      case e of
        Norm.ExpYield queryExp yielder -> do
          queryType <- typeCheck PredQuery queryExp
          case queryType of
            TCons "query" [TRecord fields] ->
              if isHtmlPred
              then do
                _ <- withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck (PredExact tHtml) yielder)
                return tHtml
              else case p of
                PredExact (TCons "list" [elemT]) ->
                  tList <$> withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck (PredExact elemT) yielder)

                _ ->
                  locatedFail eSpan ("Expression is a yield, but expected " <> predExpected)
              
            _ ->
              locatedFail (locatedSpan queryExp) "Expecting a query"

        Norm.ExpAbs sym body ->
          case p of
            PredExact (TCons "->" [argT, retT]) -> do
              retT' <- introduce (Norm.discardAnnotation sym) argT (typeCheck (PredExact retT) body)
              return (tFn argT retT')

            PredPostResponse ->
              case sym of
                Norm.Annotated argName (Just argT) -> do
                  retT <- introduce argName argT (typeCheck PredResponse body)
                  return (tFn argT retT)

                _ ->
                  locatedFail eSpan "Type annotation required"
              
              
            _ ->
              locatedFail eSpan ("Expression is a function, but expected " <> predExpected)
          
        Norm.ExpList exps ->
          if isHtmlPred
          then do
            mapM_ (typeCheck (PredExact tHtml)) exps
            return (tList tHtml)
          else case p of
            PredExact (TCons "list" [t]) -> do
              mapM_ (typeCheck (PredExact t)) exps
              return (tList t)

            _ ->
              locatedFail eSpan ("Expression is a list, but expected " <> predExpected)

        Norm.ExpIf a b c -> do
          _ <- typeCheck (PredExact tBool) a
          if isHtmlPred
            then do
              _ <- typeCheck (PredExact tHtml) b
              _ <- typeCheck (PredExact tHtml) c
              return tHtml

            else case p of
              PredExact t -> do
                _ <- typeCheck (PredExact t) b
                _ <- typeCheck (PredExact t) c
                return t
  
              _ ->
                locatedFail eSpan ("Expected both 'if' branches to be " <> predExpected)

        _ ->
          locatedFail eSpan "Cannot infer type"

  where
    isHtml t =
      t `elem` [tHtmlTag, tHtmlTagWithAttrs, tHtml, tText, tInt]

    isHtmlPred =
      p `elem` [PredExact tHtml, PredExact tHtmlTagArg, PredExact tResponse, PredResponse, PredPostResponse]

    isAttributes =
      \case
        TRecord fields -> all ((tText ==) . snd) fields
        _ -> False

    isPostFun =
      \case
        TCons "->" [TRecord fields, ret] -> all ((tText ==) . snd) fields && isResponseSubtype ret
        _ -> False

    isResponseSubtype =
      \case
        TCons "io" [t] -> t == tResponse || isHtml t
        t -> t == tResponse || isHtml t

    isQuery =
      \case
        TCons "query" _ -> True
        _ -> False

    isProject =
      \case
        TRecord fields -> all isDbType (map snd fields)
        _ -> False

    applyPred t =
      case p of
        PredExact (TCons "html" []) -> isHtml t
        PredExact (TCons "htmlTagArg" []) -> isHtml t || isAttributes t
        PredExact t' -> Maybe.isJust (matchType t' t)
        PredPostResponse -> isPostFun t || isResponseSubtype t
        PredResponse -> isResponseSubtype t
        PredQuery -> isQuery t
        PredOrd -> t `elem` [tInt, tText, tTimestamp, tBool]
        PredProject -> isProject t
        PredCompare -> t `elem` [tInt, tText, tTimestamp, tBool] -- TODO records, lists
        
    predExpected =
      case p of
        PredPostResponse -> "html or POST function"
        PredResponse -> "response"
        PredExact t -> typeName t
        PredQuery -> "a query"
        PredOrd -> "an orderable value"
        PredCompare -> "a comparable value"
        PredProject -> "a projection"


infer :: Norm.Exp -> TypeCheck (Maybe Type)
infer =
  \case
    Norm.ExpLiteral lit ->
      let t =
            case lit of
              Norm.LitStr _ -> tText
              Norm.LitInt _ -> tInt
      in
      return (Just t)

    Norm.ExpVar (At varSpan var) -> do
      t <- lookupType var
      case t of
        Just t' ->
          return (Just t')

        Nothing ->
          locatedFail varSpan ("Unrecognised variable '" <> symbolName var <> "'")

    Norm.ExpApp f arg -> do
      fType <- infer (discardLocation f)
      case fType of
        Just (TCons "htmlTag" []) -> do
          argType <- typeCheck (PredExact tHtmlTagArg) arg
          case argType of
            TRecord _ -> return (Just tHtmlTagWithAttrs)
            _ -> return (Just tHtml)

        Just (TCons "htmlTagWithAttrs" []) -> do
          _ <- typeCheck (PredExact tHtml) arg
          return (Just tHtml)

        Just (TCons "->" [argT, retT]) -> do
          argT' <- typeCheck (PredExact argT) arg
          case matchType argT argT' of
            Just subst -> return (Just (applySubst subst retT))
            Nothing -> locatedFail (locatedSpan arg) ("Expression is " <> typeName argT' <> ", but expecting " <> typeName argT)

        Just t ->
          locatedFail (locatedSpan f) ("Expression has type '" <> typeName t <> "', but a function is expected")

        Nothing ->
          return Nothing

    Norm.ExpAbs (Norm.Annotated sym (Just t)) body ->
      (fmap (tFn t)) <$> introduce sym t (infer (discardLocation body))

    Norm.ExpThen var a b ->
      infer (discardLocation a) >>= \case
      Just (TCons "io" [varT]) -> maybe (infer (discardLocation b)) (\sym -> introduce sym varT (infer (discardLocation b))) var >>= \case
        t@(Just (TCons "io" _)) -> return t
        Just t -> return (Just (tIO t))
        Nothing -> return Nothing
      Just t -> locatedFail (locatedSpan a) ("Expression has type '" <> typeName t <> "', but an 'io' action is expected")
      Nothing -> return Nothing

    Norm.ExpRecord fields ->
      (fmap TRecord) <$> inferFields [] fields

      where
        inferFields :: [(Symbol, Type)] -> [(Symbol, Located Norm.Exp)] -> TypeCheck (Maybe [(Symbol, Type)])
        inferFields accum =
          \case
            [] ->
              return (Just (reverse accum))
            ((sym, At _ e) : fs) -> do
              t <- infer e
              case t of
                Just t' -> inferFields ((sym, t') : accum) fs
                Nothing -> return Nothing

    Norm.ExpDot (At rSpan r) (At fieldSpan fieldName) -> do
      rType <- infer r
      case rType of
        Just (TRecord fields) ->
          case lookup fieldName fields of
            Just fieldT -> return (Just fieldT)
            Nothing -> locatedFail fieldSpan ("Record does not contain field '" <> symbolName fieldName <> "'")

        Just _ ->
          locatedFail rSpan "Expecting a record"

        Nothing ->
          return Nothing

    Norm.ExpBinOp op a b -> do
      aType <- typeCheck opPred a
      _ <- typeCheck (PredExact aType) b
      return (Just opRetT)

      where
        (opPred, opRetT) =
          case op of
            Norm.OpEq -> (PredCompare, tBool)
            Norm.OpLt -> (PredOrd, tBool)
            Norm.OpGt -> (PredOrd, tBool)
            Norm.OpLtEq -> (PredOrd, tBool)
            Norm.OpGtEq -> (PredOrd, tBool)
            Norm.OpPlus -> (PredExact tInt, tInt)
            Norm.OpMinus -> (PredExact tInt, tInt)
            Norm.OpMul -> (PredExact tInt, tInt)
            Norm.OpDiv -> (PredExact tInt, tInt)
            Norm.OpAnd -> (PredExact tBool, tBool)
            Norm.OpOr -> (PredExact tBool, tBool)
            Norm.OpConcat -> (PredExact tText, tText)

    Norm.ExpInsert (At tableSpan tableName) value ->
      lookupTable tableName >>= \case
      Just table -> do
        _ <- typeCheck (PredExact (tableRowType table)) value
        return (Just (tIO tUnit))
      Nothing ->
        locatedFail tableSpan "Unknown table"

    Norm.ExpQLimit queryExp limitExp -> do
      queryType <- typeCheck PredQuery queryExp
      _ <- typeCheck (PredExact tInt) limitExp
      return (Just queryType)

    Norm.ExpQWhere queryExp whereExp -> do
      queryType <- typeCheck PredQuery queryExp
      case queryType of
        TCons "query" [TRecord fields] -> do
          _ <- withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck (PredExact tBool) whereExp)
          return (Just queryType)

        _ ->
          locatedFail (locatedSpan queryExp) "Expecting a query"

    Norm.ExpQNatJoin a b -> do
      a' <- typeCheck PredQuery a >>= asFields (locatedSpan a)
      b' <- typeCheck PredQuery b >>= asFields (locatedSpan b)
      let fieldsIntersect = M.intersectionWith (,) (M.fromList a') (M.fromList b')
          fieldsUnion = a' ++ filter (\(key, _) -> key `notElem` map fst a') b'
      if M.null fieldsIntersect
        then locatedFail (locatedSpan b) "Right query does not have fields in command with left query"
        else do
        mapM_ checkFieldType (M.toList fieldsIntersect)
        return $ Just $ tQuery $ TRecord fieldsUnion

      where
        asFields loc =
          \case
            TCons "query" [TRecord fields] ->
              return fields

            _ ->
              locatedFail loc "Expecting a query"

        checkFieldType (Symbol key, (t1, t2)) =
          if t1 == t2
          then return ()
          else locatedFail (locatedSpan b) $
               "Right query has conflicting type for field '" <> key <> "': " <> typeName t1 <> " vs. " <> typeName t2

    Norm.ExpQSelect queryExp selectExp -> do
      queryType <- typeCheck PredQuery queryExp
      case queryType of
        TCons "query" [TRecord fields] ->
          (Just . TCons "query" . return) <$> withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck PredProject selectExp)

        _ ->
          locatedFail (locatedSpan queryExp) "Expecting a query"

    Norm.ExpQOrder queryExp orderExp _ -> do
      queryType <- typeCheck PredQuery queryExp
      case queryType of
        TCons "query" [TRecord fields] -> do
          _ <- withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck PredOrd orderExp)
          return (Just queryType)
              
        _ ->
          locatedFail (locatedSpan queryExp) "Expecting a query"

    _ ->
      return Nothing
