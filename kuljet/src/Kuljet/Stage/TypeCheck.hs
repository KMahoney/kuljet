module Kuljet.Stage.TypeCheck
  ( Module
  , moduleEndpoints
  , moduleTables

    -- mimic the AST interface
  , Endpoint(..)
  , Exp(..)
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
        , serveExp :: Located Exp
        , serveType :: Type
        }
  deriving (Show)

data Exp
  = ExpVar (Located Symbol)
  | ExpLiteral Norm.Literal
  | ExpApp (Located Exp) (Located Exp)
  | ExpAbs (Norm.Annotated Symbol) (Located Exp)
  | ExpThen (Maybe Symbol) (Located Exp) (Located Exp)
  | ExpList [Located Exp]
  | ExpRecord [(Symbol, Located Exp)]
  | ExpTagF Symbol
  | ExpAnnotated (Located Exp) Type
  | ExpDot (Located Exp) (Located Symbol)
  | ExpInsert (Located Symbol) (Located Exp)
  | ExpDelete (Located Symbol) (Located Exp)
  | ExpYield (Located Exp) (Located Exp) (M.Map Symbol Type)
  | ExpQLimit (Located Exp) (Located Exp)
  | ExpQOrder (Located Exp) (Located Exp) Norm.QOrder
  | ExpQSelect (Located Exp) (Located Exp)
  | ExpQWhere (Located Exp) (Located Exp)
  | ExpQNatJoin (Located Exp) (Located Exp)
  | ExpBinOp Norm.BinOp (Located Exp) (Located Exp)
  | ExpIf (Located Exp) (Located Exp) (Located Exp)
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
isDbType t = t `elem` [tText, tInt, tTimestamp, tPassword]


typeCheckModule :: M.Map Symbol Type -> Norm.Module -> Either Error Module
typeCheckModule stdEnv normalisedModule = do
  tables <- mapM typeCheckTable normTables
  flip Module tables <$> mapM (typeCheckDecl tables) (Norm.moduleEndpoints normalisedModule)

  where
    normTables =
      Norm.moduleTables normalisedModule
      
    typeCheckDecl tables (Norm.Serve { Norm.serveMethod, Norm.servePath, Norm.serveExp }) = do
      (typedExp, serveType) <- runReaderT (typeCheck (declType serveMethod) serveExp) (initialEnv tables servePath)
      return $ Serve { serveMethod, servePath, serveExp = typedExp, serveType }

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


typeCheck :: TypePred -> Located Norm.Exp -> TypeCheck (Located Exp, Type)
typeCheck p (At eSpan e) = do
  inferred <- infer e
  case inferred of
    Just (e', t) ->
      if applyPred t
      then return (At eSpan e', t)
      else locatedFail eSpan ("Expression is " <> typeName t <> ", but expected " <> predExpected)
      
    Nothing ->
      case e of
        Norm.ExpYield queryExp yielder -> do
          (queryExp', queryType) <- typeCheck PredQuery queryExp
          case queryType of
            TCons "query" [TRecord fields] ->
              if isHtmlPred
              then do
                (yielder', _) <- withTypeEnv (\env -> M.union (M.fromList fields) env) (typeCheck (PredExact tHtml) yielder)
                return (At eSpan (ExpYield queryExp' yielder' (M.fromList fields)), tHtml)
              else case p of
                PredExact (TCons "list" [elemT]) -> do
                  (yielder', yieldT) <- withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck (PredExact elemT) yielder)
                  return (At eSpan (ExpYield queryExp' yielder' (M.fromList fields)), tList yieldT)

                _ ->
                  locatedFail eSpan ("Expression is a yield, but expected " <> predExpected)
              
            _ ->
              locatedFail (locatedSpan queryExp) "Expecting a query"

        Norm.ExpAbs sym body ->
          case p of
            PredExact (TCons "->" [argT, retT]) -> do
              (body', retT') <- introduce (Norm.discardAnnotation sym) argT (typeCheck (PredExact retT) body)
              return (At eSpan (ExpAbs sym body'), tFn argT retT')

            PredPostResponse ->
              case sym of
                Norm.Annotated argName (Just argT) -> do
                  (body', retT) <- introduce argName argT (typeCheck PredResponse body)
                  return (At eSpan (ExpAbs sym body'), tFn argT retT)

                _ ->
                  locatedFail eSpan "Type annotation required"
              
              
            _ ->
              locatedFail eSpan ("Expression is a function, but expected " <> predExpected)
          
        Norm.ExpList exps ->
          if isHtmlPred
          then do
            exps' <- map fst <$> mapM (typeCheck (PredExact tHtml)) exps
            return (At eSpan (ExpList exps'), tList tHtml)
          else case p of
            PredExact (TCons "list" [t]) -> do
              exps' <- map fst <$> mapM (typeCheck (PredExact t)) exps
              return (At eSpan (ExpList exps'), tList t)

            _ ->
              locatedFail eSpan ("Expression is a list, but expected " <> predExpected)

        Norm.ExpIf a b c -> do
          (a', _) <- typeCheck (PredExact tBool) a
          if isStrictHtmlPred
            then do
              (b', _) <- typeCheck (PredExact tHtml) b
              (c', _) <- typeCheck (PredExact tHtml) c
              return (At eSpan (ExpIf a' b' c'), tHtml)

            else do
              (b', t1) <- typeCheck p b
              (c', t2) <- typeCheck p c
              if t1 == t2
                then return (At eSpan (ExpIf a' b' c'), t1)
                else locatedFail eSpan ("Expected both 'if' branches to match, but got " <> typeName t1 <> " and " <> typeName t2)

        _ ->
          locatedFail eSpan "Cannot infer type"

  where
    isHtml t =
      t `elem` [tHtmlTag, tHtmlTagWithAttrs, tHtml, tText, tInt]
      || case t of
           TCons "list" [t'] -> isHtml t'
           _ -> False

    isStrictHtmlPred =
      p `elem` [PredExact tHtml, PredExact tHtmlTagArg]

    isHtmlPred =
      isStrictHtmlPred || p `elem` [PredExact tResponse, PredResponse, PredPostResponse]

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
        PredExact (TCons "response" []) -> isHtml t || t == tResponse
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


infer :: Norm.Exp -> TypeCheck (Maybe (Exp, Type))
infer =
  \case
    Norm.ExpLiteral lit ->
      let t =
            case lit of
              Norm.LitStr _ -> tText
              Norm.LitInt _ -> tInt
      in
      return (Just (ExpLiteral lit, t))

    Norm.ExpVar (At varSpan var) -> do
      t <- lookupType var
      case t of
        Just t' ->
          return (Just (ExpVar (At varSpan var), t'))

        Nothing ->
          locatedFail varSpan ("Unrecognised variable '" <> symbolName var <> "'")

    Norm.ExpTagF sym -> do
      return (Just (ExpTagF sym, tHtmlTag))

    Norm.ExpApp (At fSpan f) arg ->
      infer f >>= \case
      Just (f', TCons "htmlTag" []) -> do
        (arg', argType) <- typeCheck (PredExact tHtmlTagArg) arg
        case argType of
          TRecord _ -> return (Just (ExpApp (At fSpan f') arg', tHtmlTagWithAttrs))
          _ -> return (Just (ExpApp (At fSpan f') arg', tHtml))

      Just (f', TCons "htmlTagWithAttrs" []) -> do
        (arg', _) <- typeCheck (PredExact tHtml) arg
        return (Just (ExpApp (At fSpan f') arg', tHtml))

      Just (f', TCons "->" [argT, retT]) -> do
        (arg', argT') <- typeCheck (PredExact argT) arg
        case matchType argT argT' of
          Just subst -> return (Just (ExpApp (At fSpan f') arg', applySubst subst retT))
          Nothing -> locatedFail (locatedSpan arg) ("Expression is " <> typeName argT' <> ", but expecting " <> typeName argT)

      Just (_, t) ->
        locatedFail fSpan ("Expression has type '" <> typeName t <> "', but a function is expected")

      Nothing ->
        return Nothing

    Norm.ExpAbs arg@(Norm.Annotated sym (Just argT)) (At bodySpan body) -> do
      introduce sym argT (infer body) >>= \case
        Just (body', bodyT) -> return (Just (ExpAbs arg (At bodySpan body'), tFn argT bodyT))
        Nothing -> return Nothing

    Norm.ExpThen var (At aSpan a) (At bSpan b) ->
      infer a >>= \case
      Just (a', TCons "io" [varT]) ->
        maybe (infer b) (\sym -> introduce sym varT (infer b)) var >>=
        \case
          Just (b', t@(TCons "io" _)) ->
            return (Just (ExpThen var (At aSpan a') (At bSpan b'), t))

          Just (b', t) ->
            -- Lift the type into 'io' if needed
            return (Just (ExpThen var (At aSpan a') (At bSpan b'), tIO t))

          Nothing ->
            return Nothing

      Just (_, t) ->
        locatedFail aSpan ("Expression has type '" <> typeName t <> "', but an 'io' action is expected")

      Nothing ->
        return Nothing

    Norm.ExpAnnotated e t -> do
      (At _ e', _) <- typeCheck (PredExact t) e
      return (Just (e', t))

    Norm.ExpRecord fields -> do
      fmap (\fields' -> ( ExpRecord (map (\(s, (e, _)) -> (s, e)) fields')
                        , TRecord (map (\(s, (_, t)) -> (s, t)) fields')
                        ))
        <$> inferFields [] fields

      where
        inferFields :: [(Symbol, (Located Exp, Type))] -> [(Symbol, Located Norm.Exp)] -> TypeCheck (Maybe [(Symbol, (Located Exp, Type))])
        inferFields accum =
          \case
            [] ->
              return (Just (reverse accum))
            ((sym, At eSpan e) : fs) ->
              infer e >>=
              \case
                Just (e', t) -> inferFields ((sym, (At eSpan e', t)) : accum) fs
                Nothing -> return Nothing

    Norm.ExpDot (At recordSpan record) field@(At fieldSpan fieldName) ->
      infer record >>=
      \case
        Just (record', TRecord fields) ->
          case lookup fieldName fields of
            Just fieldT -> return (Just (ExpDot (At recordSpan record') field, fieldT))
            Nothing -> locatedFail fieldSpan ("Record does not contain field '" <> symbolName fieldName <> "'")

        Just _ ->
          locatedFail recordSpan "Expecting a record"

        Nothing ->
          return Nothing

    Norm.ExpBinOp op a b -> do
      (a', aType) <- typeCheck opPred a
      (b', _) <- typeCheck (PredExact aType) b
      return (Just (ExpBinOp op a' b', opRetT))

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

    Norm.ExpInsert locTable@(At tableSpan tableName) value ->
      lookupTable tableName >>= \case
      Just table -> do
        (value', _) <- typeCheck (PredExact (tableRowType table)) value
        return (Just (ExpInsert locTable value', tIO tUnit))
      Nothing ->
        locatedFail tableSpan "Unknown table"

    Norm.ExpDelete locTable@(At tableSpan tableName) whereExp ->
      lookupTable tableName >>= \case
      Just (Table { tableFields }) -> do
        (whereExp', _) <- withTypeEnv (\env -> M.union env (M.fromList tableFields)) (typeCheck (PredExact tBool) whereExp)
        return (Just (ExpDelete locTable whereExp', tIO tUnit))
      Nothing ->
        locatedFail tableSpan "Unknown table"

    Norm.ExpQLimit queryExp limitExp -> do
      (queryExp', queryType) <- typeCheck PredQuery queryExp
      (limitExp', _) <- typeCheck (PredExact tInt) limitExp
      return (Just (ExpQLimit queryExp' limitExp', queryType))

    Norm.ExpQWhere queryExp whereExp -> do
      (queryExp', queryType) <- typeCheck PredQuery queryExp
      case queryType of
        TCons "query" [TRecord fields] -> do
          (whereExp', _) <- withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck (PredExact tBool) whereExp)
          return (Just (ExpQWhere queryExp' whereExp', queryType))

        _ ->
          locatedFail (locatedSpan queryExp) "Expecting a query"

    Norm.ExpQNatJoin a b -> do
      (a', aT) <- typeCheck PredQuery a
      aFields <- asFields (locatedSpan a) aT
      (b', bT) <- typeCheck PredQuery b
      bFields <- asFields (locatedSpan b) bT
      let fieldsIntersect = M.intersectionWith (,) (M.fromList aFields) (M.fromList bFields)
          fieldsUnion = aFields ++ filter (\(key, _) -> key `notElem` map fst aFields) bFields
      if M.null fieldsIntersect
        then locatedFail (locatedSpan b) "Right query does not have fields in command with left query"
        else do
        mapM_ checkFieldType (M.toList fieldsIntersect)
        return $ Just $ (ExpQNatJoin a' b', tQuery (TRecord fieldsUnion))

      where
        asFields :: SourceSpan -> Type -> TypeCheck [(Symbol, Type)]
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
      (queryExp', queryType) <- typeCheck PredQuery queryExp
      case queryType of
        TCons "query" [TRecord fields] -> do
          (selectExp', selectType) <- withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck PredProject selectExp)
          return $ Just (ExpQSelect queryExp' selectExp', TCons "query" [selectType])

        _ ->
          locatedFail (locatedSpan queryExp) "Expecting a query"

    Norm.ExpQOrder queryExp orderExp orderDir -> do
      (queryExp', queryType) <- typeCheck PredQuery queryExp
      case queryType of
        TCons "query" [TRecord fields] -> do
          (orderExp', _) <- withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck PredOrd orderExp)
          return $ Just (ExpQOrder queryExp' orderExp' orderDir, queryType)
              
        _ ->
          locatedFail (locatedSpan queryExp) "Expecting a query"

    _ ->
      return Nothing
