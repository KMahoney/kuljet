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
import RangedParsec (Located(..), SourceSpan)
import Network.HTTP.Types.Method (Method)
import qualified Network.HTTP.Types.Method as Method
import Control.Monad.Reader

import qualified Kuljet.Stage.Norm as Norm
import qualified Kuljet.Env as Env
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
  = PredHtml
  | PredHtmlTagArg
  | PredPostResponse
  | PredResponse
  | PredExact Type
  | PredFn Type TypePred
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
isDbType t = t `elem` [TText, TInt, TTimestamp]


typeCheckModule :: Norm.Module -> Either Error Module
typeCheckModule normalisedModule = do
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
                      map (\tag -> (tag, THtmlTag)) Env.htmlTags ++
                      map (\table -> (tableName table, TQuery (tableRowType table))) tables ++
                      map (\pathVar -> (pathVar, TText)) (pathVars servePath)
          , tableEnv = M.fromList (map (\t -> (tableName t, t)) tables)
          }

    stdEnv = fmap snd Env.stdEnv

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
            TQuery (TRecord fields) ->
              TList <$> withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck p yielder)
              
            _ ->
              locatedFail (locatedSpan queryExp) "Expecting a query"

        Norm.ExpAbs sym body ->
          case p of
            PredFn argT retPred -> do
              retT <- introduce (Norm.discardAnnotation sym) argT (typeCheck retPred body)
              return (TFn argT retT)

            PredPostResponse ->
              case sym of
                Norm.Annotated argName (Just argT) -> do
                  retT <- introduce argName argT (typeCheck PredResponse body)
                  return (TFn argT retT)

                _ ->
                  locatedFail eSpan "Type annotation required"
              
              
            _ ->
              locatedFail eSpan ("Expression is a function, but expected " <> predExpected)
          
        Norm.ExpList exps ->
          if isHtmlPred
          then do
            mapM_ (typeCheck PredHtml) exps
            return (TList THtml)
          else case p of
            PredExact (TList t) -> do
              mapM_ (typeCheck (PredExact t)) exps
              return (TList t)

            _ ->
              locatedFail eSpan ("Expression is a list, but expected " <> predExpected)

        Norm.ExpIf a b c -> do
          _ <- typeCheck (PredExact TBool) a
          if isHtmlPred
            then do
              _ <- typeCheck PredHtml b
              _ <- typeCheck PredHtml c
              return THtml

            else case p of
              PredExact t -> do
                _ <- typeCheck (PredExact t) b
                _ <- typeCheck (PredExact t) c
                return t
  
              _ ->
                locatedFail eSpan ("Expected both 'if' branches to be " <> predExpected)

          
        Norm.ExpInsert (At tableSpan tableName) value andThen ->
          lookupTable tableName >>= \case
          Just table -> do
            _ <- typeCheck (PredExact (tableRowType table)) value
            typeCheck p andThen
          Nothing ->
            locatedFail tableSpan "Unknown table"

        _ ->
          locatedFail eSpan "Cannot infer type"

  where
    isHtml t =
      t `elem` [THtmlTag, THtmlTagWithAttrs, THtml, TText, TInt]

    isHtmlPred =
      p `elem` [PredHtml, PredHtmlTagArg, PredResponse, PredPostResponse]

    isAttributes =
      \case
        TRecord fields -> all ((TText ==) . snd) fields
        _ -> False

    isPostFun =
      \case
        TFn (TRecord fields) ret -> all ((TText ==) . snd) fields && (isHtml ret || isResponse ret)
        _ -> False

    isResponse =
      \case
        TResponse -> True
        _ -> False

    isQuery =
      \case
        TQuery _ -> True
        _ -> False

    isProject =
      \case
        TRecord fields -> all isDbType (map snd fields)
        _ -> False

    applyPred t =
      case p of
        PredHtml -> isHtml t
        PredHtmlTagArg -> isHtml t || isAttributes t
        PredPostResponse -> isHtml t || isPostFun t || isResponse t
        PredResponse -> isHtml t || isResponse t
        PredExact t' -> t == t'
        PredQuery -> isQuery t
        PredOrd -> t `elem` [TInt, TText, TTimestamp, TBool]
        PredProject -> isProject t
        PredCompare -> t `elem` [TInt, TText, TTimestamp, TBool] -- TODO records, lists
        _ -> False
        
    predExpected =
      case p of
        PredHtml -> "HTML"
        PredHtmlTagArg -> "HTML or HTML attributes"
        PredPostResponse -> "HTML or POST function"
        PredResponse -> "response"
        PredExact t -> typeName t
        PredFn _ _ -> "a function"
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
              Norm.LitStr _ -> TText
              Norm.LitInt _ -> TInt
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
        Just THtmlTag -> do
          argType <- typeCheck PredHtmlTagArg arg
          case argType of
            TRecord _ -> return (Just THtmlTagWithAttrs)
            _ -> return (Just THtml)

        Just THtmlTagWithAttrs -> do
          _ <- typeCheck PredHtml arg
          return (Just THtml)

        Just (TFn argT retT) -> do
          _ <- typeCheck (PredExact argT) arg
          return (Just retT)

        Just t ->
          locatedFail (locatedSpan f) ("Expression has type '" <> typeName t <> "', but a function is expected")

        Nothing ->
          return Nothing

    Norm.ExpAbs (Norm.Annotated sym (Just t)) body ->
      (fmap (TFn t)) <$> introduce sym t (infer (discardLocation body))

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
            Norm.OpEq -> (PredCompare, TBool)
            Norm.OpLt -> (PredOrd, TBool)
            Norm.OpGt -> (PredOrd, TBool)
            Norm.OpLtEq -> (PredOrd, TBool)
            Norm.OpGtEq -> (PredOrd, TBool)
            Norm.OpPlus -> (PredExact TInt, TInt)
            Norm.OpMinus -> (PredExact TInt, TInt)
            Norm.OpMul -> (PredExact TInt, TInt)
            Norm.OpDiv -> (PredExact TInt, TInt)
            Norm.OpAnd -> (PredExact TBool, TBool)
            Norm.OpOr -> (PredExact TBool, TBool)

    Norm.ExpQLimit queryExp limitExp -> do
      queryType <- typeCheck PredQuery queryExp
      _ <- typeCheck (PredExact TInt) limitExp
      return (Just queryType)

    Norm.ExpQWhere queryExp whereExp -> do
      queryType <- typeCheck PredQuery queryExp
      case queryType of
        TQuery (TRecord fields) -> do
          _ <- withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck (PredExact TBool) whereExp)
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
        return $ Just $ TQuery $ TRecord fieldsUnion

      where
        asFields loc =
          \case
            TQuery (TRecord fields) ->
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
        TQuery (TRecord fields) ->
          (Just . TQuery) <$> withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck PredProject selectExp)

        _ ->
          locatedFail (locatedSpan queryExp) "Expecting a query"

    Norm.ExpQOrder queryExp orderExp _ -> do
      queryType <- typeCheck PredQuery queryExp
      case queryType of
        TQuery (TRecord fields) -> do
          _ <- withTypeEnv (\env -> M.union env (M.fromList fields)) (typeCheck PredOrd orderExp)
          return (Just queryType)
              
        _ ->
          locatedFail (locatedSpan queryExp) "Expecting a query"

    _ ->
      return Nothing
