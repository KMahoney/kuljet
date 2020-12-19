module Kuljet.Stage.CompileSql
  (
    -- mimic the AST interface
    Module(..)
  , Endpoint(..)
  , Exp(..)
  , AST.Table(..)
  , AST.Annotated(..)
  , AST.QOrder(..)
  , AST.Literal(..)
  , AST.BinOp(..)

  , compileModule
  
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.State
import RangedParsec (Located(..), SourceSpan)
import Network.HTTP.Types.Method (Method)

import qualified Kuljet.SourceError as Error
import Kuljet.SourceError (Error(..))
import qualified Kuljet.Stage.TypeCheck as AST
import qualified Database.QueryBuilder as QB


-- Types common to all ASTs

import Kuljet.Symbol
import Kuljet.PathPattern
import Kuljet.Type


-- Compiled AST

data Module =
  Module { moduleEndpoints :: [Endpoint]
         , moduleTables :: [AST.Table]
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
  | ExpLiteral AST.Literal
  | ExpApp (Located Exp) (Located Exp)
  | ExpAbs (AST.Annotated Symbol) (Located Exp)
  | ExpThen (Maybe Symbol) (Located Exp) (Located Exp)
  | ExpList [Located Exp]
  | ExpRecord [(Symbol, Located Exp)]
  | ExpDot (Located Exp) (Located Symbol)
  | ExpInsert (Located Symbol) (Located Exp)
  | ExpDelete (Located Symbol) (QB.Expression, QueryArgs)
  | ExpYield (QB.Query, QueryArgs) (Located Exp) [Type]
  | ExpBinOp AST.BinOp (Located Exp) (Located Exp)
  | ExpIf (Located Exp) (Located Exp) (Located Exp)
  deriving (Show)

type QueryArgs = M.Map Integer Exp


-- compile Monad

data Env
  = Env { tableEnv :: M.Map Symbol AST.Table }

type Compile a = ReaderT Env (Either Error) a


locatedFail :: SourceSpan -> T.Text -> Compile a
locatedFail loc msg =
  lift $ Left $ Error.mkError $ At loc msg


lookupTable :: Symbol -> Compile (Maybe AST.Table)
lookupTable name =
  M.lookup name <$> asks tableEnv


-- compile

compileModule :: AST.Module -> Either Error Module
compileModule m = do
  moduleEndpoints <- mapM compileEndpoint (AST.moduleEndpoints m)
  return $ Module { moduleEndpoints
                  , moduleTables = AST.moduleTables m
                  }

  where
    compileEndpoint :: AST.Endpoint -> Either Error Endpoint
    compileEndpoint endpoint = do
      serveExp <- runReaderT (compileExp (AST.serveExp endpoint)) initialEnv
      return $ Serve { serveMethod = AST.serveMethod endpoint
                     , servePath = AST.servePath endpoint
                     , serveExp
                     , serveType = AST.serveType endpoint
                     }
    initialEnv =
      Env { tableEnv = M.fromList (map (\t -> (AST.tableName t, t)) (AST.moduleTables m)) }


compileExp :: Located AST.Exp -> Compile (Located Exp)
compileExp (At eSpan e) =
  case e of
    AST.ExpVar sym ->
      return $ At eSpan (ExpVar sym)

    AST.ExpLiteral lit ->
      return $ At eSpan (ExpLiteral lit)

    AST.ExpApp f a ->
      At eSpan <$> (ExpApp <$> compileExp f <*> compileExp a)

    AST.ExpAbs arg body ->
      At eSpan <$> ExpAbs arg <$> compileExp body

    AST.ExpThen sym a b ->
      At eSpan <$> (ExpThen sym <$> compileExp a <*> compileExp b)

    AST.ExpList elems ->
      At eSpan <$> ExpList <$> mapM compileExp elems

    AST.ExpAnnotated a _ ->
      compileExp a

    AST.ExpRecord fields ->
      At eSpan <$> ExpRecord <$> mapM (\(fs, fe) -> (fs,) <$> compileExp fe) fields

    AST.ExpDot r fieldName ->
      At eSpan <$> flip ExpDot fieldName <$> compileExp r

    AST.ExpInsert sym value ->
      At eSpan <$> (ExpInsert sym <$> compileExp value)

    AST.ExpDelete sym value -> do
      env <- asks tableEnv
      let fields = S.fromList $ map fst $ AST.tableFields $ env M.! discardLocation sym
      At eSpan <$> (ExpDelete sym <$> runStateT (compileQueryExp fields value) M.empty)

    AST.ExpYield a b fieldTypes -> do
      (query, queryArgs) <- runStateT (compileQuery a) M.empty
      At eSpan <$> (ExpYield (query, queryArgs) <$> compileExp b <*> pure (map ((fieldTypes M.!) . Symbol) (QB.columnNames query)))

    AST.ExpBinOp op a b ->
      At eSpan <$> (ExpBinOp op <$> compileExp a <*> compileExp b)

    AST.ExpIf a b c ->
      At eSpan <$> (ExpIf <$> compileExp a <*> compileExp b <*> compileExp c)

    AST.ExpQLimit _ _ ->
      locatedFail eSpan "Unexpected query"

    AST.ExpQOrder _ _ _ ->
      locatedFail eSpan "Unexpected query"

    AST.ExpQSelect _ _ ->
      locatedFail eSpan "Unexpected query"

    AST.ExpQWhere _ _ ->
      locatedFail eSpan "Unexpected query"

    AST.ExpQNatJoin _ _ ->
      locatedFail eSpan "Unexpected query"


type CompileQ a = StateT QueryArgs (ReaderT Env (Either Error)) a


writeArg :: Exp -> CompileQ Integer
writeArg e = do
  queryArgs <- get
  let i = toInteger (M.size queryArgs) + 1
  put (M.insert i e queryArgs)
  return i


compileQuery :: Located AST.Exp -> CompileQ QB.Query
compileQuery (At eSpan e) =
  case e of
    AST.ExpVar (At symSpan sym) -> do
      lift (lookupTable sym) >>= \case
        Just table -> return $ QB.queryTable (symbolName (AST.tableName table)) (map (symbolName . fst) (AST.tableFields table))
        Nothing -> lift (locatedFail symSpan "Unknown table")

    AST.ExpQLimit a b -> do
      query <- compileQuery a
      limitExp <- compileQueryExp S.empty b
      return $ QB.applyLimit limitExp query

    AST.ExpQSelect a (At _ (AST.ExpRecord fields)) -> do
      query <- compileQuery a
      fields' <- mapM (\(k, v) -> (k,) <$> compileQueryExp (S.fromList (map Symbol (QB.columnNames query))) v) fields
      return $ QB.applyProject (M.fromList (map (\(Symbol k, v) -> (k, v)) fields')) query

    AST.ExpQWhere a b -> do
      query <- compileQuery a
      filterExp <- compileQueryExp (S.fromList (map Symbol (QB.columnNames query))) b
      return $ QB.applyFilter filterExp query

    AST.ExpQNatJoin a b -> do
      a' <- compileQuery a
      b' <- compileQuery b
      return $ QB.applyNatJoin a' b'

    AST.ExpQOrder a b ord -> do
      query <- compileQuery a
      orderExp <- compileQueryExp (S.fromList (map Symbol (QB.columnNames query))) b
      return $ QB.applyOrder orderExp ord' query

      where
        ord' =
          case ord of
            AST.OrderAscending -> QB.OrderAscending
            AST.OrderDescending -> QB.OrderDescending
      
    _ ->
      lift (locatedFail eSpan "Cannot compile into query")
      

compileQueryExp :: S.Set Symbol -> Located AST.Exp -> CompileQ QB.Expression
compileQueryExp env (At eSpan e) =
  case e of
    AST.ExpLiteral lit ->
      case lit of
        AST.LitStr t -> return $ QB.EString t
        AST.LitInt i -> return $ QB.EInt i

    _ ->
      if containsField env e
      then case e of
        AST.ExpVar (At _ sym)
          | S.member sym env ->
            return $ QB.EField (symbolName sym)
    
        AST.ExpBinOp op a b -> do
          a' <- compileQueryExp env a
          b' <- compileQueryExp env b 
          return $ QB.EBinOp sqlOp a' b'

          where
            sqlOp =
              case op of
                AST.OpEq -> "="
                AST.OpPlus -> "+"
                AST.OpMinus -> "-"
                AST.OpMul -> "*"
                AST.OpDiv -> "/"
                AST.OpLt -> "<"
                AST.OpGt -> ">"
                AST.OpLtEq -> "<="
                AST.OpGtEq -> ">="
                AST.OpAnd -> "AND"
                AST.OpOr -> "OR"
                AST.OpConcat -> "||"
          
        _ ->
          lift (locatedFail eSpan "Cannot compile into query expression")
    
      else do
        e' <- lift (compileExp (At eSpan e))
        i <- writeArg (discardLocation e')
        return $ QB.EPlaceholder i


containsField :: S.Set Symbol -> AST.Exp -> Bool
containsField env =
  \case
    AST.ExpVar (At _ sym) ->
      S.member sym env

    AST.ExpLiteral _ ->
      False

    AST.ExpApp (At _ a) (At _ b) ->
      containsField env a || containsField env b

    AST.ExpAbs var (At _ a) ->
      containsField (S.delete (AST.discardAnnotation var) env) a

    AST.ExpThen var (At _ a) (At _ b) ->
      containsField env a ||
      containsField (maybe env (\sym -> S.delete sym env) var) b

    AST.ExpList elems ->
      any (containsField env . discardLocation) elems

    AST.ExpAnnotated e _ ->
      containsField env (discardLocation e)
      
    AST.ExpRecord fields ->
      any (containsField env . discardLocation . snd) fields
      
    AST.ExpDot (At _ a) _ ->
      containsField env a
      
    AST.ExpInsert _ (At _ a) ->
      containsField env a
      
    AST.ExpDelete _ (At _ a) ->
      containsField env a

    AST.ExpYield (At _ a) (At _ b) _ ->
      containsField env a || containsField env b
      
    AST.ExpQLimit (At _ a) (At _ b) ->
      containsField env a || containsField env b
      
    AST.ExpQOrder (At _ a) (At _ b) _ ->
      containsField env a || containsField env b
      
    AST.ExpQSelect (At _ a) (At _ b) ->
      containsField env a || containsField env b
      
    AST.ExpQWhere (At _ a) (At _ b) ->
      containsField env a || containsField env b
      
    AST.ExpQNatJoin (At _ a) (At _ b) ->
      containsField env a || containsField env b

    AST.ExpBinOp _ (At _ a) (At _ b) ->
      containsField env a || containsField env b

    AST.ExpIf (At _ a) (At _ b) (At _ c) ->
      containsField env a || containsField env b || containsField env c
