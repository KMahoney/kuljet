module Database.QueryBuilder
  ( Expression(..)
  , Source(..)
  , Query(..)
  , Settings(..)
  , Order(..)
  , buildSqLite
  , queryTable
  , collectPlaceholders
  , columnNames
  , applyFilter
  , applyOrder
  , applyProject
  , applyNatJoin
  , applyJoin
  , applyLimit
  , toSql
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Reader

import Database.Sql


data Expression
  = EField Text
  | EQualifiedField Text Text
  | EBinOp Text Expression Expression
  | EString Text
  | EInt Integer
  | ECast Expression Text
  | EPlaceholder Integer
  | EFn Text [Expression]
  | ERaw Text
  deriving (Show)

data Source
  = SourceTable Text
  | SourceQuery Query
  deriving (Show)

data Join
  = NatJoin Source
  | JoinOn Expression Source
  deriving (Show)

data Query
  = Query
    { columns :: [(Text, Expression)]
    , querySource :: Source
    , queryJoins :: [Join]
    , queryFilter :: Maybe Expression
    , queryOrder :: Maybe (Expression, Order)
    , queryLimit :: Maybe Expression
    }
  deriving (Show)

data Order
  = OrderAscending
  | OrderDescending
  deriving (Show)

data Settings
  = Settings { placeholderFormat :: Integer -> Sql }

type Build a = Reader Settings a


buildSqLite :: Query -> Sql
buildSqLite =
  buildSqlWithPlaceholder (\i -> Sql ("?" <> T.pack (show i)))


buildSqlWithPlaceholder :: (Integer -> Sql) -> Query -> Sql
buildSqlWithPlaceholder f query = runReader (toSql query) (Settings f)


queryTable :: Text -> [Text] -> Query
queryTable tableName tableColumns =
  Query { columns = map (\name -> (name, EField name)) tableColumns
        , querySource = SourceTable tableName
        , queryJoins = []
        , queryFilter = Nothing
        , queryOrder = Nothing
        , queryLimit = Nothing
        }


expandQuery :: Query -> Query
expandQuery query =
  Query { columns = columns query
        , querySource = SourceQuery query
        , queryJoins = []
        , queryFilter = Nothing
        , queryOrder = Nothing
        , queryLimit = Nothing
        }

collectPlaceholders :: Query -> [Integer]
collectPlaceholders (Query { columns, querySource, queryJoins, queryFilter }) =
  concatMap colPlaceholders columns ++
  sourcePlaceholders querySource ++
  concatMap joinPlaceholders queryJoins ++
  maybe [] expressionPlaceholders queryFilter

  where
    colPlaceholders (_, e) = expressionPlaceholders e
    sourcePlaceholders = \case
      SourceTable _ -> []
      SourceQuery query -> collectPlaceholders query
    joinPlaceholders = \case
      NatJoin source -> sourcePlaceholders source
      JoinOn e source -> sourcePlaceholders source ++ expressionPlaceholders e
    expressionPlaceholders =
      \case
        EBinOp _ e1 e2 -> expressionPlaceholders e1 ++ expressionPlaceholders e2
        ECast e _ -> expressionPlaceholders e
        EPlaceholder i -> [i]
        _ -> []


columnNames :: Query -> [Text]
columnNames = map fst . columns


applyFilter :: Expression -> Query -> Query
applyFilter expression query@(Query { queryFilter }) =
  query { queryFilter = maybe (Just expression) (Just . EBinOp "AND" expression) queryFilter }


applyOrder :: Expression -> Order -> Query -> Query
applyOrder expression order query =
  q' { queryOrder = Just (expression, order) }

  where
    q' =
      case queryOrder query of
        Just _ -> expandQuery query
        Nothing -> query


applyProject :: M.Map Text Expression -> Query -> Query
applyProject newColumns query =
  Query { columns = M.toList newColumns
        , querySource = SourceQuery query
        , queryJoins = []
        , queryFilter = Nothing
        , queryOrder = Nothing
        , queryLimit = Nothing
        }


applyNatJoin :: Query -> Query -> Query
applyNatJoin a b =
  Query { columns = map selectColumn (S.toList (S.fromList (columnNames a) `S.union` S.fromList (columnNames b)))
        , querySource = SourceQuery a
        , queryJoins = [NatJoin (SourceQuery b)]
        , queryFilter = Nothing
        , queryOrder = Nothing
        , queryLimit = Nothing
        }
  where
    selectColumn name = (name, EField name)


applyJoin :: Expression -> M.Map Text Expression -> Query -> Query -> Query
applyJoin cond merge a b =
  Query { columns = M.toList merge
        , querySource = SourceQuery a
        , queryJoins = [JoinOn cond (SourceQuery b)]
        , queryFilter = Nothing
        , queryOrder = Nothing
        , queryLimit = Nothing
        }

applyLimit :: Expression -> Query -> Query
applyLimit n query =
  q' { queryLimit = Just n }

  where
    q' =
      case queryLimit query of
        Just _ -> expandQuery query
        Nothing -> query

fieldExpressionToSql :: (Text, Expression) -> Build Sql
fieldExpressionToSql (columnName, EField f)
  | columnName == f =
    return (quoteName columnName)
fieldExpressionToSql (columnName, e) = do
  e' <- expressionToSql e
  return $ e' <+> "AS" <+> quoteName columnName


expressionToSql :: Expression -> Build Sql
expressionToSql = \case
  EField name ->
    return (quoteName name)
  EQualifiedField table name ->
    return $ quoteName table <> "." <> quoteName name
  EBinOp op e1 e2 -> do
    e1' <- expressionToSql e1
    e2' <- expressionToSql e2
    return $ parens e1' <+> Sql op <+> parens e2'
  EString s ->
    return (quoteString s)
  EInt i ->
    return $ Sql $ T.pack $ show i
  ECast e t -> do
    e' <- expressionToSql e
    return $ "(" <> e' <> ")::" <> Sql t
  EPlaceholder i -> do
    f <- asks placeholderFormat
    return (f i)
  EFn name args -> do
    args' <- mapM expressionToSql args
    return $ Sql name <> "(" <> intercalate ", " args' <> ")"
  ERaw x ->
    return (Sql x)


sourceToSql :: Source -> Build Sql
sourceToSql (SourceTable name) = return $ quoteName name
sourceToSql (SourceQuery query) = parens <$> toSql query


joinToSql :: Int -> Join -> Build Sql
joinToSql i = \case
  NatJoin source -> do
    source' <- sourceToSql source
    return $ " NATURAL JOIN" <+> source' <+> name
  JoinOn e source -> do
    e' <- expressionToSql e
    source' <- sourceToSql source
    return $ " INNER JOIN" <+> source' <+> name <+> "ON (" <> e' <> ")"

  where
    name = "AS _j" <> Sql (T.pack (show i))


parens :: Sql -> Sql
parens e = "(" <> e <> ")"


toSql :: Query -> Build Sql
toSql (Query { columns, querySource, queryJoins, queryFilter, queryOrder, queryLimit }) = do
  columns' <- mapM fieldExpressionToSql columns
  querySource' <- sourceToSql querySource
  queryJoins' <- joinSql
  queryFilter' <- filterSql
  queryOrder' <- orderSql
  limitSql' <- limitSql
  return $
    "SELECT" <+> intercalate "," columns' <+>
    "FROM" <+> querySource' <> " AS _t" <> queryJoins' <> queryFilter' <> queryOrder' <> limitSql'
  where
    joinSql =
      mconcat <$> mapM (uncurry joinToSql) (zip [(0::Int)..] queryJoins)

    filterSql =
      case queryFilter of
        Nothing -> return ""
        Just e -> (" WHERE" <+>) <$> expressionToSql e

    orderSql =
      case queryOrder of
        Nothing -> return ""
        Just (e, orderDir) -> (\e' -> " ORDER BY" <+> e' <+> orderDirSql orderDir) <$> expressionToSql e

    orderDirSql =
      \case
        OrderAscending -> "ASC"
        OrderDescending -> "DESC"

    limitSql =
      case queryLimit of
        Nothing -> return ""
        Just n -> (" LIMIT" <+>) <$> expressionToSql n
