module Kuljet.Query where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Database.SQLite3 as DB
import qualified Data.Time.Format as Time

import qualified Database.QueryBuilder as QB
import qualified Database.Sql as Sql

import Kuljet.Value


execute :: DB.Database -> QB.Query -> M.Map Integer Value -> IO [[(T.Text, DB.SQLData)]]
execute db query args = do
  stmt <- DB.prepare db sql
  DB.bindNamed stmt sqlArgs
  rows [] stmt

  where
    sql =
      Sql.toText (QB.buildSqLite query)

    sqlArgs =
      map (\(k, v) -> ("?" <> T.pack (show k), valueToSql v)) (M.toList args)
    
    rows accum stmt = do
      nextRow <- DB.step stmt
      case nextRow of
        DB.Row -> do
          values <- DB.columns stmt
          rows (zip (QB.columnNames query) values : accum) stmt
        DB.Done ->
          return (reverse accum)


valueToSql :: Value -> DB.SQLData
valueToSql =
  \case
    VText text -> DB.SQLText text
    VInt i -> DB.SQLInteger (fromInteger i)
    VTimestamp t -> DB.SQLText (T.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t))
    VBool b -> DB.SQLInteger (if b then 1 else 0)
    _ -> undefined -- TODO
