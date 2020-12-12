module Kuljet.AutoDb (middleware) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Kuljet.Stage.TypeCheck as TypeCheck
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified System.Posix.Files as Files
import Lucid
import qualified Database.SQLite3 as DB

import Kuljet.Symbol
import Kuljet.Type


type ColumnTypes = [(T.Text, T.Text)]
type Diff = (ColumnTypes, ColumnTypes)


createTable :: DB.Database -> TypeCheck.Table -> IO ()
createTable db table =
  DB.exec db createTableSql

  where
    createTableSql :: T.Text
    createTableSql =
      "CREATE TABLE " <> symbolName (TypeCheck.tableName table) <> "(" <>
      T.intercalate "," (map createFieldSql (TypeCheck.tableFields table)) <>
      ");"

    createFieldSql :: (Symbol, Type) -> T.Text
    createFieldSql (name, t) =
      symbolName name <> " " <> sqlType t

    sqlType :: Type -> T.Text
    sqlType =
      \case
        TCons "text" [] -> "text"
        TCons "int" [] -> "int"
        TCons "timestamp" [] -> "timestamp"
        _ -> error "Invalid field type"
    

diffTables :: DB.Database -> [TypeCheck.Table] -> IO [(T.Text, Diff)]
diffTables db tables =
  filter (\(_, (a, b)) -> S.fromList a /= S.fromList b) <$> mapM checkTable tables

  where
    checkTable :: TypeCheck.Table -> IO (T.Text, Diff)
    checkTable table = do
      let tableName = symbolName (TypeCheck.tableName table)
      dbColumns <- DB.prepare db ("PRAGMA table_info(" <> tableName <> ")") >>= collectColumns []
      let tableColumns = (map (\(fieldName, fieldType) -> (symbolName fieldName, sqlType fieldType)) (TypeCheck.tableFields table))
      return (tableName, (dbColumns, tableColumns))

    collectColumns :: ColumnTypes -> DB.Statement -> IO ColumnTypes
    collectColumns accum stmt = do
      nextRow <- DB.step stmt
      case nextRow of
        DB.Row -> do
          columnName <- DB.columnText stmt 1
          columnType <- DB.columnText stmt 2
          collectColumns ((columnName, columnType) : accum) stmt
        DB.Done ->
          return (reverse accum)
      
    sqlType :: Type -> T.Text
    sqlType =
      \case
        TCons "text" [] -> "text"
        TCons "int" [] -> "int"
        TCons "timestamp" [] -> "timestamp"
        _ -> error "Invalid field type"


createDatabase :: String -> [TypeCheck.Table] -> IO ()
createDatabase filename tables = do
  db <- DB.open (T.pack filename)
  createTables db

  where
    createTables db =
      mapM_ (createTable db) tables
      

create :: BS.ByteString -> Html ()
create redirect = do
  html_ $ do
    head_ $ do
      title_ "[DEBUG] Database Error"
      style_ $
        "body { max-width: 40rem; margin: 0 auto; padding: 1rem; }"
    body_ $ do
      h1_ "Database Error"
      p_ "Database does not exist."
      form_ [action_ "/_autodb/create", method_ "POST"] $ do
        input_ [type_ "hidden", name_ "redirect", value_ (T.decodeUtf8 redirect)]
        input_ [type_ "submit", value_ "Create Database"]


recreate :: [(T.Text, Diff)] -> BS.ByteString -> Html ()
recreate diffs redirect = do
  html_ $ do
    head_ $ do
      title_ "[DEBUG] Database Error"
      style_ $
        "body { max-width: 40rem; margin: 0 auto; padding: 1rem; } " <>
        ".diff { background: #EEE; padding: 1rem; margin-bottom: 1rem; }"
    body_ $ do
      h1_ "Database Error"
      p_ "Your database is out of sync."
      mapM_ showDiff diffs
      form_ [action_ "/_autodb/recreate", method_ "POST"] $ do
        input_ [type_ "hidden", name_ "redirect", value_ (T.decodeUtf8 redirect)]
        input_ [type_ "submit", value_ "Recreate Database"]
  where
    showDiff = \case
      (name, ([], _)) -> div_ [class_ "diff"] $ do
        p_ $ "Table \"" <> toHtml name <> "\" does not exist"
        form_ [action_ "/_autodb/table", method_ "POST"] $ do
          input_ [type_ "hidden", name_ "redirect", value_ (T.decodeUtf8 redirect)]
          input_ [type_ "hidden", name_ "table", value_ name]
          input_ [type_ "submit", value_ "Create Table"]
      (name, (actual, expected)) -> div_ [class_ "diff"] $ do
        p_ ("Table \"" <> toHtml name <> "\":")
        p_ ("Expected: " <> fields expected)
        p_ ("Actual: " <> fields actual)

    fields fs =
      toHtml $ "{" <> T.intercalate ", " (map field fs) <> "}"

    field (name, t) =
      name <> ": " <> t


middleware :: String -> [TypeCheck.Table] -> Wai.Middleware
middleware dbPath tables app request response =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("POST", ["_autodb", "create"]) -> do
      putStrLn "Creating Database"
      createDatabase dbPath tables
      params <- parseParams
      redirect params "Created Database"

    ("POST", ["_autodb", "table"]) -> do
      putStrLn "Creating Table"
      params <- parseParams
      let table = do
            name <- snd <$> List.find (("table" ==) . fst) params
            List.find (\t -> symbolName (TypeCheck.tableName t) == T.decodeUtf8 name) tables
      db <- DB.open (T.pack dbPath)
      maybe (return ()) (createTable db) table
      redirect params "Created Table"

    ("POST", ["_autodb", "recreate"]) -> do
      putStrLn "Recreating Database"
      Files.removeLink dbPath
      createDatabase dbPath tables
      params <- parseParams
      redirect params "Recreated Database"
      
    _ -> do
      dbExists <- Files.fileExist dbPath
      if dbExists
        then do
          db <- DB.open (T.pack dbPath)
          diffs <- diffTables db tables
          if null diffs
            then app request response
            else response $ Wai.responseLBS HTTP.ok200 [] $ Lucid.renderBS (recreate diffs (Wai.rawPathInfo request))
        else
          response $ Wai.responseLBS HTTP.ok200 [] $ Lucid.renderBS (create (Wai.rawPathInfo request))

  where
    parseParams =
      fst <$> Wai.parseRequestBody Wai.lbsBackEnd request
      
    redirect params msg = do
      let redirectHeader = (HTTP.hLocation,) . snd <$> List.find (("redirect" ==) . fst) params
      response $ Wai.responseLBS HTTP.found302 (maybe [] pure redirectHeader) msg
      
