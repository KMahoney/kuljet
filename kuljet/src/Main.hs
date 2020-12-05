module Main where

import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import Control.Monad (join)
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status
import qualified System.Exit as Exit
import qualified System.Posix.Files as Files

import qualified RangedParsec as Parsec
import qualified Kuljet.Stage.AST as AST
import qualified Kuljet.Stage.Norm as Norm
import qualified Kuljet.Stage.TypeCheck as TypeCheck
import qualified Kuljet.Stage.CompileSql as Compile
import qualified Kuljet.Interpret as Interpret
import qualified Kuljet.SourceError as SourceError
import qualified Kuljet.PathPattern as PathPattern

import qualified Database.SQLite3 as DB

import Kuljet.Symbol
import Kuljet.Type


createDatabase :: String -> [TypeCheck.Table] -> IO DB.Database
createDatabase filename tables = do
  dbExists <- Files.fileExist filename
  db <- DB.open (T.pack filename)
  if dbExists then checkTables db else createTables db
  return db

  where
    checkTables db = do
      checks <- mapM (checkTable db) tables
      if and checks
        then
        putStrLn ("Using database: " ++ filename)
        else do
        putStrLn $ "\nDelete '" ++ filename ++ "' to recreate the database, or migrate the database schema.\n"
        Exit.exitFailure

    checkTable db table = do
      dbColumns <- DB.prepare db ("PRAGMA table_info(" <> symbolName (TypeCheck.tableName table) <> ")") >>= collectColumns []
      let tableColumns = (map (\(fieldName, fieldType) -> (symbolName fieldName, sqlType fieldType)) (TypeCheck.tableFields table))
      if S.fromList dbColumns == S.fromList tableColumns
        then
        return True
        else do
        putStrLn $ "Table '" ++ T.unpack (symbolName (TypeCheck.tableName table)) ++ "' does not match the declared type:"
        putStrLn $ "  Database: { " ++ showColumns dbColumns ++ " }"
        putStrLn $ "  Declared: { " ++ showColumns tableColumns ++ " }"
        return False

    showColumns :: [(T.Text, T.Text)] -> String
    showColumns = T.unpack . T.intercalate ", " . map (\(col, t) -> col <> " : " <> t)

    collectColumns accum stmt = do
      nextRow <- DB.step stmt
      case nextRow of
        DB.Row -> do
          columnName <- DB.columnText stmt 1
          columnType <- DB.columnText stmt 2
          collectColumns ((columnName, columnType) : accum) stmt
        DB.Done ->
          return (reverse accum)
      
    createTables db = do
      mapM_ (createTable db) tables
      putStrLn ("Created database: " ++ filename)
      
    createTable db table = do
      DB.exec db (dropTableSql table)
      DB.exec db (createTableSql table)

    dropTableSql :: TypeCheck.Table -> T.Text
    dropTableSql table =
      "DROP TABLE IF EXISTS " <> symbolName (TypeCheck.tableName table) <> ";"

    createTableSql :: TypeCheck.Table -> T.Text
    createTableSql table =
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


loadFile :: String -> IO Compile.Module
loadFile filename = do
  source <- T.readFile filename -- FIXME: Utf8
  case AST.parseModule (T.pack filename) source of
    Left err -> do
      T.putStrLn (Parsec.errMessage err)
      putDoc (Parsec.prettyPos (Parsec.errSourcePos err))
      Exit.exitFailure
      
    Right parsedMod ->
      case TypeCheck.typeCheckModule (Norm.normalise parsedMod) >>= Compile.compileModule of
        Left err -> do
          SourceError.putError err
          Exit.exitFailure
          
        Right ast ->
          return ast


runInterpreter :: DB.Database -> [Interpret.InterpretedRoute] -> IO ()
runInterpreter db routes = do
  putStrLn "Running server on localhost:4000"
  Warp.run 4000 app

  where
    app request respond =
      route routes request respond
      
    route [] _ respond =
      respond $ Wai.responseLBS Status.notFound404 [] "Not Found"
      
    route (r:rs) request respond =
      if Interpret.routeMethod r == Wai.requestMethod request
      then
        case PathPattern.matchPath (Interpret.routePath r) (Wai.pathInfo request) of
          Just pathValues ->
            Interpret.routeRun r db pathValues request respond
          
          Nothing ->
            route rs request respond
      else
        route rs request respond


serve :: String -> Maybe String -> IO ()
serve filename dbPath = do
  typecheckedModule <- loadFile filename
  db <- createDatabase (Maybe.fromMaybe (filename <> ".db") dbPath) (Compile.moduleTables typecheckedModule)
  runInterpreter db (Interpret.moduleInterpreter typecheckedModule)


check :: String -> IO ()
check filename = do
  _ <- loadFile filename
  return ()


optionParser :: ParserInfo (IO ())
optionParser = info (parser <**> helper) infoMod
  where
    parser :: Parser (IO ())
    parser =
      hsubparser commands

    commands :: Mod CommandFields (IO ())
    commands =
      command "serve" (info (serve <$> filename <*> ((Just <$> dbPath) <|> (pure Nothing))) (progDesc "Start server")) <>
      command "check" (info (check <$> filename) (progDesc "Check for errors"))

    filename =
      strArgument (metavar "FILENAME")

    dbPath =
      strOption (metavar "DB" <> long "db")

    infoMod :: InfoMod (IO ())
    infoMod =
      progDesc "The Kuljet language tools and server"
      

main :: IO ()
main =
  join (customExecParser (prefs showHelpOnError) optionParser)
