module Main where

import qualified Data.Maybe as Maybe
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status
import qualified System.Exit as Exit
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Posix
import Control.Monad.State
import qualified Data.IORef as IORef

import qualified RangedParsec as Parsec
import qualified Kuljet.Stage.AST as AST
import qualified Kuljet.Stage.Norm as Norm
import qualified Kuljet.Stage.TypeCheck as TypeCheck
import qualified Kuljet.Stage.CompileSql as Compile
import qualified Kuljet.Interpret as Interpret
import qualified Kuljet.SourceError as SourceError
import qualified Kuljet.PathPattern as PathPattern
import qualified Kuljet.Env as Env
import qualified Kuljet.AutoDb as AutoDb

import qualified Database.SQLite3 as DB


data ServeOptions
  = ServeOptions { serveSourcePath :: String
                 , serveReload :: Bool
                 , serveDebug :: Bool
                 , serveDbPath :: Maybe String
                 }


dbPath :: ServeOptions -> String
dbPath opts =
  Maybe.fromMaybe (serveSourcePath opts <> ".db") (serveDbPath opts)


loadFile :: String -> IO (Maybe Compile.Module)
loadFile filename = do
  source <- T.readFile filename -- FIXME: Utf8
  case AST.parseModule (T.pack filename) source of
    Left err -> do
      T.putStrLn (Parsec.errMessage err)
      putDoc (Parsec.prettyPos (Parsec.errSourcePos err))
      return Nothing
      
    Right parsedMod ->
      case TypeCheck.typeCheckModule (fmap snd Env.stdEnv) (Norm.normalise parsedMod) >>= Compile.compileModule of
        Left err -> do
          SourceError.putError err
          return Nothing
          
        Right ast ->
          return (Just ast)


data ReloadState =
  ReloadState { reloadRoutes :: [Interpret.InterpretedRoute]
              , reloadTables :: [TypeCheck.Table]
              , reloadModTime :: Posix.EpochTime
              }


runInterpreter :: ServeOptions -> [TypeCheck.Table] -> [Interpret.InterpretedRoute] -> IO ()
runInterpreter opts initialTables initialRoutes = do
  putStrLn "Running server on localhost:4000"
  if serveReload opts
    then do
      reloadStateRef <- (ReloadState initialRoutes initialTables <$> queryModTime) >>= IORef.newIORef
      Warp.run 4000 (reloadApp reloadStateRef)
    else
      Warp.run 4000 (app initialTables initialRoutes)

  where
    filename =
      serveSourcePath opts

    reloadApp :: IORef.IORef ReloadState -> Wai.Application
    reloadApp serveStateRef request respond = do
      serveState <- IORef.readIORef serveStateRef
      newModTime <- queryModTime
      if newModTime > reloadModTime serveState
        then do
          typecheckedModule <- loadFile filename
          case typecheckedModule of
            Just m -> do
              let tables = Compile.moduleTables m
              let routes = Interpret.moduleInterpreter (fmap fst Env.stdEnv) m
              IORef.writeIORef serveStateRef (ReloadState routes tables newModTime)
              putStrLn "Reloaded"
              app tables routes request respond
            Nothing ->
              app (reloadTables serveState) (reloadRoutes serveState) request respond
        else
          app (reloadTables serveState) (reloadRoutes serveState) request respond

    queryModTime =
      Files.modificationTime <$> Files.getFileStatus filename

    app :: [TypeCheck.Table] -> [Interpret.InterpretedRoute] -> Wai.Application
    app tables routes =
      if serveDebug opts
      then AutoDb.middleware (dbPath opts) tables (withConnection (route routes))
      else (withConnection (route routes))

    withConnection :: (DB.Database -> Wai.Application) -> Wai.Application
    withConnection innerApp request respond = do
      db <- DB.open (T.pack (dbPath opts))
      innerApp db request respond
      
    route :: [Interpret.InterpretedRoute] -> DB.Database -> Wai.Application
    route [] _ _ respond =
      respond $ Wai.responseLBS Status.notFound404 [] "Not Found"
    route (r:rs) db request respond =
      if Interpret.routeMethod r == Wai.requestMethod request
      then
        case PathPattern.matchPath (Interpret.routePath r) (Wai.pathInfo request) of
          Just pathValues ->
            Interpret.routeRun r db pathValues request respond
          
          Nothing ->
            route rs db request respond

      else
        route rs db request respond


serve :: ServeOptions -> IO ()
serve opts = do
  typecheckedModule <- loadFile (serveSourcePath opts) >>= maybe Exit.exitFailure return
  runInterpreter opts (Compile.moduleTables typecheckedModule) (Interpret.moduleInterpreter (fmap fst Env.stdEnv) typecheckedModule)


dev :: ServeOptions -> IO ()
dev opts = do
  serve (opts { serveReload = True, serveDebug = True })


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
      command "dev" (info (dev <$> serveOptions) (progDesc "Alias for 'serve --debug --reload'")) <>
      command "serve" (info (serve <$> serveOptions) (progDesc "Start server")) <>
      command "check" (info (check <$> filename) (progDesc "Check for errors"))

    filename =
      strArgument (metavar "FILENAME")

    serveOptions =
      ServeOptions <$>
      filename <*>
      switch (long "reload") <*>
      switch (long "debug") <*>
      optional (strOption (metavar "DB" <> long "db"))

    infoMod :: InfoMod (IO ())
    infoMod =
      progDesc "The Kuljet language tools and server"
      

main :: IO ()
main =
  join (customExecParser (prefs showHelpOnError) optionParser)
