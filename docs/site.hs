{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

import System.Environment (getArgs)
import qualified Skylighting.Types as Sky
import qualified Skylighting.Loader as Sky
import qualified Skylighting as Sky
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Data.Map as M
import qualified Data.List as L
import System.Directory
import qualified Text.Pandoc as Pan
import qualified Text.Pandoc.Shared as Pan
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as T
import Lucid


data Example
  = Example { exUrl :: String
            , exSrc :: String
            , exName :: String
            , exRunning :: Maybe String
            }
  deriving (Show)

data Doc
  = Doc { docUrl :: String
        , docTitle :: T.Text
        , docBody :: T.Text
        }
  deriving (Show)

data Docs
  = Docs { root :: [Doc]
         , examples :: [Doc]
         , guides :: [Doc]
         , reference :: [Doc]
         , notes :: [Doc]
         }
  deriving (Show)




rFile :: String -> IO T.Text
rFile path = do
  putStrLn ("R " <> path)
  T.readFile path


wFile :: String -> T.Text -> IO ()
wFile path content = do
  putStrLn ("W " <> path)
  T.writeFile path content


parseDoc :: Sky.Syntax -> String -> String -> String -> String -> IO Doc
parseDoc syntax base dir filename url = do
  source <- rFile path
  Pan.runIOorExplode $ do
    pd@(Pan.Pandoc meta _) <- Pan.readMarkdown readOpts source
    out <- Pan.writeHtml5String (writeOpts syntax) pd
    return $ Doc url (titleText meta) out

  where
    path = base <> dir <> filename
    titleText meta = Pan.stringify $ Pan.docTitle meta


parseDir :: Sky.Syntax -> String -> String -> IO [Doc]
parseDir syntax base dir = do
  paths <- L.sort <$> listDirectory ("." <> dir)
  mapM (\path -> parseDoc syntax base dir path (url path)) paths

  where
    url :: String -> String
    url docFilename =
      dir <> (drop 1 . dropWhile (/= '-') . takeWhile (/= '.')) docFilename <> "/"


parseExample :: Sky.Syntax -> Example -> IO Doc
parseExample syntax (Example { exSrc, exUrl, exName, exRunning }) = do
  source <- T.readFile exSrc
  let Right tokens = Sky.tokenize (Sky.TokenizerConfig (syntaxMap syntax) False) syntax source
      srcHtml = LT.toStrict $ Blaze.renderHtml $ Sky.formatHtmlBlock formatOpts tokens
      pageHtml = LT.toStrict $ renderText $ examplePage srcHtml
  return $ Doc ("/examples/" <> exUrl <> "/") (T.pack exName) pageHtml

  where
    formatOpts = Sky.defaultFormatOpts

    examplePage :: T.Text -> Html ()
    examplePage srcHtml = do
      maybe mempty runningOn exRunning
      toHtmlRaw srcHtml

    runningOn :: String -> Html ()
    runningOn url = do
      p_ $ do
        "Running on "
        a_ [ href_ (T.pack url) ] (toHtml url)


main :: IO ()
main = do
  [srcDir, exampleDir, destDir] <- getArgs

  Right syntax <- Sky.loadSyntaxFromFile "kuljet-syntax.xml"

  rootDocs <- sequence
    [ parseDoc syntax srcDir "/" "intro.md" "/"
    , parseDoc syntax srcDir "/" "install.md" "/install/"
    ]

  exampleDocs <-
    let
      chatExample = Example { exUrl = "chat"
                            , exSrc = exampleDir <> "/chat/chat.kj"
                            , exName = "A Simple Chat Server"
                            , exRunning = Just "https://chat.kuljet.com"
                            }
      forumExample = Example { exUrl = "forum"
                             , exSrc = exampleDir <> "/forum/forum.kj"
                             , exName = "A Forum"
                             , exRunning = Just "https://forum.kuljet.com"
                             }
      jsonExample = Example { exUrl = "simplejson"
                            , exSrc = exampleDir <> "/simplejson/simplejson.kj"
                            , exName = "A Simple JSON API"
                            , exRunning = Nothing
                            }
    in
      mapM (parseExample syntax) [ chatExample, forumExample, jsonExample ]
    
  docs <- Docs rootDocs exampleDocs
            <$> parseDir syntax srcDir "/guides/"
            <*> parseDir syntax srcDir "/reference/"
            <*> parseDir syntax srcDir "/notes/"

  createDirectoryIfMissing False destDir
  rFile (srcDir <> "/css/default.css") >>= wFile (destDir <> "/default.css")
  rFile (srcDir <> "/images/ext.svg") >>= wFile (destDir <> "/ext.svg")
  rFile (srcDir <> "/images/bars.svg") >>= wFile (destDir <> "/bars.svg")

  mapM_ (writeDoc destDir docs) (root docs)
  mapM_ (writeDoc destDir docs) (examples docs)
  mapM_ (writeDoc destDir docs) (guides docs)
  mapM_ (writeDoc destDir docs) (reference docs)
  mapM_ (writeDoc destDir docs) (notes docs)


writeDoc :: String -> Docs -> Doc -> IO ()
writeDoc destDir docs doc = do
  createDirectoryIfMissing True (destDir <> docUrl doc)
  wFile (destDir <> docUrl doc <> "index.html") (renderPage docs doc)


renderPage :: Docs -> Doc -> T.Text
renderPage docs doc =
  LT.toStrict (renderText (page docs doc))


readOpts :: Pan.ReaderOptions
readOpts =
  Pan.def { Pan.readerExtensions = exts }

  where
    exts =
      Pan.githubMarkdownExtensions <> additionalExts

    additionalExts =
      Pan.extensionsFromList
      [ Pan.Ext_yaml_metadata_block
      , Pan.Ext_markdown_in_html_blocks
      , Pan.Ext_backtick_code_blocks
      ]


writeOpts :: Sky.Syntax -> Pan.WriterOptions
writeOpts syntax =
  Pan.def { Pan.writerSyntaxMap = syntaxMap syntax }


syntaxMap :: Sky.Syntax -> Sky.SyntaxMap
syntaxMap syntax =
  M.fromList [("kuljet", syntax)]

  
page :: Docs -> Doc -> Html ()
page docs doc = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]
      meta_ [ httpEquiv_ "x-ua-compatible", content_ "ie=edge" ]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]
      link_ [ rel_ "stylesheet", href_ "/default.css" ]
      title_ ("Kuljet: " <> toHtml title)
    body_ $ do
      div_ [id_ "navBar", onclick_ toggleJs] (img_ [src_ "/bars.svg", style_ "height: 1.2rem; pointer-events: none;"])
      nav_ $ do
        ul_ $ do
          li_ "Kuljet"
          ul_ $ do
            mapM_ navLink (root docs)
            li_ (a_ [href_ "https://github.com/KMahoney/kuljet"] (ext <> "GitHub"))
          li_ "Examples"
          ul_ $ do
            mapM_ navLink (examples docs)
          li_ "Guides"
          ul_ $ do
            mapM_ navLink (guides docs)
          li_ "Reference"
          ul_ $ do
            mapM_ navLink (reference docs)
          li_ "Notes"
          ul_ $ do
            mapM_ navLink (notes docs)
      main_ $ do
        h1_ (toHtml title)
        toHtmlRaw (docBody doc)

  where
    title =
      docTitle doc

    toggleJs =
      "var navStyle = document.querySelector('nav').style; navStyle.display = navStyle.display === 'block' ? 'none' : 'block'"

    ext =
      img_ [src_ "/ext.svg", style_ "height: 0.8rem; margin-right: 0.1rem;"]

    navLink :: Doc -> Html ()
    navLink navDoc =
      li_ [class_ (if docUrl navDoc == docUrl doc then "selected" else "")]
        (a_ [href_ (T.pack (docUrl navDoc))] (toHtml (docTitle navDoc)))
