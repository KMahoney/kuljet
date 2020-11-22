{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

import System.Environment (getArgs)
import qualified Skylighting.Types as Sky
import qualified Skylighting.Loader as Sky
import qualified Data.Map as M
import qualified Data.List as L
import System.Directory
import qualified Text.Pandoc as Pan
import qualified Text.Pandoc.Shared as Pan
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as T
import Lucid


data Doc
  = Doc { docDir :: String, docFilename :: String, docTitle :: T.Text, docBody :: T.Text }
  deriving (Show)

data Docs
  = Docs { guides :: [Doc], reference :: [Doc], notes :: [Doc] }
  deriving (Show)


docUrl :: Doc -> String
docUrl (Doc { docDir, docFilename }) =
  docDir <> (drop 1 . dropWhile (/= '-') . takeWhile (/= '.')) docFilename <> "/"


rFile :: String -> IO T.Text
rFile path = do
  putStrLn ("R " <> path)
  T.readFile path


wFile :: String -> T.Text -> IO ()
wFile path content = do
  putStrLn ("W " <> path)
  T.writeFile path content


parseDoc :: Sky.Syntax -> String -> String -> String -> IO Doc
parseDoc syntax base dir filename = do
  source <- rFile path
  Pan.runIOorExplode $ do
    pd@(Pan.Pandoc meta _) <- Pan.readMarkdown readOpts source
    out <- Pan.writeHtml5String (writeOpts syntax) pd
    return $ Doc dir filename (titleText meta) out

  where
    path = base <> dir <> filename
    titleText meta = Pan.stringify $ Pan.docTitle meta


parseDir :: Sky.Syntax -> String -> String -> IO [Doc]
parseDir syntax base dir = do
  paths <- L.sort <$> listDirectory ("." <> dir)
  mapM (parseDoc syntax base dir) paths


main :: IO ()
main = do
  [srcDir, destDir] <- getArgs

  Right syntax <- Sky.loadSyntaxFromFile "kuljet-syntax.xml"

  intro <- parseDoc syntax srcDir "/" "intro.md"
  install <- parseDoc syntax srcDir "/" "install.md"
  docs <- Docs <$> parseDir syntax srcDir "/guides/"
               <*> parseDir syntax srcDir "/reference/"
               <*> parseDir syntax srcDir "/notes/"

  createDirectoryIfMissing False destDir
  rFile (srcDir <> "/css/default.css") >>= wFile (destDir <> "/default.css")
  rFile (srcDir <> "/images/ext.svg") >>= wFile (destDir <> "/ext.svg")
  rFile (srcDir <> "/images/bars.svg") >>= wFile (destDir <> "/bars.svg")
  wFile (destDir <> "/index.html") (renderPage docs intro)

  createDirectoryIfMissing True (destDir <> "/install")
  wFile (destDir <> "/install/index.html") (renderPage docs install)

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
  Pan.def { Pan.writerSyntaxMap = M.fromList [("kuljet", syntax)] }


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
            li_ (a_ [href_ "/"] "Introduction")
            li_ (a_ [href_ "/install/"] "Install")
            li_ (a_ [href_ "https://github.com/KMahoney/kuljet"] (ext <> "GitHub"))
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
      li_ (a_ [href_ (T.pack (docUrl navDoc))] (toHtml (docTitle navDoc)))
