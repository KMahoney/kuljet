{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import qualified Data.Map as M
import qualified Skylighting.Loader as Sky
import qualified Text.Pandoc.Options as Pan


main :: IO ()
main = do
  Right syntax <- Sky.loadSyntaxFromFile "kuljet-syntax.xml"
  let writeOpt = defaultHakyllWriterOptions { Pan.writerSyntaxMap = M.insert "kuljet" syntax (Pan.writerSyntaxMap defaultHakyllWriterOptions) }
      compilePandoc = compile $ pandocCompilerWith defaultHakyllReaderOptions writeOpt
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "intro.md" compilePandoc
    match "guides/*" compilePandoc
    match "reference/*" compilePandoc
    match "notes/*" compilePandoc

    match "index.html" $ do
        route idRoute
        compile $ do
          intro <- loadBody "intro.md"
          guides <- loadAll "guides/*"
          references <- loadAll "reference/*"
          notes <- loadAll "notes/*"

          let indexCtx =
                constField "intro" intro <>
                listField "guides" (slugContext <> defaultContext) (return guides) <>
                listField "references" (slugContext <> defaultContext) (return references) <>
                listField "notes" (slugContext <> defaultContext) (return notes) <>
                defaultContext

          getResourceBody
            >>= applyAsTemplate indexCtx
            >>= relativizeUrls


slugContext :: Context a
slugContext =
  field "slug" (\item -> return (slugify (toFilePath (itemIdentifier item))))

  where
    slugify = drop 1 . dropWhile (/= '-') . takeWhile (/= '.')
