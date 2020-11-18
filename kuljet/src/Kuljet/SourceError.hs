module Kuljet.SourceError (Error(..), mkError, putError, addContext) where

import qualified Data.Text as T

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import RangedParsec (Located(..))
import qualified RangedParsec as Parsec


type Annotation = Located T.Text


data Error
  = Error
    { errMessage :: Annotation
    , errContext :: [Annotation]
    }


mkError :: Annotation -> Error
mkError = flip Error []


renderAnnotation :: Annotation -> Doc AnsiStyle
renderAnnotation (At range msg) = do
  location <> ": " <> pretty msg <> line <> line <> Parsec.prettyRange range <> line

  where
    ((errLine, errCol), _) =
      Parsec.discardSource range

    location =
      pretty (Parsec.sourceFilename range) <> ":" <>
      pretty errLine <> ":" <>
      pretty errCol


render :: Error -> Doc AnsiStyle
render (Error { errMessage, errContext }) =
  vcat (renderAnnotation errMessage : map renderAnnotation errContext)


putError :: Error -> IO ()
putError = putDoc . render


addContext :: Annotation -> Error -> Error
addContext ann err =
  err { errContext = ann : errContext err }
