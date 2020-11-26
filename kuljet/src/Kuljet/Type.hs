module Kuljet.Type where

import qualified Data.Text as T

import Kuljet.Symbol


data Type
  = THtmlTag
  | THtmlTagWithAttrs
  | THtml
  | TList Type
  | TText
  | TInt
  | TResponse
  | TRecord [(Symbol, Type)]
  | TIO Type
  | TFn Type Type
  | TQuery Type
  | TTimestamp
  | TBool
  | TUnit
  deriving (Show, Eq)


typeName :: Type -> T.Text
typeName =
  \case
    THtmlTag -> "htmlTag"
    THtmlTagWithAttrs -> "htmlTagAttrs"
    THtml -> "html"
    TText -> "text"
    TInt -> "int"
    TResponse -> "response"
    TList t -> "list " <> typeName t
    TIO t -> "io " <> typeName t
    TRecord fields ->
      "{" <> T.intercalate ", " (map field fields) <> "}"
      where field (Symbol name, t) = name <> ": " <> typeName t
    TFn dom rng ->
      typeName dom <> " -> " <> typeName rng
    TQuery t ->
      "query " <> typeName t
    TTimestamp -> "timestamp"
    TBool -> "bool"
    TUnit -> "unit"
