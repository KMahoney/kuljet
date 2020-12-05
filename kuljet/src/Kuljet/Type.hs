module Kuljet.Type where

import qualified Data.Text as T

import Kuljet.Symbol


data Type
  = TCons T.Text [Type]
  | TRecord [(Symbol, Type)]
  deriving (Show, Eq)


tUnit :: Type
tUnit = TCons "unit" []


tBool :: Type
tBool = TCons "bool" []


tText :: Type
tText = TCons "text" []


tInt :: Type
tInt = TCons "int" []


tTimestamp :: Type
tTimestamp = TCons "timestamp" []


tHtml :: Type
tHtml = TCons "html" []


tHtmlTag :: Type
tHtmlTag = TCons "htmlTag" []


tHtmlTagWithAttrs :: Type
tHtmlTagWithAttrs = TCons "htmlTagWithAttrs" []


tList :: Type -> Type
tList t = TCons "list" [t]


tResponse :: Type
tResponse = TCons "response" []


tIO :: Type -> Type
tIO t = TCons "io" [t]


tFn :: Type -> Type -> Type
tFn t1 t2 = TCons "->" [t1, t2]


tQuery :: Type -> Type
tQuery t = TCons "query" [t]


typeName :: Type -> T.Text
typeName =
  \case
    TRecord fields ->
      "{" <> T.intercalate ", " (map field fields) <> "}"
      where field (Symbol name, t) = name <> ": " <> typeName t
    TCons "->" [dom, rng] ->
      typeName dom <> " -> " <> typeName rng
    TCons name ts ->
      name <> (T.concat (map ((" " <>) . typeName) ts))
