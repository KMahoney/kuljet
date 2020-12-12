module Kuljet.Type where

import qualified Data.Map as M
import qualified Data.Text as T

import Kuljet.Symbol


data Type
  = TCons T.Text [Type]
  | TRecord [(Symbol, Type)]
  | TVar Int
  deriving (Show, Eq)


type Subst = M.Map Int Type


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


tPassword :: Type
tPassword = TCons "password" []


tHtml :: Type
tHtml = TCons "html" []


tHtmlTag :: Type
tHtmlTag = TCons "htmlTag" []


tHtmlTagArg :: Type
tHtmlTagArg = TCons "htmlTagArg" []


tHtmlTagWithAttrs :: Type
tHtmlTagWithAttrs = TCons "htmlTagWithAttrs" []


tMaybe :: Type -> Type
tMaybe t = TCons "maybe" [t]


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
    TVar i ->
      "_" <> T.pack (show i)


applySubst :: Subst -> Type -> Type
applySubst subst =
  \case
    TCons name ts -> TCons name (map sRec ts)
    TRecord fields -> TRecord (map (\(name, t) -> (name, sRec t)) fields)
    TVar i ->
      case M.lookup i subst of
        Just t -> t
        Nothing -> TVar i

  where
    sRec = applySubst subst


unionSubst :: Subst -> Subst -> Subst
unionSubst a b =
  M.union b (fmap (applySubst b) a)


emptySubst :: Subst
emptySubst = M.empty


matchType :: Type -> Type -> Maybe Subst
matchType =
  curry match

  where
    match :: (Type, Type) -> Maybe Subst
    match =
      \case
        (TCons n1 ts1, TCons n2 ts2)
          | n1 == n2 -> matchTypes ts1 ts2

        (TRecord fs1, TRecord fs2)
          | M.keys (M.fromList fs1) == M.keys (M.fromList fs2) ->
              foldr union (Just emptySubst) $
              M.elems $ M.intersectionWith (,) (M.fromList fs1) (M.fromList fs2)
          where
            union :: (Type, Type) -> Maybe Subst -> Maybe Subst
            union _ Nothing = Nothing
            union (a, b) (Just acc) = unionSubst acc <$> matchType a (applySubst acc b)

        (TVar i, t) ->
          Just (M.singleton i t)

        _ ->
          Nothing

    matchTypes :: [Type] -> [Type] -> Maybe Subst
    matchTypes [] [] = Just M.empty
    matchTypes _  [] = Nothing
    matchTypes [] _  = Nothing
    matchTypes (t1:t1s) (t2:t2s) = do
      s1 <- match (t1, t2)
      s2 <- matchTypes (map (applySubst s1) t1s) t2s
      return (M.union s1 s2)
