module Kuljet.Stage.Norm
  ( Module
  , moduleEndpoints
  , moduleTables

    -- mimic the AST interface
  , Endpoint(..)
  , Exp(..)
  , AST.Literal(..)
  , AST.QOrder(..)
  , Annotated(..)
  , AST.Table(..)
  , AST.BinOp(..)
  
  , normalise
  ) where


import qualified Data.Text as T
import Data.List (intercalate)
import RangedParsec.Pos (Located(..))
import Network.HTTP.Types.Method (Method)

import Kuljet.Stage.AST (Annotated(..))
import qualified Kuljet.Stage.AST as AST


-- Types common to all ASTs

import Kuljet.Symbol
import Kuljet.PathPattern
import Kuljet.Type


-- Normalised AST

data Module =
  Module { moduleEndpoints :: [Endpoint]
         , moduleTables :: [AST.Table]
         }
  deriving (Show)


data Endpoint =
  Serve { serveMethod :: Method
        , servePath :: Path
        , serveExp :: Located Exp
        }
  deriving (Show)


data Exp
  = ExpVar (Located Symbol)
  | ExpLiteral AST.Literal
  | ExpApp (Located Exp) (Located Exp)
  | ExpAbs (Annotated Symbol) (Located Exp)
  | ExpList [Located Exp]
  | ExpRecord [(Symbol, Located Exp)]
  | ExpDot (Located Exp) (Located Symbol)
  | ExpInsert (Located Symbol) (Located Exp) (Located Exp)
  | ExpYield (Located Exp) (Located Exp)
  | ExpQLimit (Located Exp) (Located Exp)
  | ExpQOrder (Located Exp) (Located Exp) AST.QOrder
  | ExpQSelect (Located Exp) (Located Exp)
  | ExpQWhere (Located Exp) (Located Exp)
  | ExpQNatJoin (Located Exp) (Located Exp)
  | ExpBinOp AST.BinOp (Located Exp) (Located Exp)
  | ExpIf (Located Exp) (Located Exp) (Located Exp)

instance Show Exp where
  show =
    \case
      ExpVar (At _ sym) -> T.unpack (symbolName sym)
      ExpLiteral (AST.LitStr s) -> "\"" ++ T.unpack s ++ "\""
      ExpLiteral (AST.LitInt i) -> show i
      ExpApp (At _ a) (At _ b) -> "(" ++ show a ++ " " ++ show b ++ ")"
      ExpAbs sym (At _ a) -> "(fun " ++ T.unpack (annotatedSym sym) ++ " -> " ++ show a ++ ")"
      ExpList elems -> "[" ++ intercalate "," (map show elems) ++ "]"
      ExpRecord fields -> "{" ++ intercalate "," (map (\(sym, e) -> T.unpack (symbolName sym) ++ " = " ++ show e) fields) ++ "}"
      ExpDot (At _ a) (At _ b) -> show a ++ "." ++ show b
      ExpInsert (At _ sym) (At _ value) (At _ rest) -> "insert " <> T.unpack (symbolName sym) <> " " <> show value <> " then\n" <> show rest
      ExpYield (At _ a) (At _ b) -> "(" ++ show a ++ " -> " ++ show b ++ ")"
      ExpQLimit (At _ a) (At _ b) -> "(" ++ show a ++ " limit " ++ show b ++ ")"
      ExpQOrder (At _ a) (At _ b) ord -> "(" ++ show a ++ " order " ++ show b ++ " " ++ showOrd ord ++ ")"
      ExpQSelect (At _ a) (At _ b) -> "(" ++ show a ++ " select " ++ show b ++ ")"
      ExpQWhere (At _ a) (At _ b) -> "(" ++ show a ++ " where " ++ show b ++ ")"
      ExpQNatJoin (At _ a) (At _ b) -> "(" ++ show a ++ " natJoin " ++ show b ++ ")"
      ExpBinOp op (At _ a) (At _ b) -> "(" ++ show a ++ " " ++ showOp op ++ " " ++ show b ++ ")"
      ExpIf (At _ a) (At _ b) (At _ c) -> "(if " ++ show a ++ " then " ++ show b ++ " else " ++ show c ++ ")"

    where
      annotatedSym =
        \case
          Annotated {discardAnnotation = sym, annotation = Nothing} ->
            symbolName sym
          Annotated {discardAnnotation = sym, annotation = Just t} ->
            symbolName sym <> " : " <> typeName t

      showOrd =
        \case
          AST.OrderAscending -> "asc"
          AST.OrderDescending -> "desc"

      showOp =
        \case
          AST.OpEq -> "="
          AST.OpPlus -> "+"
          AST.OpMinus -> "-"
          AST.OpMul -> "*"
          AST.OpDiv -> "/"
          AST.OpLt -> "<"
          AST.OpGt -> ">"
          AST.OpLtEq -> "<="
          AST.OpGtEq -> ">="
          AST.OpAnd -> "and"
          AST.OpOr -> "or"


normalise :: AST.Module -> Module
normalise parsedModule =
  Module (normaliseDecls [] (AST.moduleDecls parsedModule)) (AST.moduleTables parsedModule)

  where
    normaliseDecls :: [(Symbol, Exp)] -> [AST.Decl] -> [Endpoint]
    normaliseDecls lets =
      \case
        [] ->
          []
        AST.EndpointDecl serve : decls ->
          normaliseServe lets serve : normaliseDecls lets decls
        AST.LetDecl symbol letExp : decls ->
          normaliseDecls ((symbol, substLets lets (expand letExp)):lets) decls
        _ : decls ->
          normaliseDecls lets decls

    normaliseServe lets (AST.Serve { AST.serveMethod, AST.servePath, AST.serveExp }) =
      Serve { serveMethod, servePath, serveExp = fmap (reduce . substLets lets . expand) serveExp }

    substLets :: [(Symbol, Exp)] -> Exp -> Exp
    substLets lets e =
      foldr (\(key, a) b -> subst key a b) e lets


expand :: AST.Exp -> Exp
expand =
  \case
    AST.ExpVar sym ->
      ExpVar sym

    AST.ExpLiteral lit ->
      ExpLiteral lit

    AST.ExpApp f a ->
      ExpApp (fmap expand f) (fmap expand a)

    AST.ExpAbs arg body ->
      ExpAbs arg (fmap expand body)

    AST.ExpLet sym a b ->
      subst sym (expand (discardLocation a)) (expand (discardLocation b))

    AST.ExpList elems ->
      ExpList (map (fmap expand) elems)

    AST.ExpRecord fields ->
      ExpRecord (map (\(fs, fe) -> (fs, fmap expand fe)) fields)

    AST.ExpDot r fieldName ->
      ExpDot (fmap expand r) fieldName

    AST.ExpParens subExp ->
      expand subExp

    AST.ExpInsert sym value next ->
      ExpInsert sym (fmap expand value) (fmap expand next)

    AST.ExpYield a b ->
      ExpYield (fmap expand a) (fmap expand b)

    AST.ExpQLimit a b ->
      ExpQLimit (fmap expand a) (fmap expand b)

    AST.ExpQOrder a b o ->
      ExpQOrder (fmap expand a) (fmap expand b) o

    AST.ExpQSelect a b ->
      ExpQSelect (fmap expand a) (fmap expand b)

    AST.ExpQWhere a b ->
      ExpQWhere (fmap expand a) (fmap expand b)

    AST.ExpQNatJoin a b ->
      ExpQNatJoin (fmap expand a) (fmap expand b)

    AST.ExpBinOp op a b ->
      ExpBinOp op (fmap expand a) (fmap expand b)

    AST.ExpIf a b c ->
      ExpIf (fmap expand a) (fmap expand b) (fmap expand c)


subst :: Symbol -> Exp -> Exp -> Exp
subst key value =
  \case
    ExpVar (At symSpan sym)
      | sym == key -> value
      | otherwise -> ExpVar (At symSpan sym)

    ExpLiteral lit ->
      ExpLiteral lit
      
    ExpApp f a ->
      ExpApp (fmap (subst key value) f) (fmap (subst key value) a)
      
    ExpAbs sym absBody ->
      ExpAbs sym (fmap (subst key value) absBody)
      
    ExpList elems ->
      ExpList (map (fmap (subst key value)) elems)
      
    ExpRecord fields ->
      ExpRecord (map (\(fs, fe) -> (fs, fmap (subst key value) fe)) fields)

    ExpDot r fieldName ->
      ExpDot (fmap (subst key value) r) fieldName

    ExpInsert sym a b ->
      ExpInsert sym (fmap (subst key value) a) (fmap (subst key value) b)

    ExpYield a b ->
      ExpYield (fmap (subst key value) a) (fmap (subst key value) b)

    ExpQLimit a b ->
      ExpQLimit (fmap (subst key value) a) (fmap (subst key value) b)

    ExpQOrder a b ord ->
      ExpQOrder (fmap (subst key value) a) (fmap (subst key value) b) ord

    ExpQSelect a b ->
      ExpQSelect (fmap (subst key value) a) (fmap (subst key value) b)

    ExpQWhere a b ->
      ExpQWhere (fmap (subst key value) a) (fmap (subst key value) b)

    ExpQNatJoin a b ->
      ExpQNatJoin (fmap (subst key value) a) (fmap (subst key value) b)

    ExpBinOp op a b ->
      ExpBinOp op (fmap (subst key value) a) (fmap (subst key value) b)

    ExpIf a b c ->
      ExpIf (fmap (subst key value) a) (fmap (subst key value) b) (fmap (subst key value) c)


reduce :: Exp -> Exp
reduce =
  \case
    ExpVar sym ->
      ExpVar sym

    ExpLiteral lit ->
      ExpLiteral lit
      
    ExpApp (At _ (ExpAbs annoSym a)) b ->
      reduce (subst (discardAnnotation annoSym) (discardLocation b) (discardLocation a))

    ExpApp f a ->
      ExpApp (fmap reduce f) (fmap reduce a)
      
    ExpAbs sym a ->
      ExpAbs sym (fmap reduce a)
      
    ExpList elems ->
      ExpList (map (fmap reduce) elems)
      
    ExpRecord fields ->
      ExpRecord (map (\(fs, fe) -> (fs, fmap reduce fe)) fields)
    
    ExpDot r fieldName ->
      ExpDot (fmap reduce r) fieldName

    ExpInsert sym a b ->
      ExpInsert sym (fmap reduce a) (fmap reduce b)

    ExpYield a b ->
      ExpYield (fmap reduce a) (fmap reduce b)

    ExpQLimit a b ->
      ExpQLimit (fmap reduce a) (fmap reduce b)

    ExpQOrder a b ord ->
      ExpQOrder (fmap reduce a) (fmap reduce b) ord

    ExpQSelect a b ->
      ExpQSelect (fmap reduce a) (fmap reduce b)

    ExpQWhere a b ->
      ExpQWhere (fmap reduce a) (fmap reduce b)

    ExpQNatJoin a b ->
      ExpQNatJoin (fmap reduce a) (fmap reduce b)

    ExpBinOp op a b ->
      ExpBinOp op (fmap reduce a) (fmap reduce b)

    ExpIf a b c ->
      ExpIf (fmap reduce a) (fmap reduce b) (fmap reduce c)
