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
  | ExpThen (Maybe Symbol) (Located Exp) (Located Exp)
  | ExpList [Located Exp]
  | ExpRecord [(Symbol, Located Exp)]
  | ExpTagF Symbol
  | ExpAnnotated (Located Exp) Type
  | ExpDot (Located Exp) (Located Symbol)
  | ExpInsert (Located Symbol) (Located Exp)
  | ExpDelete (Located Symbol) (Located Exp)
  | ExpYield (Located Exp) (Located Exp)
  | ExpQLimit (Located Exp) (Located Exp)
  | ExpQOrder (Located Exp) (Located Exp) AST.QOrder
  | ExpQSelect (Located Exp) (Located Exp)
  | ExpQWhere (Located Exp) (Located Exp)
  | ExpQNatJoin (Located Exp) (Located Exp)
  | ExpBinOp AST.BinOp (Located Exp) (Located Exp)
  | ExpIf (Located Exp) (Located Exp) (Located Exp)
  deriving (Show)


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

    AST.ExpThen sym a b ->
      ExpThen sym (fmap expand a) (fmap expand b)

    AST.ExpList elems ->
      ExpList (map (fmap expand) elems)

    AST.ExpAnnotated e t ->
      ExpAnnotated (fmap expand e) t

    AST.ExpRecord fields ->
      ExpRecord (map (\(fs, fe) -> (fs, fmap expand fe)) fields)

    AST.ExpTagF sym ->
      ExpTagF sym

    AST.ExpDot r fieldName ->
      ExpDot (fmap expand r) fieldName

    AST.ExpParens subExp ->
      expand subExp

    AST.ExpInsert sym value ->
      ExpInsert sym (fmap expand value)

    AST.ExpDelete sym value ->
      ExpDelete sym (fmap expand value)

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
      ExpAbs sym (if discardAnnotation sym /= key then fmap (subst key value) absBody else absBody)
      
    ExpThen sym a b ->
      ExpThen sym (fmap (subst key value) a) (if sym /= Just key then fmap (subst key value) b else b)

    ExpList elems ->
      ExpList (map (fmap (subst key value)) elems)

    ExpAnnotated e t ->
      ExpAnnotated (fmap (subst key value) e) t
      
    ExpRecord fields ->
      ExpRecord (map (\(fs, fe) -> (fs, fmap (subst key value) fe)) fields)

    ExpTagF lit ->
      ExpTagF lit

    ExpDot r fieldName ->
      ExpDot (fmap (subst key value) r) fieldName

    ExpInsert sym a ->
      ExpInsert sym (fmap (subst key value) a)

    ExpDelete sym a ->
      ExpDelete sym (fmap (subst key value) a)

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

    ExpThen sym a b ->
      ExpThen sym (fmap reduce a) (fmap reduce b)
      
    ExpList elems ->
      ExpList (map (fmap reduce) elems)
      
    ExpAnnotated e t ->
      ExpAnnotated (fmap reduce e) t

    ExpRecord fields ->
      ExpRecord (map (\(fs, fe) -> (fs, fmap reduce fe)) fields)
    
    ExpTagF lit ->
      ExpTagF lit

    ExpDot r fieldName ->
      ExpDot (fmap reduce r) fieldName

    ExpInsert sym a ->
      ExpInsert sym (fmap reduce a)

    ExpDelete sym a ->
      ExpDelete sym (fmap reduce a)

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
