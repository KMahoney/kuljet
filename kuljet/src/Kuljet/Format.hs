module Kuljet.Format (format, Box, asText) where

import Data.String
import Data.List (intersperse)
import qualified Data.Text as T
import Kuljet.Stage.AST
import Kuljet.Symbol
import Kuljet.Type
import qualified Kuljet.PathPattern as Path
import RangedParsec.Pos (discardLocation, Located(..))


data Part
  = PKeyword T.Text
  | PSpan T.Text
  | PString T.Text
  | PVar T.Text
  | PSpace
  | PTab

data Chunk = Chunk [Part]

instance Semigroup Chunk where
  (Chunk a) <> (Chunk b) = Chunk (a <> b)


data Box = Box {unbox :: [Chunk]}

instance Semigroup Box where
  a <> b = boxAppend a b

instance IsString Box where
  fromString = part . PSpan . T.pack

instance Monoid Box where
  mempty = Box []


part :: Part -> Box
part = Box . (:[]) . Chunk . (:[])


boxAppend :: Box -> Box -> Box
boxAppend (Box a) (Box b) = Box (recurse a b)
  where
    recurse [] y = y
    recurse x [] = x
    recurse (x:[]) (y:ys) = (x <> y) : ys
    recurse (x:xs) y = x : recurse xs y
  

(<+>) :: Box -> Box -> Box
a <+> b = a <> space <> b


vcat :: [Box] -> Box
vcat = Box . concat . map unbox


space :: Box
space = part PSpace


keyword :: T.Text -> Box
keyword = part . PKeyword


symbol :: Symbol -> Box
symbol = part . PSpan . symbolName


prefix :: Chunk -> Box -> Box
prefix pre = Box . map (pre <>) . unbox


indent :: Box -> Box
indent = prefix (Chunk [PTab])


line :: Box
line = Box [Chunk [], Chunk []]


multiline :: Box -> Bool
multiline (Box ls) = length ls > 1


format :: Module -> Box
format m =
  vcat (intersperse line (map formatDecl (moduleDecls m)))


formatDecl :: Decl -> Box
formatDecl = \case
  EndpointDecl (Serve { serveMethod, servePath, serveExp }) ->
    keyword "serve" <+> method <+> formatPath servePath <+> "=" <> line <>
    indent (formatExp (discardLocation serveExp))

    where
      method = case serveMethod of
        "GET" -> "get"
        "POST" -> "post"
        _ -> "get"

  TableDecl (Table { tableName, tableType }) ->
    keyword "table" <+> symbol tableName <> line <>
    indent (formatType (discardLocation tableType))

  LetDecl sym e ->
    keyword "let" <+> symbol sym <+> "=" <> line <>
    indent (formatExp e)

  CommentBlock comment ->
    part (PSpan ("---" <> comment <> "---")) <> line

  where
    formatPath =
      part . PSpan . Path.toText


formatExp :: Exp -> Box
formatExp =
  \case
    ExpVar (At _ sym) ->
      symbol sym

    ExpLiteral lit ->
      case lit of
        LitStr text -> part (PString text)
        LitInt i -> part (PSpan $ T.pack $ show i)

    ExpApp (At _ a) (At _ b) ->
      formatExp a <+> formatExp b

    ExpAbs (Annotated sym annotation) (At _ a) ->
      if multiline body
      then
        fun <> line <> indent body
      else
        fun <+> body

      where
        fun = "fun" <+> symbol sym <> maybe mempty ((": " <>) . formatType) annotation <+> "->"
        body = formatExp a

    ExpLet sym (At _ a) (At _ b) ->
      if multiline body
      then
        let_ <> line <> indent body <> line <> "in" <> line <> formatExp b
      else
        let_ <+> body <+> "in" <> line <> formatExp b

      where
        let_ = "let" <+> symbol sym <+> "="
        body = formatExp a

    ExpThen asSym (At _ a) (At _ b) ->
      formatExp a <> maybe mempty ((" as " <>) . symbol) asSym <+> "then" <> line <> formatExp b

    ExpList exps ->
      "[" <> mconcat (intersperse ", " (map (formatExp . discardLocation) exps)) <> "]"

    ExpRecord fields ->
      "{" <> mconcat (intersperse ", " (map formatField fields)) <> "}"
      where
        formatField (sym, At _ a) = symbol sym <+> "=" <+> formatExp a

    ExpParens a ->
      "(" <> formatExp a <> ")"

    ExpTagF tagName ->
      "<" <> symbol tagName <> ">"

    ExpAnnotated (At _ a) t ->
      formatExp a <+> ":" <+> formatType t

    ExpDot (At _ a) (At _ sym) ->
      formatExp a <> "." <> symbol sym

    ExpInsert (At _ sym) (At _ a) ->
      "insert" <+> symbol sym <+> formatExp a

    ExpDelete (At _ sym) (At _ a) ->
      "delete" <+> symbol sym <+> "where" <+> formatExp a

    ExpYield (At _ a) (At _ b) ->
      formatExp a <+> "->" <+> formatExp b

    ExpQLimit (At _ a) (At _ b) ->
      formatExp a <+> "limit" <+> formatExp b

    ExpQOrder (At _ a) (At _ b) order ->
      formatExp a <+> "order" <+> formatExp b <+>
      case order of
        OrderAscending -> "asc"
        OrderDescending -> "desc"

    ExpQSelect (At _ a) (At _ b) ->
      formatExp a <+> "select" <+> formatExp b

    ExpQWhere (At _ a) (At _ b) ->
      formatExp a <+> "where" <+> formatExp b

    ExpQNatJoin (At _ a) (At _ b) ->
      formatExp a <+> "natJoin" <+> formatExp b

    ExpBinOp op (At _ a) (At _ b) ->
      formatExp a <+> op' <+> formatExp b
      where
        op' = case op of
          OpEq -> "="
          OpPlus -> "+"
          OpMinus -> "-"
          OpMul -> "*"
          OpDiv -> "/"
          OpLt -> "<"
          OpGt -> ">"
          OpLtEq -> "<="
          OpGtEq -> ">="
          OpAnd -> "and"
          OpOr -> "or"
          OpConcat -> "||"

    ExpIf (At _ a) (At _ b) (At _ c) ->
      if multiline a' || multiline b' || multiline c'
      then
        "if" <+> a' <> line <> "then" <> line <> indent b' <> line <> "else" <> line <> indent c'
      else
        "if" <+> a' <+> "then" <+> b' <+> "else" <+> c'

      where
        a' = formatExp a
        b' = formatExp b
        c' = formatExp c
  

formatType :: Type -> Box
formatType =
  \case
    TCons cons args ->
      part (PSpan cons) <> mconcat (map ((" " <>) . formatType) args)
    TRecord fields ->
      "{" <> mconcat (intersperse ", " (map formatField fields)) <> "}"
      where
        formatField (sym, t) = symbol sym <> ":" <+> formatType t
    TVar i ->
      "'" <> (part $ PSpan $ T.singleton $ "abcdefghijklmnopqrstuvwxyz" !! i)


asText :: Box -> T.Text
asText = T.intercalate "\n" . map fromChunk . unbox

  where
    fromChunk :: Chunk -> T.Text
    fromChunk (Chunk l) = T.concat (map fromPart l)

    fromPart :: Part -> T.Text
    fromPart = \case
      PKeyword text -> text
      PSpan text -> text
      PString text -> "\"" <> text <> "\""
      PVar text -> text
      PSpace -> " "
      PTab -> "    "
