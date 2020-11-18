module Kuljet.Stage.AST
  ( Module
  , moduleDecls
  , moduleEndpoints
  , moduleTables
  
  , Endpoint(..)
  , Exp(..)
  , Literal(..)
  , QOrder(..)
  , Annotated(..)
  , Table(..)
  , BinOp(..)
  
  , parseModule
  ) where

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Maybe as Maybe

import Network.HTTP.Types.Method (Method)
import Control.Applicative
import Data.Char (isAlphaNum, isSpace)
import qualified Network.HTTP.Types.Method as Method

import RangedParsec as Parsec


-- Types common to all ASTs

import Kuljet.Symbol
import Kuljet.PathPattern
import Kuljet.Type


-- AST

data Module =
  Module { moduleDecls :: [Decl] }
  deriving (Show)


moduleEndpoints :: Module -> [Endpoint]
moduleEndpoints = Maybe.mapMaybe endpoint . moduleDecls
  where
    endpoint :: Decl -> Maybe Endpoint
    endpoint =
      \case
        EndpointDecl e -> Just e
        _ -> Nothing
  

moduleTables :: Module -> [Table]
moduleTables = Maybe.mapMaybe table . moduleDecls
  where
    table :: Decl -> Maybe Table
    table =
      \case
        TableDecl t -> Just t
        _ -> Nothing


data Decl
  = EndpointDecl Endpoint
  | TableDecl Table
  deriving (Show)


data Endpoint =
  Serve { serveMethod :: Method
        , servePath :: Path
        , serveExp :: Located Exp
        }
  deriving (Show)


data Table =
  Table { tableName :: Symbol
        , tableType :: Located Type
        }
  deriving (Show)


data Exp
  = ExpVar (Located Symbol)
  | ExpLiteral Literal
  | ExpApp (Located Exp) (Located Exp)
  | ExpAbs (Annotated Symbol) (Located Exp)
  | ExpLet Symbol (Located Exp) (Located Exp)
  | ExpList [Located Exp]
  | ExpRecord [(Symbol, Located Exp)]
  | ExpParens Exp
  | ExpDot (Located Exp) (Located Symbol)
  | ExpInsert (Located Symbol) (Located Exp) (Located Exp)
  | ExpYield (Located Exp) (Located Exp)
  | ExpQLimit (Located Exp) (Located Exp)
  | ExpQOrder (Located Exp) (Located Exp) QOrder
  | ExpQSelect (Located Exp) (Located Exp)
  | ExpQWhere (Located Exp) (Located Exp)
  | ExpBinOp BinOp (Located Exp) (Located Exp)
  deriving (Show)

data QOrder
  = OrderAscending
  | OrderDescending
  deriving (Show)

data BinOp
  = OpEq
  deriving (Show)

data Literal
  = LitStr T.Text
  | LitInt Integer
  deriving (Show)

data Annotated a =
  Annotated { discardAnnotation :: a, annotation :: Maybe Type }
  deriving (Show)


-- Parser

parseModule :: T.Text -> T.Text -> Either ParseError Module
parseModule filename source =
  parse kuljetModule filename source


-- Grammar

kuljetModule :: Parsec Module
kuljetModule =
  skipSpace *> (Module <$> decl `manyTill` eof)


decl :: Parsec Decl
decl =
  expecting "'serve' or 'table' declaration" $
  (EndpointDecl <$> serve) <|> (TableDecl <$> table)

  where
    serve =
      Serve <$> (kwServe *> method) <*> path <*> (opEq *> parseLocated expression)

    table =
      Table <$> (kwTable *> (expecting "table name" symbol)) <*> parseLocated typeDecl


method :: Parsec Method.Method
method =
  expecting "HTTP method (post/get)" $
  get <|> post
  
  where
    get = kwGet >> return Method.methodGet
    post = kwPost >> return Method.methodPost
  

expression :: Parsec Exp
expression = insert <|> yields


insert :: Parsec Exp
insert =
  ExpInsert <$> (kwInsert *> parseLocated symbol) <*> parseLocated expression <*> (kwThen *> parseLocated expression)


yields :: Parsec Exp
yields = do
  start <- getPos
  parseLocated queryOps >>= args start

  where
    args start e =
      nextArg start e <|> pure (discardLocation e)

    nextArg start e = do
      operator "->"
      a <- parseLocated queryOps
      fSpan <- getSpan start
      args start (At fSpan (ExpYield e a))


queryOps :: Parsec Exp
queryOps = do
  start <- getPos
  parseLocated next >>= ops start

  where
    next =
      binOps
      
    ops start e =
      nextOp start e <|> pure (discardLocation e)

    nextOp start e =
      qBin kwLimit ExpQLimit start e <|>
      qBin kwSelect ExpQSelect start e <|>
      qBin kwWhere ExpQWhere start e <|>
      qOrder start e

    qBin kw cons start e = do
      kw
      a <- parseLocated next
      fSpan <- getSpan start
      ops start (At fSpan (cons e a))

    qOrder start e = do
      kwOrder
      a <- parseLocated next
      ord <- optional ((kwAsc >> return OrderAscending) <|> (kwDesc >> return OrderDescending))
      fSpan <- getSpan start
      ops start (At fSpan (ExpQOrder e a (Maybe.fromMaybe OrderAscending ord)))
  

binOps :: Parsec Exp
binOps = do
  start <- getPos
  parseLocated next >>= ops start

  where
    next =
      application
      
    ops start e =
      nextOp start e <|> pure (discardLocation e)

    nextOp start e = do
      opEq
      a <- parseLocated next
      fSpan <- getSpan start
      ops start (At fSpan (ExpBinOp OpEq e a))


application :: Parsec Exp
application = do
  start <- getPos
  parseLocated fields >>= args start

  where
    args start e =
      nextArg start e <|> pure (discardLocation e)

    nextArg start e = do
      a <- parseLocated fields
      fSpan <- getSpan start
      args start (At fSpan (ExpApp e a))


fields :: Parsec Exp
fields = do
  start <- getPos
  parseLocated simpleExpression >>= recurse start

  where
    recurse start e =
      nextField start e <|> pure (discardLocation e)

    nextField start e = do
      dot
      sym <- parseLocated symbol
      fSpan <- getSpan start
      recurse start (At fSpan (ExpDot e sym))


simpleExpression :: Parsec Exp
simpleExpression =
  expecting "expression" $
  parens <|> fn <|> array <|> record <|> int <|> var <|> str <|> let_
  
  where
    parens = ExpParens <$> (lParen *> expression <* rParen)
    fn = ExpAbs <$> (kwFun *> annotatedSymbol) <*> (operator "->" *> parseLocated expression)
    array = ExpList <$> (lBracket *> parseLocated expression `sepBy` comma <* rBracket)
    int = ExpLiteral . LitInt <$> lexeme integer
    str = ExpLiteral . LitStr <$> lexeme quotedString
    var = ExpVar <$> parseLocated symbol
    let_ = ExpLet <$>
      (kwLet *> symbol) <*>
      (operator "=" *> parseLocated expression) <*>
      (kwIn *> parseLocated expression)
    record = ExpRecord <$> (lCurly *> field `sepBy` comma <* rCurly)

    -- fields support name punning, so that {a} = {a = a}
    field = do
      At sSpan s <- parseLocated symbol
      e <- optional (operator "=" *> parseLocated expression)
      return (s, Maybe.fromMaybe (At sSpan (ExpVar (At sSpan s))) e)


annotatedSymbol :: Parsec (Annotated Symbol)
annotatedSymbol =
  Annotated <$> symbol <*> (optional (colon *> typeDecl))


typeDecl :: Parsec Type
typeDecl = expecting "type declartion" $
  simpleType <|> listType <|> recordType

  where
    simpleType = do
      sym <- symbol
      case symbolName sym of
        "html" -> return THtml
        "htmlTag" -> return THtmlTag
        "htmlTagAttrs" -> return THtmlTagWithAttrs
        "text" -> return TText
        "int" -> return TInt
        "timestamp" -> return TTimestamp
        name -> parseError (S.singleton ("unknown type '" <> name <> "'"))

    listType =
      TList <$> (lBracket *> typeDecl <* rBracket)

    recordType =
      TRecord <$> (lCurly *> field `sepBy` comma <* rCurly)

    field =
      (,) <$> symbol <*> (colon *> typeDecl)


-- Lexemes

lexeme :: Parsec a -> Parsec a
lexeme p = p <* skipSpace


-- Match the way WAI reports paths, where [] is root and
-- [""] is a trailing slash
path :: Parsec Path
path = Path <$> (lexeme (segments <|> root))
  where
    isPathChar c =
      not (c == '/') && not (isSpace c)

    segments =
      (:) <$>
      (try (char '/' *> (var <|> match1))) <*>
      (many (char '/' *> (var <|> match)))

    var =
      PathVar . Symbol <$> (char ':' *> Parsec.takeWhile1 isIdentifierChar)

    match1 =
      PathMatch <$> Parsec.takeWhile1 isPathChar

    match =
      PathMatch <$> Parsec.takeWhile isPathChar

    root =
      (char '/' >> return [])


keyword :: T.Text -> Parsec ()
keyword kw = expecting ("'" <> kw <> "'") $
  lexeme (matchSpan isIdentifierChar kw >> return ())


comma :: Parsec ()
comma = lexeme (char ',')


dot :: Parsec ()
dot = lexeme (char '.')


colon :: Parsec ()
colon = lexeme (char ':')


lParen, rParen :: Parsec ()
lParen = lexeme (char '(')
rParen = lexeme (char ')')


lCurly, rCurly :: Parsec ()
lCurly = lexeme (char '{')
rCurly = lexeme (char '}')


lBracket, rBracket :: Parsec ()
lBracket = lexeme (char '[')
rBracket = lexeme (char ']')


kwServe, kwGet, kwPost, kwFun, kwLet, kwIn, kwTable, kwInsert, kwThen, kwLimit, kwOrder, kwAsc, kwDesc, kwSelect, kwWhere :: Parsec ()
kwServe = keyword "serve"
kwGet = keyword "get"
kwPost = keyword "post"
kwFun = keyword "fun"
kwLet = keyword "let"
kwIn = keyword "in"
kwTable = keyword "table"
kwInsert = keyword "insert"
kwThen = keyword "then"
kwLimit = keyword "limit"
kwOrder = keyword "order"
kwAsc = keyword "asc"
kwDesc = keyword "desc"
kwSelect = keyword "select"
kwWhere = keyword "where"

anyKeyword :: Parsec ()
anyKeyword = foldr (<|>) empty
  [ kwServe, kwGet, kwPost, kwFun, kwLet, kwIn
  , kwTable, kwInsert, kwThen, kwLimit, kwOrder, kwAsc, kwDesc
  , kwSelect, kwWhere
  ]


isIdentifierChar :: Char -> Bool
isIdentifierChar c =
  isAlphaNum c || c `elem` ("_" :: String)


operator :: T.Text -> Parsec ()
operator op = expecting ("operator '" <> op <> "'") $
  lexeme (matchSpan isOperatorChar op >> return ())


opEq :: Parsec ()
opEq = operator "="


isOperatorChar :: Char -> Bool
isOperatorChar c =
  c `elem` (":+-<>|=/*" :: String)


symbol :: Parsec Symbol
symbol =
  excluding anyKeyword >>
  (Symbol <$> lexeme (Parsec.takeWhile1 isIdentifierChar))
  
