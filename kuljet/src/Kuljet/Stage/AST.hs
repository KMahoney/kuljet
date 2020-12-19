module Kuljet.Stage.AST
  ( Module
  , moduleDecls
  , moduleEndpoints
  , moduleTables

  , Decl(..)
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
  | LetDecl Symbol Exp
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
  | ExpThen (Maybe Symbol) (Located Exp) (Located Exp)
  | ExpList [Located Exp]
  | ExpRecord [(Symbol, Located Exp)]
  | ExpParens Exp
  | ExpTagF Symbol
  | ExpAnnotated (Located Exp) Type
  | ExpDot (Located Exp) (Located Symbol)
  | ExpInsert (Located Symbol) (Located Exp)
  | ExpDelete (Located Symbol) (Located Exp)
  | ExpYield (Located Exp) (Located Exp)
  | ExpQLimit (Located Exp) (Located Exp)
  | ExpQOrder (Located Exp) (Located Exp) QOrder
  | ExpQSelect (Located Exp) (Located Exp)
  | ExpQWhere (Located Exp) (Located Exp)
  | ExpQNatJoin (Located Exp) (Located Exp)
  | ExpBinOp BinOp (Located Exp) (Located Exp)
  | ExpIf (Located Exp) (Located Exp) (Located Exp)
  deriving (Show)

data QOrder
  = OrderAscending
  | OrderDescending
  deriving (Show)

data BinOp
  = OpEq
  | OpPlus
  | OpMinus
  | OpMul
  | OpDiv
  | OpGt
  | OpLt
  | OpGtEq
  | OpLtEq
  | OpAnd
  | OpOr
  | OpConcat
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
  (EndpointDecl <$> serve) <|> (TableDecl <$> table) <|> topLet

  where
    serve =
      Serve <$> (kwServe *> method) <*> path <*> (opEq *> parseLocated expression)

    table =
      Table <$> (kwTable *> (expecting "table name" symbol)) <*> parseLocated typeDecl

    topLet =
      LetDecl <$> (kwLet *> symbol) <*> (opEq *> expression)


method :: Parsec Method.Method
method =
  expecting "HTTP method (post/get)" $
  get <|> post
  
  where
    get = kwGet >> return Method.methodGet
    post = kwPost >> return Method.methodPost
  

expression :: Parsec Exp
expression = letInExpression


letInExpression :: Parsec Exp
letInExpression = let_ <|> next

  where
    next =
      asThenExpression
      
    let_ = ExpLet <$>
      (kwLet *> symbol) <*>
      (operator "=" *> parseLocated expression) <*>
      (kwIn *> parseLocated expression)


asThenExpression :: Parsec Exp
asThenExpression = do
  e <- parseLocated next
  recurse e <|> pure (discardLocation e)

  where
    next =
      typeAnnotation

    recurse e1 = do
      asSym <- optional (kwAs *> symbol)
      kwThen
      e2 <- parseLocated expression
      return (ExpThen asSym e1 e2)


typeAnnotation :: Parsec Exp
typeAnnotation = do
  e <- parseLocated next
  recurse e <|> pure (discardLocation e)

  where
    next =
      infixExpressions

    recurse e1 = do
      operator ":"
      t <- typeDecl
      return (ExpAnnotated e1 t)


-- FIXME: improve this
infixExpressions :: Parsec Exp
infixExpressions = yields

  where
    yields :: Parsec Exp
    yields = do
      start <- getPos
      parseLocated next >>= args start
    
      where
        next =
          queryOps
          
        args start e =
          nextArg start e <|> pure (discardLocation e)
    
        nextArg start e = do
          operator "->"
          a <- parseLocated next
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
          qBin kwNatJoin ExpQNatJoin start e <|>
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
    binOps =
      startChain initialTiers
    
      where
        next =
          application
    
        initialTiers =
          [ (kwAnd >> pure OpAnd) <|> (kwOr >> pure OpOr)
          , (opP "=" OpEq)
          , lessThan <|> (opP ">" OpGt) <|> (opP "<=" OpLtEq) <|> (opP ">=" OpGtEq)
          , (opP "+" OpPlus) <|> (opP "-" OpMinus) <|> (opP "||" OpConcat)
          , (opP "*" OpMul) <|> (opP "/" OpDiv)
          ]

        -- Special case for the less-then operator to distinguish it from
        -- HTML tag functions (e.g. <html>)
        lessThan =
          lexeme (try (char '<' >> excluding symbol)) >> return OpLt
    
        opP op t =
          operator op >> pure t
    
        startChain [] = next
        startChain (tier:nextTiers) = do
          start <- getPos
          parseLocated (startChain nextTiers) >>= loop tier (startChain nextTiers) start
            
        loop tier nextTier start e =
          nextOp tier nextTier start e <|> pure (discardLocation e)
    
        nextOp tier nextTier start e = do
          op <- tier
          a <- parseLocated nextTier
          fSpan <- getSpan start
          loop tier nextTier start (At fSpan (ExpBinOp op e a))


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
  parens <|> fn <|> tagF <|> ifExp <|> array <|> record <|> insert <|> delete <|> int <|> var <|> str
  
  where
    parens = ExpParens <$> (lParen *> expression <* rParen)
    fn = ExpAbs <$> (kwFun *> annotatedSymbol) <*> (operator "->" *> parseLocated expression)
    array = ExpList <$> (lBracket *> parseLocated expression `sepBy` comma <* rBracket)
    int = ExpLiteral . LitInt <$> lexeme integer
    str = ExpLiteral . LitStr <$> lexeme quotedString
    var = ExpVar <$> parseLocated symbol
    record = ExpRecord <$> (lCurly *> field `sepBy` comma <* rCurly)
    insert = ExpInsert <$> (kwInsert *> parseLocated symbol) <*> parseLocated simpleExpression
    delete = ExpDelete <$> (kwDelete *> parseLocated symbol) <*> (kwWhere *> parseLocated infixExpressions)
    tagF = ExpTagF <$> lexeme (try (char '<' *> symbol <* char '>'))

    -- FIXME conflicting 'then'
    ifExp = ExpIf <$> (kwIf *> parseLocated typeAnnotation)
                  <*> (kwThen *> parseLocated expression)
                  <*> (kwElse *> parseLocated expression)

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
  recordType <|> simpleType

  where
    simpleType = do
      (Symbol tName) <- symbol
      return (TCons tName [])

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


kwServe, kwGet, kwPost, kwFun, kwLet, kwIn, kwTable :: Parsec ()
kwInsert, kwDelete, kwThen, kwAs, kwLimit, kwOrder, kwAsc, kwDesc :: Parsec ()
kwSelect, kwWhere, kwNatJoin, kwAnd, kwOr, kwIf, kwElse :: Parsec ()

kwServe = keyword "serve"
kwGet = keyword "get"
kwPost = keyword "post"
kwFun = keyword "fun"
kwLet = keyword "let"
kwIn = keyword "in"
kwTable = keyword "table"
kwInsert = keyword "insert"
kwDelete = keyword "delete"
kwThen = keyword "then"
kwAs = keyword "as"
kwLimit = keyword "limit"
kwOrder = keyword "order"
kwAsc = keyword "asc"
kwDesc = keyword "desc"
kwSelect = keyword "select"
kwWhere = keyword "where"
kwNatJoin = keyword "natJoin"
kwAnd = keyword "and"
kwOr = keyword "or"
kwIf = keyword "if"
kwElse = keyword "else"


anyKeyword :: Parsec ()
anyKeyword = foldr (<|>) empty
  [ kwServe, kwGet, kwPost, kwFun, kwLet, kwIn
  , kwTable, kwInsert, kwThen, kwAs, kwLimit, kwOrder, kwAsc, kwDesc
  , kwSelect, kwWhere, kwNatJoin, kwAnd, kwOr, kwIf, kwElse
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
  
