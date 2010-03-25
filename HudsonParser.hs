module HudsonParser where

import Text.ParserCombinators.Parsec
import Control.Monad
import HudsonToken

data Block = BlockStmt Stmt
           | BlockDecl Decl
             deriving (Eq, Show)

type VarID = String
type ClassID = String

data Stmt = Assignment VarID Expr
          | ProcCall VarID [Expr]
          | If {condition :: Expr, thenCode :: [Block], elseCode :: [Block]}
          | While {condition :: Expr, whileCode :: [Block]}
          | Return (Maybe Expr)
          | Assert Expr
          | Null
            deriving (Eq, Show)

data Decl = VarDecl {varName :: VarID, varType :: (Maybe ClassID), varExpr :: Expr}
          | ConstDecl VarID Expr
          | FuncDecl {funcName :: VarID, funcParams :: [Param], funcCode :: [Block]}
          | ProcDecl {procName :: VarID, procParams :: [Param], procCode :: [Block]}
          | ClassDecl {className :: ClassID,
                       inherits :: Maybe ClassID,
                       subtypes :: [ClassID],
                       classCode :: [Block]}
            deriving (Eq, Show)

data Param = Param {ref :: Bool, paramName :: VarID, paramType :: (Maybe ClassID)}
             deriving (Eq, Show)

data Expr = LiteralInt Int
          | LiteralStr String
          | LiteralBool Bool
          | LiteralNull
          | Negate Expr
          | Not Expr
          | Binary BinaryOp Expr Expr
          | VarID String
          | FuncCall String
          | TypeTest Expr String
            deriving (Eq, Show)

data BinaryOp = Add
              | Sub
              | Mult
              | Div
              | Mod
              | Equal
              | NotEqual
              | LessThan
              | LessThanEqual
              | GreaterThan
              | GreaterThanEqual
              | And
              | Or
              | Concat
                deriving (Eq, Show)

-- TODO
--
-- * Have a separate lex phase to resolve continuation comments.
--
-- * How to handle offside rule?  Maybe add an indentation parameter
-- to parseCode.
--
-- * The lexer built by makeTokenParser ignores newlines.  The current
-- parser is very accepting of malformed input.

parseFile :: CharParser () [Block]
parseFile = manyTill parseBlock eof

parseBlock :: CharParser () Block
parseBlock = liftM BlockStmt parseStmt
         <|> liftM BlockDecl parseDecl
         <?> "declaration or statemnt"

parseStmt :: CharParser () Stmt
parseStmt = parseAssign
        <|> parseProcCall
        <|> parseIf
        <|> parseWhile
        <|> parseReturn
        <|> parseAssert
        <|> parseNull
        <?> "statement"

parseDecl :: CharParser () Decl
parseDecl = parseVarDecl
        <|> parseConstDecl
        <|> parseFuncDecl
        <|> parseProcDecl
        <|> parseClassDecl
        <?> "declaration"

parseAssign = undefined

parseProcCall = undefined

parseIf = undefined

parseWhile = undefined

parseReturn = do
  reserved "return"
  e <- parseExpr
  return $ Return (Just e)

parseAssert = do
  reserved "assert"
  e <- parseExpr
  return $ Assert e

parseNull = reserved "null" >> return Null

-- Declarations

parseVarDecl = do
  reserved "variable"
  n <- parseVarID
  optType <- parseOptionalType
  reservedOp ":="
  e <- parseExpr
  return VarDecl {varName = n, varType = optType, varExpr = e}

parseConstDecl = undefined

parseFuncDecl = do
  indent <- liftM sourceColumn getPosition
  reserved "function"
  n <- identifier
  ps <- parseParams
  reserved "is"
  c <- parseCode indent
  return $ FuncDecl {funcName = n, funcParams = ps, funcCode = c}

parseProcDecl = undefined

parseClassDecl = undefined

-- Params
parseParams = parens (commaSep parseParam)

parseParam = do
  r <- (reserved "ref" >> return True) <|> return False
  v <- parseVarID
  t <- parseOptionalType
  return $ Param {ref = r, paramName = v, paramType = t}

-- Expressions
parseExpr = return $ LiteralInt 2

-- Utilities
validChar = alphaNum <|> char '_'

parseClassID :: CharParser () ClassID
parseClassID = liftM2 (:) upper (lexeme $ many validChar)

parseVarID :: CharParser () VarID
parseVarID = liftM2 (:) lower (lexeme $ many validChar)

parseOptionalType :: CharParser () (Maybe ClassID)
parseOptionalType = do reservedOp ":"
                       c <- parseClassID
                       return $ Just c
                <|> return Nothing

-- | Parse indented code block of a method or class.  Parses all code
-- indented greater than n spaces.
parseCode :: Int -> CharParser () [Block]
parseCode n = many1 parseCode' <?> "indented declaration"
    where
      parseCode' = do indent <- liftM sourceColumn getPosition
                      if indent > n then parseBlock else pzero

hudFunc = "function blah(one, two) is\n  write(1)\n  function nest () is\n    write(2)\n"
       ++ "function joe (three, four) is \n  write(3)\n"
       ++ "write(4)"

pr = putStrLn hudFunc
test = parse parseFile "hudson" hudFunc

