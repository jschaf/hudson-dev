module HudsonParser where

import Text.ParserCombinators.Parsec
import Control.Monad
import HudsonToken

data Block = BlockStmt Stmt
           | BlockDecl Decl
             deriving (Show)

type VarID = String
type ClassID = String

data Stmt = Null
          | Assignment VarID Expr
          | ProcCall VarID [Expr]
          | Return (Maybe Expr)
          | If {condition :: Expr, thenCode :: [Block], elseCode :: [Block]}
          | While {condition :: Expr, whileCode :: [Block]}
          | Assert Expr
            deriving (Show)

data Decl = VarDecl {varName :: VarID, varType :: (Maybe ClassID), varExpr :: Expr}
          | ConstDecl VarID Expr
          | FuncDecl {funcName :: VarID, funcParams :: [Param], funcCode :: [Block]}
          | ProcDecl {procName :: VarID, procParams :: [Param], procCode :: [Block]}
          | ClassDecl {className :: ClassID,
                       inherits :: Maybe ClassID,
                       subtypes :: [ClassID],
                       classCode :: [Block]}
            deriving (Show)

data Param = Param {ref :: Bool, paramName :: VarID, pType :: (Maybe ClassID)}
             deriving (Eq, Show)

data Expr = Literal LiteralOp
          | Unary UnaryOp Expr
          | Binary BinaryOp Expr Expr
          | VarID String
          | FuncCall String
          | TypeTest Expr String
            deriving (Show)

data LiteralOp = LiteralInt Int
               | LiteralStr String
               | LiteralBool Bool
               | LiteralNull
                 deriving (Show)

data UnaryOp = Negate            -- ^ integer negation
             | Not               -- ^ boolean negation
               deriving (Show)

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
                deriving (Show)

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
parseStmt = parseReturn
        <|> parseAssert
        <|> parseNull
        <?> "statement"

parseDecl :: CharParser () Decl
parseDecl = parseFuncDecl
        <|> parseVarDecl
        <?> "declaration"

parseVarDecl :: CharParser () Decl
parseVarDecl = do
  reserved "variable"
  n <- parseVarID
  optType <- parseOptionalType
  reservedOp ":="
  e <- parseExpr
  return VarDecl {varName = n, varType = optType, varExpr = e}

parseReturn = do
  reserved "return"
  e <- parseExpr
  return $ Return (Just e)

parseAssert = do
  reserved "assert"
  e <- parseExpr
  return $ Assert e

parseNull = reserved "null" >> return Null

parseExpr = return $ Literal (LiteralInt 2)

validChar = alphaNum <|> char '_'

parseClassID :: CharParser () ClassID
parseClassID = liftM2 (:) upper (lexeme $ many validChar)

parseVarID :: CharParser () VarID
parseVarID = liftM2 (:) lower (lexeme $ many validChar)

parseFuncDecl :: CharParser () Decl
parseFuncDecl = do
  indent <- liftM sourceColumn getPosition
  reserved "function"
  n <- identifier
  ps <- parseParams
  reserved "is"
  c <- return [] --parseCode indent
  return $ FuncDecl {funcName = n, funcParams = ps, funcCode = c}

-- -- | Parse indented code block of a method or class.  Parses all code
-- -- indented greater than n spaces.
-- parseCode :: Int -> CharParser () [HDecl]
-- parseCode n = many1 parseCode' <?> "indented declaration"
--     where
--       parseCode' = do indent <- liftM sourceColumn getPosition
--                       if indent > n then parseDecl else pzero

parseOptionalType :: CharParser () (Maybe ClassID)
parseOptionalType = do reservedOp ":"
                       c <- parseClassID
                       return $ Just c
                <|> return Nothing
  
parseParams = parens (commaSep parseParam)

parseParam = do
  r <- (reserved "ref" >> return True) <|> return False
  v <- parseVarID
  t <- parseOptionalType
  return $ Param {ref = r, paramName = v, pType = t}

hudFunc = "function blah(one, two) is\n  write(1)\n  function nest () is\n    write(2)\n"
       ++ "function joe (three, four) is \n  write(3)\n"
       ++ "write(4)"

pr = putStrLn hudFunc
test = parse parseFile "hudson" hudFunc

