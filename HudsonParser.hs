module HudsonParser where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative ((<*))
import HudsonToken
import HudsonPreproc (normalize)
data Block = BlockStmt Stmt
           | BlockDecl Decl
             deriving (Eq, Show)

type VarID = String
type ClassID = String

data Stmt = Assignment VarID Expr
          | ProcCall VarID [Expr]
          | If {ifCond :: Expr, thenCode :: [Block], elseCode :: [Block]}
          | While {whileCond :: Expr, whileCode :: [Block]}
          | Return (Maybe Expr)
          | Assert Expr
          | Null
            deriving (Eq, Show)

data Decl = VarDecl {varName :: VarID, varType :: (Maybe ClassID), varExpr :: Expr}
          | ConstDecl {constName :: VarID, constType :: (Maybe ClassID), constExpr :: Expr}
          | FuncDecl {funcName :: VarID, funcParams :: [Param], funcCode :: [Block]}
          | ProcDecl {procName :: VarID, procParams :: [Param], procCode :: [Block]}
          | ClassDecl {className :: ClassID,
                       inherit :: Maybe ClassID,
                       subtypes :: [ClassID],
                       classCode :: [Block]}
            deriving (Eq, Show)

data Param = Param {ref :: Bool, paramName :: VarID, paramType :: (Maybe ClassID)}
             deriving (Eq, Show)

data Expr = LiteralInt Integer
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
-- Have a separate scanner to resolve continuation comments and
-- indentation.
--
-- Allow single line function and procedure declarations.
--
-- Add more error messages.
--
-- Create expression parser.
parseFile fname = do
  input <- readFile fname
  return $ parseInput input

-- parseInput :: String -> [Block]
parseInput s = ps
    where s' = maybe "" id (normalize s)
          ps = parse (manyTill parseBlock eof) "" s'

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

parseAssign = try $ do
  v <- varIdentifier
  reservedOp ":="
  e <- parseExpr
  newline
  return $ Assignment v e

parseProcCall = try $ do
  p <- varIdentifier
  ps <- parens (commaSep parseExpr)
  newline
  return $ ProcCall p ps

parseIf = do
  indent <- getIndent
  parseIf' indent

parseIf' n = do
  reserved "if"
  cond <- parseExpr
  reserved "then"
  ifCode <- parseCode n
  elseCode <- parseElse n
  return $ If cond ifCode elseCode

parseElse indent = do
  reserved "else"
  -- Special case the if statement because the indentation should match
  -- that of the enclosing else block, not the new if statment.
  enclosedIf <|> parseCode indent
      where enclosedIf = liftM ((:[]) . BlockStmt) (parseIf' indent)

parseWhile = do
  indent <- getIndent
  reserved "while"
  cond <- parseExpr
  reserved "do"
  code <- parseCode indent
  return $ While cond code
          

parseReturn = do
  reserved "return"
  e <- try (liftM Just parseExpr) <|> (whiteSpace >> return Nothing)
  newline
  return $ Return e

parseAssert = do
  reserved "assert"
  e <- parseExpr
  newline
  return $ Assert e

parseNull = reserved "null" >> newline >> return Null

-- Declarations

parseVarDecl = do
  reserved "variable"
  n <- varIdentifier
  optType <- parseOptionalType
  reservedOp ":="
  e <- parseExpr
  newline
  return VarDecl {varName = n, varType = optType, varExpr = e}

parseConstDecl = do
  reserved "constant"
  n <- varIdentifier
  optType <- parseOptionalType
  reservedOp ":="
  e <- parseExpr
  newline
  return ConstDecl {constName = n, constType = optType, constExpr = e}

parseFuncDecl = do
  indent <- getIndent
  reserved "function"
  n <- varIdentifier
  ps <- parseParams
  reserved "is"
  c <- parseCode indent
  return FuncDecl {funcName = n, funcParams = ps, funcCode = c}

parseProcDecl = do
  indent <- getIndent
  reserved "procedure"
  n <- varIdentifier
  ps <- parseParams
  reserved "is"
  c <- parseCode indent
  return ProcDecl {procName = n, procParams = ps, procCode = c}

parseClassDecl = do
  indent <- getIndent
  reserved "class"
  n <- classIdentifier
  i <- liftM Just (reserved "inherit" >> classIdentifier) <|> return Nothing
  s <- (reservedOp "<" >> commaSep1 classIdentifier) <|> return []
  reserved "is"
  c <- parseCode indent
  return ClassDecl {className = n, inherit = i, subtypes = s, classCode = c}

-- Params
parseParams = parens (commaSep parseParam)

parseParam = do
  r <- (reserved "ref" >> return True) <|> return False
  v <- varIdentifier
  t <- parseOptionalType
  return Param {ref = r, paramName = v, paramType = t}

-- Expressions
parseExpr = liftM LiteralInt integer

parseOptionalType :: CharParser () (Maybe ClassID)
parseOptionalType = try (liftM Just (colon >> classIdentifier))
                <|> return Nothing

-- | Parse indented code block of a method or class.  Parses all code
-- indented greater than n spaces.
parseCode :: Int -> CharParser () [Block]
parseCode n = singleStmt <|> multipleStmts
    where
      singleStmt = liftM (:[]) parseBlock
      multipleStmts = newline >> parseMultStmts n

parseMultStmts n = do
  spaces
  indent <- getIndent
  if indent <= n
     then fail "need indented declaration"
     else many1 (parseCode' indent) <?> "indented declaration"
    where
      parseCode' m = do spaces
                        indent <- getIndent
                        if indent >= m
                           then parseBlock <?> "gubba, gubba"
                           else pzero

hudFunc = "function blah(one, two) is\n  write(1)\n  function nest () is\n    write(2)\n"
       ++ "function joe (three, four) is \n  write(3)\n"
       ++ "write(4)"

pr = putStrLn hudFunc
t = parseFile "test1.hud"

pt a b = parse a "h" b

getIndent = liftM sourceColumn getPosition