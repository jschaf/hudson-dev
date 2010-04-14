{-# LANGUAGE FlexibleContexts #-}

module HudsonParser where

import HudsonScanner

import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Applicative ((<*))

import Text.Parsec ((<?>), (<|>))
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.String
import Text.Printf

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

type Parser = Parsec [Token] ()

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
  ts <- tokenizeHudsonFile fname
  case ts of
    Left err -> print err
    Right xs -> print (removeJunk xs)


tokenP :: (Stream [Token] Identity Token) => Token -> Parsec [Token] () Token
tokenP x = token showTok posFromTok testTok
    where
      showTok :: Token -> String
      showTok (t , pos) = show t
      posFromTok :: Token -> SourcePos
      posFromTok (t, pos)  = pos
      testTok :: Token -> Maybe Token
      testTok tok = if equal x tok then Just tok else Nothing


equal (NumberTok _, _)       (NumberTok _, _)      = True
equal (ReservedTok s, _)     (ReservedTok s', _)    = s == s'
equal (OperatorTok o, _)     (OperatorTok o', _)    = o == o'
equal (SeparatorTok s, _)    (SeparatorTok s', _)   = s == s'
equal (StringTok _, _)       (StringTok _, _)      = True
equal (UpperIDTok _, _)      (UpperIDTok _, _)     = True
equal (LowerIDTok _, _)      (LowerIDTok _, _)     = True
equal (ObjMemberIDTok _, _)  (ObjMemberIDTok _, _) = True
equal (ContCommentTok _, _)  (ContCommentTok _, _) = True
equal (CommentTok _, _)      (CommentTok _, _)     = True
equal (IndentTok, _)         (IndentTok, _)        = True
equal (OutdentTok, _)        (OutdentTok, _)       = True
equal (NewlineTok, _)        (NewlineTok, _)       = True
equal (JunkTok, _)           (JunkTok, _)          = True
equal _ _                                          = False

emptyPos = newPos "" 0 0
withEmpty = flip (,) emptyPos
numberTag = tokenP . withEmpty $ NumberTok 0
reserved s = tokenP . withEmpty $ ReservedTok s
operator o = tokenP . withEmpty $ OperatorTok o
string = tokenP . withEmpty $ StringTok "" 
upperID = tokenP . withEmpty $ UpperIDTok "" 
lowerID = tokenP . withEmpty $ LowerIDTok "" 
objMemberID = tokenP . withEmpty $ ObjMemberIDTok "" 
contComment = tokenP . withEmpty $ ContCommentTok "" 
comment = tokenP . withEmpty $ CommentTok "" 
indent = tokenP . withEmpty $ IndentTok
outdent = tokenP . withEmpty $ OutdentTok
newline = tokenP . withEmpty $ NewlineTok
junk = tokenP . withEmpty $ JunkTok

-- parseBlocks :: [Token] -> Parser [Block]
-- parseBlocks = manyTill parseBlock eof

-- parseBlock :: Parser Block
-- parseBlock = liftM BlockStmt parseStmt
         -- <|> liftM BlockDecl parseDecl
         -- <?> "declaration or statemnt"

-- parseStmt :: Parser Stmt
-- parseStmt = parseAssign
--         <|> parseProcCall
--         <|> parseIf
        -- <|> parseWhile
        -- <|> parseReturn
        -- <|> parseAssert
        -- <|> parseNull
        -- <?> "statement"

-- parseDecl :: Parser Decl
-- parseDecl = parseVarDecl
--         <|> parseConstDecl
--         <|> parseFuncDecl
--         <|> parseProcDecl
--         <|> parseClassDecl
--         <?> "declaration"

-- parseAssign = try $ do
--   v <- varIdentifier
--   reservedOp ":="
--   e <- parseExpr
--   newline
--   return $ Assignment v e

-- spaces :: (Stream s m CharPos) => ParsecT s u m [CharPos]

-- upperID :: (Stream s m Token) => ParsecT s u m String
-- upperID = myToken UpperID


-- token' x = token showTok posFromTok testTok
--     where
--       showTok (pos,t)     = show t
--       posFromTok (pos,t)  = pos
--       testTok (pos,t)     = if x == t then Just t else Nothing

-- blah = token' "a"

-- parseProcCall = try $ do
--   p <- varIdentifier
--   ps <- parens (commaSep parseExpr)
--   newline
--   return $ ProcCall p ps

-- parseIf = do
--   indent <- getIndent
--   parseIf' indent
           
-- parseIf' n = do
--   reserved "if"
--   cond <- parseExpr
--   reserved "then"
--   ifCode <- parseCode n
--   elseCode <- parseElse n <|> return []
--   return $ If cond ifCode elseCode

-- parseElse indent = do
--   reserved "else"
--   -- Special case the if statement because the indentation should match
--   -- that of the enclosing else block, not the new if statment.
--   enclosedIf <|> parseCode indent
--       where enclosedIf = liftM ((:[]) . BlockStmt) (parseIf' indent)


-- -- | Parse indented code block of a method or class.  Parses all code
-- -- indented greater than n spaces.
-- parseCode :: Int -> Parser [Block]
-- parseCode n = multipleStmts <|> singleStmt
--     where
--       singleStmt = liftM (:[]) parseBlock
--       multipleStmts = newline >> parseMultStmts n

-- parseMultStmts n = do
--   whiteSpace
--   indent <- getIndent
--   if indent <= n
--      then fail "need indented declaration"
--      else many1 (parseCode' indent) <?> "indented declaration"
--     where
--       parseCode' m = do whiteSpace
--                         indent <- getIndent
--                         if indent >= m
--                            then parseBlock
--                            else pzero

-- parseWhile = do
--   indent <- getIndent
--   reserved "while"
--   cond <- parseExpr
--   reserved "do"
--   code <- parseCode indent
--   return $ While cond code
          
-- parseReturn = do
--   reserved "return"
--   e <- option Nothing $ try (liftM Just parseExpr)
--   newline
--   return $ Return e

-- parseAssert = do
--   reserved "assert"
--   e <- parseExpr
--   newline
--   return $ Assert e

-- parseNull = reserved "null" >> newline >> return Null

-- -- Declarations

-- parseVarDecl = do
--   reserved "variable"
--   n <- varIdentifier
--   optType <- parseOptionalType
--   reservedOp ":="
--   e <- parseExpr
--   newline
--   return VarDecl {varName = n, varType = optType, varExpr = e}

-- parseConstDecl = do
--   reserved "constant"
--   n <- varIdentifier
--   optType <- parseOptionalType
--   reservedOp ":="
--   e <- parseExpr
--   newline
--   return ConstDecl {constName = n, constType = optType, constExpr = e}

-- parseFuncDecl = do
--   indent <- getIndent
--   reserved "function"
--   n <- varIdentifier
--   ps <- parseParams
--   reserved "is"
--   c <- parseCode indent
--   return FuncDecl {funcName = n, funcParams = ps, funcCode = c}

-- parseProcDecl = do
--   indent <- getIndent
--   reserved "procedure"
--   n <- varIdentifier
--   ps <- parseParams
--   reserved "is"
--   c <- parseCode indent
--   return ProcDecl {procName = n, procParams = ps, procCode = c}

-- parseClassDecl = do
--   indent <- getIndent
--   reserved "class"
--   n <- classIdentifier
--   i <- liftM Just (reserved "inherit" >> classIdentifier) <|> return Nothing
--   s <- (reservedOp "<" >> commaSep1 classIdentifier) <|> return []
--   reserved "is"
--   c <- parseCode indent
--   return ClassDecl {className = n, inherit = i, subtypes = s, classCode = c}

-- -- Params
-- parseParams = parens (commaSep parseParam)

-- parseParam = do
--   r <- (reserved "ref" >> return True) <|> return False
--   v <- varIdentifier
--   t <- parseOptionalType
--   return Param {ref = r, paramName = v, paramType = t}

-- -- Expressions
-- parseExpr = liftM LiteralInt integer

-- parseOptionalType :: Parser (Maybe ClassID)
-- parseOptionalType = try (liftM Just (colon >> classIdentifier))
--                 <|> return Nothing

-- hudFunc = "function blah(one, two) is\n  write(1)\n  function nest () is\n    write(2)\n"
--        ++ "function joe (three, four) is \n  write(3)\n"
--        ++ "write(4)"

-- pr = putStrLn hudFunc
-- t = parseFile "test1.hud"

-- pt a b = parse a "h" b

-- getIndent = liftM sourceColumn getPosition
