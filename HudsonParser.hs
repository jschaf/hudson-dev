{-# LANGUAGE FlexibleContexts #-}

module HudsonParser where

import HudsonScanner

import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Applicative ((<*), (<$>), liftA)

import Text.Parsec ((<?>), (<|>))
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos
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

data Decl = VarDecl { varName :: VarID
                    , varType :: (Maybe ClassID)
                    , varExpr :: Expr
                    }
          | ConstDecl { constName :: VarID
                      , constType :: (Maybe ClassID)
                      , constExpr :: Expr
                      }
          | FuncDecl { funcName :: VarID
                     , funcParams :: [Param]
                     , funcCode :: [Block]
                     }
          | ProcDecl { procName :: VarID, procParams :: [Param]
                     , procCode :: [Block]
                     }
          | ClassDecl { className :: ClassID
                      , inherit :: Maybe ClassID
                      , subtypes :: [ClassID]
                      , classCode :: [Block]
                      }
            deriving (Eq, Show)

data Param = Param { ref :: Bool
                   , paramName :: VarID
                   , paramType :: (Maybe ClassID)
                   }
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

type Parser t = Parsec [Token] () t

-- TODO
--
-- Create expression parser.

parseFile fname = do
  ts <- tokenizeHudsonFile fname
  return $ parse parseBlocks fname <$> ts
  -- case ts of
  --   Left err -> print err
  --   Right xs -> print (removeJunk xs)


tokenP :: (Stream [Token] Identity Token) => Token -> Parser Token
tokenP x = token showTok posFromTok testTok
    where
      showTok (t , pos) = show t
      posFromTok (t, pos)  = pos
      testTok tok = if equal x tok then Just tok else Nothing

-- TODO: This is ugly
equal (NumberTok _, _)       (NumberTok _, _)      = True
equal (ReservedTok s, _)     (ReservedTok s', _)   = s == s'
equal (OperatorTok o, _)     (OperatorTok o', _)   = o == o'
equal (SeparatorTok s, _)    (SeparatorTok s', _)  = s == s'
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
withEmpty   = flip (,) emptyPos

numberTag   = tokenP (withEmpty $ NumberTok 0) <?> "integer"
reserved s  = tokenP (withEmpty $ ReservedTok s) <?> "keyword " ++ keywordString s
operator o  = tokenP (withEmpty $ OperatorTok o) <?> "operator"
separator s = tokenP (withEmpty $ SeparatorTok s) <?> "separator"
string      = tokenP (withEmpty $ StringTok "" ) <?> "string"
upperID     = tokenString <$> tokenP (withEmpty $ UpperIDTok "" ) <?> "upperID"
lowerID     = tokenString <$> tokenP (withEmpty $ LowerIDTok "" ) <?> "lowerID"
objMemberID = tokenString <$> tokenP (withEmpty $ ObjMemberIDTok "" ) <?> "objMemberID"
contComment = tokenP (withEmpty $ ContCommentTok "" ) <?> "contComment"
comment     = tokenP (withEmpty $ CommentTok "" ) <?> "comment"
indent      = tokenP (withEmpty IndentTok) <?> "indent"
outdent     = tokenP (withEmpty OutdentTok) <?> "outdent"
newline     = tokenP (withEmpty NewlineTok) <?> "newline"
junk        = tokenP (withEmpty JunkTok) <?> "junk"

parens = between (separator LParenSep) (separator RParenSep)
commaSep = flip sepBy (separator CommaSep)
commaSep1 = flip sepBy1 (separator CommaSep)

parseBlocks :: Parser [Block]
parseBlocks = (many newline >> sepEndBy1 parseBlock (many newline)) <?> "general error"

parseBlock :: Parser Block
parseBlock = liftM BlockStmt parseStmt
         <|> liftM BlockDecl parseDecl
         <?> "declaration or statemnt"

parseStmt :: Parser Stmt
parseStmt = choice [parseAssign, parseProcCall, parseIf, parseWhile,
                    parseReturn, parseAssert, parseNull]
        <?> "statement"

parseDecl :: Parser Decl
parseDecl = choice [parseVarDecl, parseConstDecl, parseFuncDecl, parseProcDecl,
                    parseClassDecl]
        <?> "declaration"

-- TODO: Ugly and not typesafe
tokenString :: Token -> String
tokenString (StringTok s, _) = s
tokenString (UpperIDTok s, _) = s
tokenString (LowerIDTok s, _) = s
tokenString (ObjMemberIDTok s, _) = s
tokenString (ContCommentTok s, _) = s
tokenString (CommentTok s, _) = s
tokenString _ = error "Can't make a string from a non-string token."

parseAssign = try $ do
  v <- lowerID
  separator AssignSep
  e <- parseExpr
  return $ Assignment v e

parseProcCall = try $ do
  p <- lowerID
  ps <- parens (commaSep parseExpr) <?> "expression"
  return $ ProcCall p ps

parseIf = do
  reserved IfKW
  cond <- parseExpr
  reserved ThenKW
  ifCode <- parseCode
  elseCode <- (reserved ElseKW >> parseCode) <|> return [] <?> "else block"
  return $ If cond ifCode elseCode

parseCode = (newline >> between indent outdent parseBlocks)
        <|> (parseBlock <:> return [])
        <?> "indented code block or single statement"

parseWhile = do
  reserved WhileKW
  cond <- parseExpr
  reserved DoKW
  code <- parseCode
  return $ While cond code

parseReturn = do
  reserved ReturnKW
  e <- option Nothing $ try (liftM Just parseExpr)
  return $ Return e

parseAssert = do
  reserved AssertKW
  e <- parseExpr
  return $ Assert e

parseNull = reserved NullKW >> return Null

-- Declarations

parseVarDecl = do
  reserved VariableKW
  n <- lowerID
  optType <- parseOptionalType
  separator AssignSep
  e <- parseExpr
  return VarDecl {varName = n, varType = optType, varExpr = e}

parseConstDecl = do
  reserved ConstantKW
  n <- lowerID
  optType <- parseOptionalType
  separator AssignSep
  e <- parseExpr
  return ConstDecl {constName = n, constType = optType, constExpr = e}

parseFuncDecl = do
  reserved FunctionKW
  n <- lowerID
  ps <- parseParams
  reserved IsKW
  c <- parseCode
  return FuncDecl {funcName = n, funcParams = ps, funcCode = c}

parseProcDecl = do
  reserved ProcedureKW
  n <- lowerID
  ps <- parseParams
  reserved IsKW
  c <- parseCode
  return ProcDecl {procName = n, procParams = ps, procCode = c}

parseClassDecl = do
  reserved ClassKW
  n <- upperID
  inherit <- liftM Just (reserved InheritKW >> upperID)
          <|> return Nothing <?> "parent class"
  subtypes <- (operator GreaterOp >> commaSep1 upperID)
          <|> return [] <?> "subtypes"
  reserved IsKW
  c <- parseCode
  return ClassDecl {className = n, inherit = inherit,
                    subtypes = subtypes, classCode = c}

parseParams = parens (commaSep parseParam)

parseParam = do
  r <- (reserved RefKW >> return True) <|> return False
  v <- lowerID
  t <- parseOptionalType
  return Param {ref = r, paramName = v, paramType = t}

-- -- Expressions
parseExpr = liftM LiteralInt pInteger

pInteger = do
  (NumberTok n, _) <- numberTag
  return n

parseOptionalType = try (liftM Just (tokenString <$>
                                     separator ColonSep >> upperID))
                <|> return Nothing

pt p s = parse p "" <$> tokenizeString s
ts = tokenizeString


