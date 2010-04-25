{-# LANGUAGE FlexibleContexts #-}

module HudsonParser where

import HudsonScanner

import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Applicative ((<*), (*>), (<*>), (<$>), liftA)

import Data.List (foldl', intercalate)
import Text.Parsec ((<?>), (<|>))
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Printf

data Block = BlockStmt Stmt
           | BlockDecl Decl
             deriving (Eq, Show)

type VarID = String
type ClassID = String

data Stmt = Assignment Expr Expr
          | ProcCall VarID [Expr]
          | ObjCall Expr
          | If {ifCond :: Expr, thenCode :: [Block], elseCode :: [Block]}
          | While {whileCond :: Expr, whileCode :: [Block]}
          | Return (Maybe Expr)
          | Assert Expr
          | Null
            deriving (Eq, Show)

data Decl = VarDecl { varName :: VarID
                    , varType :: Maybe ClassID
                    , varExpr :: Maybe Expr
                    }
          | ConstDecl { constName :: VarID
                      , constType :: Maybe ClassID
                      , constExpr :: Maybe Expr
                      }
          | FuncDecl { funcName :: VarID
                     , funcParams :: [Param]
                     , funcCode :: [Block]
                     }
          | ProcDecl { procName :: VarID
                     , procParams :: [Param]
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
          | LiteralThis
          | ClassLookup ClassID
          | Negate Expr
          | Not Expr
          | Binary BinaryOp Expr Expr
          | VarLookup VarID
          | FuncCall VarID [Expr]
          | ClassCall ClassID [Expr]
          | ParenExpr Expr
          | ObjFieldExpr { fieldName :: VarID
                         , fieldObjExpr :: Expr
                         }
          | ObjMethodExpr { methodName :: VarID
                          , methodActuals :: [Expr]
                          , methodObjExpr :: Expr
                          }
          | LambdaExpr { lambdaParams :: [Param]
                       , lambdaExpr :: Expr
                       }
            deriving (Eq, Show)

data ObjAST = ObjFieldLookup VarID
            | ObjMethodCall VarID [Expr]
              deriving (Eq, Show)

data BinaryOp = Add
              | Sub
              | Mult
              | Div
              | Mod
              | Equal
              | NotEqual
              | LessThan
              | TypeTest  -- TODO: Should the right be a classID?
              | LessThanEqual
              | GreaterThan
              | GreaterThanEqual
              | And
              | Or
              | Concat
                deriving (Enum, Eq, Show)

type Parser t = Parsec [Token] () t


parseFile fname = do
  ss <- readFile fname
  return $ parseString' fname ss

parseString s = parseString' "" s

parseString' fname s = 
  case tokenizeString' fname s of
    Left err -> Left (show err)
    Right xs ->
        case parse parseBlocks fname xs of
          Left err -> Left (show err)
          Right ps -> Right ps

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

lowerOrObjID = lowerID <|> objMemberID

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
parseStmt = choice [parseAssign, parseObjCall, parseProcCall, parseIf,
                    parseWhile, parseReturn, parseAssert, parseNull]
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

parseAssign = try (do v <- parseExpr
                      separator AssignSep
                      e <- parseExpr
                      return $ Assignment v e
                  )
              <?> "assignment"

parseActuals = parens (commaSep parseExpr) <?> "actual parameters"

parseProcCall = try (do p <- lowerOrObjID
                        ps <- parseActuals
                        return $ ProcCall p ps
                    )
                <?> "procedure call"

parseObjCall = try (return ObjCall <*> parseObjExpr) <?> "object call"

-- TODO: Layout for `else statement` not strictly enforced following a
-- block `if statement`.
parseIf = do reserved IfKW
             cond <- parseExpr
             reserved ThenKW
             ifCode <- parseCode
             elseCode <- try (optional newline >> reserved ElseKW >> parseCode)
                      <|> return [] <?> "else block"
             return $ If cond ifCode elseCode
          <?> "if statement"

parseCode = (many1 newline >> between indent outdent parseBlocks)
        <|> (parseBlock <:> return [])
        <?> "indented code block or single statement"

parseWhile = do reserved WhileKW
                cond <- parseExpr
                reserved DoKW
                code <- parseCode
                return $ While cond code
             <?> "while loop"
 
parseReturn = do reserved ReturnKW
                 e <- option Nothing $ try (liftM Just parseExpr)
                 return $ Return e
              <?> "return statement"

parseAssert = do reserved AssertKW
                 e <- parseExpr
                 return $ Assert e
              <?> "assert statement"

parseNull = reserved NullKW >> return Null <?> "null keyword"

-- Declarations

parseVarDecl = do reserved VariableKW
                  n <- lowerOrObjID
                  optType <- parseOptionalType
                  e <- parseOptionalExpr
                  return VarDecl {varName = n, varType = optType, varExpr = e}
               <?> "variable declaration"

parseConstDecl = do reserved ConstantKW
                    n <- lowerOrObjID
                    optType <- parseOptionalType
                    e <- parseOptionalExpr
                    return ConstDecl {constName = n, constType = optType, constExpr = e}
                 <?> "constant declaration"

parseOptionalExpr = liftM Just (separator AssignSep >> parseExpr)
                <|> return Nothing
                <?> "optional expression"

parseFuncDecl = do reserved FunctionKW
                   n <- lowerOrObjID
                   ps <- parseParams
                   reserved IsKW
                   c <- parseCode
                   return FuncDecl {funcName = n, funcParams = ps, funcCode = c}
                <?> "function declaration"

parseProcDecl = do reserved ProcedureKW
                   n <- lowerOrObjID
                   ps <- parseParams
                   reserved IsKW
                   c <- parseCode
                   return ProcDecl {procName = n, procParams = ps, procCode = c}
                <?> "procedure declaration"

parseClassDecl = do reserved ClassKW
                    n <- upperID
                    inherit <- liftM Just (reserved InheritKW >> upperID)
                            <|> return Nothing <?> "parent class"
                    subtypes <- (operator LessOp >> commaSep1 upperID)
                            <|> return [] <?> "subtypes"
                    reserved IsKW
                    c <- parseCode
                    return ClassDecl {className = n, inherit = inherit,
                                      subtypes = subtypes, classCode = c}
                 <?> "class declaration"

parseParams = parens (commaSep parseParam) <?> "parameters"

parseParam = do r <- (reserved RefKW >> return True) <|> return False
                v <- lowerID
                t <- parseOptionalType
                return Param {ref = r, paramName = v, paramType = t}
             <?> "parameter"

parseOptionalType = try (liftM Just (tokenString <$>
                                     separator ColonSep >> upperID))
                <|> return Nothing
                <?> "optional type"


-- Expressions
parseExpr = buildExpressionParser table term <?> "expression"

-- | Parse an expression that uses object methods or fields.  Start by
-- parsing the subset of expressions that may have an object field or
-- method.  We use a subset to avoid infinite recursion.  Then parse
-- one or more object method calls or fields.  Finally, traverse the
-- gathered calls and fields wrapping each around the previous item to
-- build an expression.
parseObjExpr = try (do e <- parens exprSubset <|> exprSubset 
                       os <- many1 (parseObjMethodCall <|> parseObjFieldLookup)
                       return $ foldl' f e os
                   )
               <?> "object expression"
    where f :: Expr -> ObjAST -> Expr
          f expr (ObjMethodCall n ps) = ObjMethodExpr n ps expr
          f expr (ObjFieldLookup n)   = ObjFieldExpr n expr
          -- TODO: Did we miss any other expressions that can have
          -- object fields or methods.
          exprSubset = choice [parseFuncCall, parseClassCall, parseThis,
                               parseClassLookup, parseVarLookup]

parseObjMethodCall = try (do n <- objMemberID
                             ps <- parseActuals
                             return $ ObjMethodCall n ps
                         )
                     <?> "object method call"

parseObjFieldLookup = (return ObjFieldLookup <*> objMemberID) <?> "object field"

term = parseLambdaExpr
   <|> parseObjExpr
   <|> parseFuncCall
   <|> parseThis
   <|> parseTrue
   <|> parseFalse
   <|> parseClassCall
   <|> parseClassLookup
   <|> parseExprNull
   <|> parseVarLookup
   <|> parseInteger
   <|> parseLiteralString
   <|> parseParenExpr
   <?> "simple expresion"

table   = [ [binaryLeft (TypeTestOp, TypeTest)]
          , [prefix MinusOp Negate, prefixKW NotKW Not]
          , [binaryLeft es | es <- [(MultiplyOp, Mult), (DivideOp, Div),
                                    (ModulusOp, Mod)]]
          , [binaryLeft es | es <- [(PlusOp, Add), (MinusOp, Sub),
                                    (ConcatenateOp, Concat)]]
          , [binaryLeft es | es <- [(EqualityOp, Equal), (NotEqOp, NotEqual),
                                    (LessOp, LessThan),
                                    (LessEqOp, LessThanEqual),
                                    (GreaterEqOp, GreaterThanEqual),
                                    (GreaterOp, GreaterThan)]]
          , [binaryKWLeft AndKW And]
          , [binaryKWLeft OrKW Or]
          ]

parseClassLookup = try $ return ClassLookup <*> upperID

parseClassCall = try (do c <- upperID
                         ps <- parens (commaSep parseExpr)
                         return $ ClassCall c ps
                     )
                 <?> "class call"

binaryKW name fun assoc = Infix (do{ reserved name; return $ Binary fun }) assoc
binaryKWLeft name fun = binaryKW name fun AssocLeft

binary name fun assoc = Infix (do{ operator name; return $ Binary fun }) assoc
binaryLeft (name, fun) = binary name fun AssocLeft

prefixKW name fun = Prefix (reserved name >> return fun)
prefix name fun = Prefix (operator name >> return fun)

parseLambdaExpr = do reserved FunKW
                     ps <- parseParams
                     reserved IsKW
                     e <- parseExpr
                     return LambdaExpr {lambdaParams = ps, lambdaExpr = e}
                  <?> "fun (lambda) expression"

parseFuncCall = try (do p <- lowerOrObjID
                        ps <- parens (commaSep parseExpr) <?> "expression"
                        return $ FuncCall p ps
                    )
                <?> "function call"

parseVarLookup = (try $ return VarLookup <*> lowerOrObjID)
             <?> "variable or constant lookup"
  
parseInteger = do (NumberTok n, _) <- numberTag
                  return $ LiteralInt n
              <?> "integer"

parseLiteralString = do (StringTok s, _) <- string
                        return $ LiteralStr s
                     <?> "literal string"

parseParenExpr = return ParenExpr <*> parens parseExpr

parseTrue = reserved TrueKW >> (return $ LiteralBool True) <?> "true"
parseFalse = reserved FalseKW >> (return $ LiteralBool False) <?> "false"
parseExprNull = reserved NullKW >> (return $ LiteralNull) <?> "null"
parseThis = reserved ThisKW >> (return $ LiteralThis) <?> "this"  
pt p s = parse p "" <$> tokenizeString s
ts = tokenizeString


ifs = "if true then\n\
      \   e()\n\
      \else f()"
