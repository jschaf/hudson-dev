import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token (LanguageDef, TokenParser, lexeme)
import qualified Text.ParserCombinators.Parsec.Token as T
import Control.Monad

-- TODO: Maybe remove the extra level of indirection with HDeclFunc.
-- The problem is then the parsers aren't type safe but I'm not sure
-- if that's important.
data HDecl = HDeclFunc HFunc
           | HDeclWrite HWrite
           deriving (Show)

data HFunc = HFunc {name :: HId,
                    params :: HParams,
                    code :: [HDecl]}
           deriving (Show)

data HWrite = HWrite Integer
            deriving (Show)

type HId = String
type HParams = [HId]

-- Ideas
-- 1. Have a separate lex phase to resolve continuation comments

parseDecl :: CharParser () HDecl
parseDecl = do liftM HDeclFunc parseFunc
           <|> liftM HDeclWrite parseWrite
           <?> "declaration"

parseFunc :: CharParser () HFunc
parseFunc = do reserved "function"
               n <- identifier
               ps <- parseParams
               reserved "is"
               c <- parseCode 0
               return HFunc {name = n, params = ps, code = c}

parseWrite :: CharParser () HWrite
parseWrite = do reserved "write"
                n <- parens integer
                return $ HWrite n

parseCode :: Int -> CharParser () [HDecl]
parseCode n = do 
                 optional eof
                 m <- liftM sourceColumn getPosition
                 
                 if m > n then
                     do d <- parseDecl
                        ds <- parseCode n
                        return (d:ds)
                  else return []

parseParams = parens (commaSep identifier)

hudFunc = "function blah(one, two) is\n  write(1) "
hudWrite = "write(12)"

test = parse parseDecl "hudson"
t = test hudFunc

-- Tokenizing Stuff - This should probably be separated into it's own
-- source file

hudsonStyle :: LanguageDef st
hudsonStyle = T.LanguageDef       -- TODO: was emptyDef removed?
                { T.commentStart    = ""
                , T.commentEnd      = ""
                , T.commentLine     = "#"
                , T.nestedComments  = False
                , T.identStart      = letter <|> char '.'
                , T.identLetter     = alphaNum <|> oneOf "._"
                , T.opStart         = T.opLetter hudsonStyle
                , T.opLetter        = oneOf ":%*+/<=>?^-"
                , T.reservedNames   = []
                , T.reservedOpNames = []
                , T.caseSensitive   = True
                }

hudsonDef :: LanguageDef st
hudsonDef = hudsonStyle
            { T.reservedOpNames = [":=", "?"]
            , T.reservedNames = ["and", "assert", "class", "constant", "do", "else",
                                 "false", "fun", "function", "if", "inherit", "is",
                                 "not", "null", "or", "procedure", "ref", "return",
                                 "then", "this", "true", "variable", "while"]
            }

lexer :: TokenParser st
lexer = T.makeTokenParser hudsonDef

reserved = T.reserved lexer
identifier = T.identifier lexer
commaSep = T.commaSep lexer
parens = T.parens lexer
integer = T.integer lexer
whiteSpace = T.whiteSpace lexer          