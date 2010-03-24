import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token (LanguageDef, TokenParser)
import qualified Text.ParserCombinators.Parsec.Token as T
import Control.Monad
import Data.Char (isSpace)
-- TODO: Maybe remove the extra level of indirection in HDecl.  The
-- problem is then the parsers aren't completely type safe but I'm not
-- sure if that's necessary or important.  It would create a huge data
-- declaration if the info was created here directly.
data HDecl = Func HFunc
           | Write HWrite
           deriving (Show)

data HFunc = HFunc {name :: HId,
                    params :: HParams,
                    code :: [HDecl]}
           deriving (Show)

data HWrite = HWrite Integer
            deriving (Show)


type HId = String
type HParams = [HId]


-- TODO
-- 
-- * Have a separate lex phase to resolve continuation comments.
-- 
-- * How to handle offside rule?  Maybe add an indentation parameter
-- to parseCode.
--
-- * The lexer built by makeTokenParser ignores newlines.  The current
-- parser is very accepting of malformed input.

parseFile :: CharParser () [HDecl]
parseFile = manyTill parseDecl eof

parseDecl :: CharParser () HDecl
parseDecl = liftM Func parseFunc
        <|> liftM Write parseWrite
        <?> "declaration"

parseFunc :: CharParser () HFunc
parseFunc = do indent <- liftM sourceColumn getPosition
               reserved "function"
               n <- identifier
               ps <- parseParams
               reserved "is"
               c <- parseCode indent
               return HFunc {name = n, params = ps, code = c}

parseWrite :: CharParser () HWrite
parseWrite = do reserved "write"
                n <- parens integer
                return $ HWrite n

-- | Parse indented code block of a method or class.  Parses all code
-- indented greater than n spaces.
parseCode :: Int -> CharParser () [HDecl]
parseCode n = many1 parseCode' <?> "indented declaration"
    where
      parseCode' = do indent <- liftM sourceColumn getPosition
                      if indent > n then parseDecl else pzero

-- TODO: handle optional types and ref indicator
parseParams = parens (commaSep identifier)

hudFunc = "function blah(one, two) is\n  write(1)\n  function nest () is\n    write(2)\n"
       ++ "function joe (three, four) is \n  write(3)\n"
       ++ "write(4)"

pr = putStrLn hudFunc
test = parse parseFile "hudson" hudFunc

-- TODO: The tokenizing helpers should probably be separated in its
-- own module.

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
lexer = let newLineLexer = T.makeTokenParser hudsonDef in
        newLineLexer
        { T.whiteSpace = skipMany (simpleHorizontalSpace <|> oneLineComment <?> "")
        -- Override the whiteSpace definition to not consume newlines
        -- because they are significant in Hudson.

        }
        
-- Copied from Parsec.Token source.
oneLineComment = do try (string (T.commentLine hudsonDef))
                    skipMany (satisfy (/= '\n'))
                    return ()

-- Copied and modified from Parsec.Token source.
simpleHorizontalSpace = skipMany1 (satisfy isHorizontalSpace)
isHorizontalSpace c = c /= '\r' && c /= '\n' && isSpace c

reserved = T.reserved lexer
identifier = T.identifier lexer
commaSep = T.commaSep lexer
parens = T.parens lexer
integer = T.integer lexer
whiteSpace = T.whiteSpace lexer          