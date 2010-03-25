module HudsonToken where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char (isSpace)

-- TODO: The tokenizing helpers should probably be separated in its
-- own module.

hudsonStyle :: T.LanguageDef st
hudsonStyle = T.LanguageDef       -- TODO: was emptyDef removed?
                { T.commentStart    = ""
                , T.commentEnd      = ""
                , T.commentLine     = "#"
                , T.nestedComments  = False
                , T.identStart      = letter <|> char '.'
                , T.identLetter     = alphaNum <|> char '_'
                , T.opStart         = T.opLetter hudsonStyle
                , T.opLetter        = oneOf ":%*+/<=>?^-"
                , T.reservedNames   = []
                , T.reservedOpNames = []
                , T.caseSensitive   = True
                }

hudsonDef :: T.LanguageDef st
hudsonDef = hudsonStyle
            { T.reservedOpNames = [":=", "?"]
            , T.reservedNames = ["and", "assert", "class", "constant", "do", "else",
                                 "false", "fun", "function", "if", "inherit", "is",
                                 "not", "null", "or", "procedure", "ref", "return",
                                 "then", "this", "true", "variable", "while"]
            }

lexer :: T.TokenParser st
lexer = let newLineLexer = T.makeTokenParser hudsonDef in
        newLineLexer
        -- TODO: still consumes newlines
        { T.whiteSpace = whiteSpace,
          T.lexeme = lexeme
        -- Override the whiteSpace definition to not consume newlines
        -- because they are significant in Hudson.

        }

-- Copied from Parsec.Token source.
oneLineComment = do try (string (T.commentLine hudsonDef))
                    skipMany (satisfy (/= '\n'))
                    return ()

-- Copied and modified from Parsec.Token source.
simpleHorizontalSpace = skipMany1 (satisfy isHorizontalSpace)
hspace = skipMany (satisfy isHorizontalSpace)
isHorizontalSpace c = c /= '\r' && c /= '\n' && isSpace c

reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
identifier = T.identifier lexer
commaSep = T.commaSep lexer
parens = T.parens lexer
integer = T.integer lexer
whiteSpace = skipMany (simpleHorizontalSpace <|> oneLineComment <?> "")
lexeme p = do {x <- p; T.whiteSpace; return x}
