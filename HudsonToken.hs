module HudsonToken where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char (isAlpha, toLower, toUpper, isSpace, digitToInt)
import Data.List (sort, elem)
import Control.Monad

hudsonDef :: T.LanguageDef st
hudsonDef = T.LanguageDef       -- TODO: was emptyDef removed?
                { T.commentStart    = ""
                , T.commentEnd      = ""
                , T.commentLine     = "#"
                , T.nestedComments  = False
                , T.identStart      = letter <|> char '.'
                , T.identLetter     = alphaNum <|> char '_'
                , T.opStart         = T.opLetter hudsonDef
                , T.opLetter        = oneOf ":%*+/<=>?^-"
                , T.reservedNames   = ["and", "assert", "class", "constant",
                                       "do", "else", "false", "fun", "function",
                                       "if", "inherit", "is", "not", "null",
                                       "or", "procedure", "ref", "return",
                                       "then", "this", "true", "variable",
                                       "while"]
                , T.reservedOpNames = [":=", "?"]
                , T.caseSensitive   = True
                }

parens p        = between (symbol "(") (symbol ")") p
braces p        = between (symbol "{") (symbol "}") p
angles p        = between (symbol "<") (symbol ">") p
brackets p      = between (symbol "[") (symbol "]") p

semi            = symbol ";"
comma           = symbol ","
dot             = symbol "."
colon           = symbol ":"

commaSep p      = sepBy p comma
semiSep p       = sepBy p semi

commaSep1 p     = sepBy1 p comma
semiSep1 p      = sepBy1 p semi


-----------------------------------------------------------
-- Chars & Strings
-----------------------------------------------------------
stringLiteral   = lexeme (
                  do{ str <- between (char '"')
                                     (char '"' <?> "end of string")
                                     (many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string")
    where
      stringChar = do{ c <- stringLetter; return (Just c) }
               <|> stringEscape
               <?> "string character"

      stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

      stringEscape = liftM Just (char '\\' >> escapeCode)

      escapeCode = charEsc <?> "escape code"
      charEsc = choice (map parseEsc escMap)
      parseEsc (c,code) = char c >> return code
      escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"


-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
integer = lexeme int <?> "integer"
    where
      int = do{ f <- lexeme sign
              ; n <- nat
              ; return (f n)
              }
      sign = (char '-' >> return negate)
         <|> (char '+' >> return id)
         <|> return id
      nat = zeroNumber <|> decimal
      zeroNumber = char '0' >> (decimal <|> return 0)
               <?> ""

      decimal = number 10 digit

      number base baseDigit
          = do{ digits <- many1 baseDigit
              ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
              ; seq n (return n)
              }

-----------------------------------------------------------
-- Operators & reserved ops
-----------------------------------------------------------
reservedOp name =
    lexeme $ try $
    do{ string name
      ; notFollowedBy (T.opLetter hudsonDef) <?> ("end of " ++ show name)
      }

operator =
    lexeme $ try $
    do{ name <- oper
      ; if isReservedOp name
         then unexpected ("reserved operator " ++ show name)
         else return name
      }
    where
      oper = liftM2 (:) (T.opStart hudsonDef) (many (T.opLetter hudsonDef))
         <?> "operator"

      isReservedOp name = isReserved (sort (T.reservedOpNames hudsonDef)) name


-----------------------------------------------------------
-- Identifiers & Reserved words
-----------------------------------------------------------
reserved name =
    lexeme $ try $
    do{ string name
      ; notFollowedBy (T.identLetter hudsonDef) <?> ("end of " ++ show name)
      }


identifier' startP =
    lexeme $ try $
    do{ name <- ident
      ; if isReservedName name
         then unexpected ("reserved word " ++ show name)
         else return name
      }
    where
      ident = liftM2 (:) startP (many (T.identLetter hudsonDef))
          <?> "identifier"

      isReservedName name = isReserved theReservedNames name

varIdentifier = identifier' lower
classIdentifier = identifier' upper

isReserved names name = elem name names

theReservedNames = sort (T.reservedNames hudsonDef)

-----------------------------------------------------------
-- White space & symbols
-----------------------------------------------------------
symbol name = lexeme (string name)

lexeme p = do{ x <- p; whiteSpace; return x  }

spaces = many (char ' ')
whiteSpace = skipMany (simpleHorizontalSpace <|> oneLineComment <?> "")
    where
      oneLineComment = try (string "#") >> skipMany (satisfy (/= '\n')) >> return ()
      simpleHorizontalSpace = skipMany1 (satisfy isHorizontalSpace)
      isHorizontalSpace c = c /= '\r' && c /= '\n' && isSpace c

verticalSpace = skipMany (char '\r' <|> char '\n')
            <?> "newline"
