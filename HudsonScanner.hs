{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module HudsonScanner
    (removeJunk,
     tokenize,
     Token,
     tokenizeHudsonFile,
     Tok(..),
     Tag(..)
    ) where

import Control.Applicative (liftA, liftA2)

import Data.Char
import Data.Function (on)
import Data.List (mapAccumL, foldl')

import Text.Parsec ((<?>), parseTest)
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

-- TODO:
--
-- Try to not rewrite all the Parsec Char parsers
--
-- Find some way to remove the type signatures

data CharPos = CharPos {cpChar :: Char, cpPos :: SourcePos}
             deriving (Show)

instance Eq CharPos where
    (==) = (==) `on` cpChar

type Token  = (Tok, SourcePos)

data Tok = Tok Tag String
           deriving (Show)

data Tag = Number
         | Reserved
         | UpperID
         | LowerID
         | Symbol
         | ContComment
         | Comment
         | Newline
         | Junk
           deriving (Eq, Show)

tokenizeHudsonFile fname = do
  input <- readFile fname
  let lexed = prelex input fname
  return $ runP tokenize () fname lexed

removeJunk :: [Token] -> [Token]
removeJunk = filter f
    where f ((Tok Junk _), _) = False
          f _ = True

toString :: [CharPos] -> String
toString = map cpChar

-- | Pair each character with its source position.
prelex :: String -> String -> [CharPos]
prelex xs sname = prelex' (initialPos sname) xs

prelex' initial xs = snd $ mapAccumL step initial xs
    where
      step srcPos c = (updatePosChar srcPos c, CharPos c srcPos)

updatePos :: SourcePos -> CharPos -> SourcePos
updatePos srcPos (CharPos c pos) = updatePosChar srcPos c

-- TODO: This is reinventing the wheel but I don't know a better way.
satisfy :: (Stream s m CharPos) => (Char -> Bool) -> ParsecT s u m CharPos
satisfy f           = tokenPrim (\cp -> show [(cpChar cp)])
                                (\pos c _cs -> updatePos pos c)
                                (\cp -> if f (cpChar cp) then Just cp else Nothing)

char c = satisfy (==c) <?> show [c]

space :: (Stream s m CharPos) => ParsecT s u m CharPos
space = satisfy isSpace <?> "space"

spaces :: (Stream s m CharPos) => ParsecT s u m [CharPos]
spaces = many space <?> "white space"

upper :: (Stream s m CharPos) => ParsecT s u m CharPos
upper = satisfy (isUpper) <?> "uppercase letter"

lower :: (Stream s m CharPos) => ParsecT s u m CharPos
lower = satisfy isLower <?> "lowercase letter"

alphaNum :: (Stream s m CharPos) => ParsecT s u m CharPos
alphaNum = satisfy isAlphaNum <?> "letter or digit"

digit :: (Stream s m CharPos) => ParsecT s u m CharPos
digit = satisfy isDigit <?> "digit"

anyChar :: (Stream s m CharPos) => ParsecT s u m CharPos
anyChar = satisfy (const True)

newline :: (Stream s m CharPos) => ParsecT s u m CharPos
newline = try (optional $ char '\r') >> char '\n'

string :: (Stream s m CharPos) => String -> ParsecT s u m [CharPos]
string s = do
  pos <- getPosition
  tokens (map cpChar) (foldl' updatePos) (prelex' pos s)

tokenize :: (Stream s m CharPos) => ParsecT s u m [Token]
tokenize = manyTill p eof
    where p = choice [pNewline, spaceJunk, number, reserved, upperID, lowerID, symbol,
                      contComment, comment]

toToken :: (Stream s m CharPos) =>
          ParsecT s u m [CharPos]
       -> Tag
       -> ParsecT s u m Token
toToken p t = do
  xss@(x:xs) <- p
  let tok = Tok t $ toString xss
  return (tok, cpPos x)

spaceJunk :: (Stream s m CharPos) => ParsecT s u m Token
spaceJunk = toToken spaces Junk

number :: (Stream s m CharPos) => ParsecT s u m Token
number = toToken (many1 digit) Number

idChar :: (Stream s m CharPos) => ParsecT s u m CharPos
idChar = alphaNum <|> char '_'

upperID :: (Stream s m CharPos) => ParsecT s u m Token
upperID = toToken p UpperID
    where p = liftA2 (:) upper $ many idChar

lowerID :: (Stream s m CharPos) => ParsecT s u m Token
lowerID = toToken p LowerID
    where p = liftA2 (:) lower $ many idChar

hudsonReservedWords = ["and", "assert", "class", "constant", "do", "else",
                       "false", "fun", "function", "if", "inherit", "is", "not",
                       "null", "or", "procedure", "ref", "return", "then",
                       "this", "true", "variable", "while"]

reserved :: (Stream s m CharPos) => ParsecT s u m Token
reserved = toToken p Reserved
    where p = choice [try $ string r | r <- hudsonReservedWords]

hudsonSymbols = ["+", "-", "*", "/", ":=", ":", ",", "=", "(", ")", "?", ">=", ">", "<", "<="]

symbol :: (Stream s m CharPos) => ParsecT s u m Token
symbol = toToken p Symbol
    where p = choice [try $ string s | s <- hudsonSymbols]

contComment :: (Stream s m CharPos) => ParsecT s u m Token
contComment = toToken p ContComment
    where p = string "##" >> manyTill anyChar newline

comment :: (Stream s m CharPos) => ParsecT s u m Token
comment = toToken p Comment
    where p = char '#' >> manyTill anyChar newline

pNewline :: (Stream s m CharPos) => ParsecT s u m Token
pNewline = toToken p Newline
    where p = liftA (:[]) newline
