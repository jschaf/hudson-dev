{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HudsonScanner
    (removeJunk,
     tokenize,
     Token,
     tokenizeHudsonFile,
     Tok(..),
     Tag(..)
    ) where

import Control.Applicative (Applicative, liftA, liftA2)
import Control.Monad.Identity

import Data.Char
import Data.Function (on)
import Data.List (mapAccumL, foldl')

import Text.Parsec ((<?>), parseTest, ParseError)
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
         | LiteralString
         | Comment
         | Newline
         | Junk
           deriving (Eq, Show)

(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr 5 <:>

tokenizeHudsonFile :: String -> IO (Either ParseError [Token])
tokenizeHudsonFile fname = do
  input <- readFile fname
  let lexed = prelex input fname
  return $ parse tokenize fname lexed

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
satisfy :: (Stream [CharPos] Identity CharPos) => (Char -> Bool)
          -> ParsecT [CharPos] () Identity CharPos
satisfy f = tokenPrim (\cp -> show [(cpChar cp)])
                      (\pos c _cs -> updatePos pos c)
                      (\cp -> if f (cpChar cp) then Just cp else Nothing)

char c = satisfy (==c) <?> show [c]

space = satisfy isSpace <?> "space"

spaces = many space <?> "white space"

upper = satisfy isUpper <?> "uppercase letter"

lower = satisfy isLower <?> "lowercase letter"

alphaNum = satisfy isAlphaNum <?> "letter or digit"

digit = satisfy isDigit <?> "digit"

anyChar = satisfy (const True)

newline = optional (char '\r') >> char '\n'

string s = do
  pos <- getPosition
  tokens (map cpChar) (foldl' updatePos) (prelex' pos s)

tokenize = manyTill p eof
    where p = choice [pNewline, spaceJunk, number, reserved, upperID, lowerID, symbol,
                      contComment, comment]

toToken p t = do
  xss@(x:xs) <- p
  let tok = Tok t $ toString xss
  return (tok, cpPos x)

spaceJunk = toToken spaces Junk

number = toToken (many1 digit) Number

idChar = alphaNum <|> char '_' <|> char '.'

upperID = toToken p UpperID
    where p = upper <:> many idChar

lowerID = toToken p LowerID
    where p =     char '.' <:> lower <:> many idChar
              <|> lower <:> many idChar


hudsonReservedWords = ["and", "assert", "class", "constant", "do", "else",
                       "false", "fun", "function", "if", "inherit", "is", "not",
                       "null", "or", "procedure", "ref", "return", "then",
                       "this", "true", "variable", "while"]

reserved = toToken p Reserved
    where p = choice [try $ string r | r <- hudsonReservedWords]

hudsonSymbols = ["+", "-", "*", "/", "%", ":=", ":", ",", "=", "(", ")", "?", ">=",
                 ">", "<", "<=", "&", "'", "\""]

symbol = toToken p Symbol
    where p = choice [try $ string s | s <- hudsonSymbols]

contComment = toToken p ContComment
    where p = try (string "##") >> manyTill anyChar newline

comment = toToken p Comment
    where p = char '#' >> manyTill anyChar newline

pNewline = toToken p Newline
    where p = newline <:> return []
