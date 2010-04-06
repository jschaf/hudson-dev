{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module HudsonScanner where

import Control.Monad
import Control.Applicative (liftA, liftA2)

import Data.Char
import Data.Function (on)
import Data.List (mapAccumL, foldl')

import Text.Parsec ((<?>), parseTest)
import Text.Parsec.Combinator
import Text.Parsec.Prim -- (ParsecT, Stream, tokenPrim, tokens, skipMany)
import Text.Parsec.Pos (SourcePos, newPos, updatePosString)

type Pos = (Int, Int)

data CharPos = CharPos {cpChar :: Char, cpPos :: Pos}
             deriving (Show)

instance Eq CharPos where
    (==) = (==) `on` cpChar

type Token  = (Tok, Pos)

data Tok = Number Integer
         | UpperID String
         | LowerID String
         | Reserved String
         | Symbol String
         | Comment String
         | ContComment String
         | Junk
           deriving (Show)

toString :: [CharPos] -> String
toString = map cpChar

-- toToken :: [CharPos] -> Tok -> Token
-- toToken [] _ = error "need CharPos"
-- toToken (c:cs) t = (

-- | Pair each character with its source position.
prelex :: String -> [CharPos]
prelex xs = snd $ mapAccumL step (1,0) xs
    where
      step (r,c) x | x == '\n' = ((r+1, 0), charInfo)
                   | otherwise = ((r, c+1), charInfo)
          where charInfo = CharPos x (r,c)

updateCharPos :: SourcePos -> CharPos -> SourcePos
updateCharPos _s (CharPos c (line, col)) = newPos [c] line col

-- TODO: This is reinventing the wheel but I don't know a better way.
satisfy :: (Stream s m CharPos) => (Char -> Bool) -> ParsecT s u m CharPos
satisfy f           = tokenPrim (\cp -> show [(cpChar cp)])
                                (\pos c _cs -> updateCharPos pos c)
                                (\cp -> if f (cpChar cp) then Just cp else Nothing)

char c = satisfy (==c) <?> show [c]

space :: (Stream s m CharPos) => ParsecT s u m CharPos
space = satisfy isSpace <?> "space"

spaces :: (Stream s m CharPos) => ParsecT s u m ()
spaces = skipMany space <?> "white space"

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

string :: (Stream s m CharPos) => String -> ParsecT s u m [CharPos]
string s = tokens display updatePos s' -- TODO: Hack!
    where display = map cpChar
          updatePos pos cs = foldl' updateCharPos pos cs
          s' = [CharPos c (0,0) | c <- s]

-- TODO: find out how to get CharPos into Tokens

-- tokenize :: (Stream s m CharPos) => [CharPos] -> ParsecT s u m [Token]
-- tokenize s = manyTill p eof
--     where p =     (spaces >> return Junk)
--               -- <|> liftA (UpperID . toString) upperID

toToken :: (Stream s m CharPos) =>
          ParsecT s u m [CharPos]
       -> (String -> Tok)
       -> ParsecT s u m Token
toToken p t = do
  xss@(x:xs) <- p
  let tok = t $ toString xss
  return (tok, cpPos x)

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

-- symbol :: (Stream s m CharPos) => ParsecT s u m Token

test p s = parseTest p (prelex s)