{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module HudsonScanner where

import Control.Monad
import Control.Applicative (liftA, liftA2)

import Data.Char
import Data.List (mapAccumL, foldl')

import Text.Parsec ((<?>), parseTest)
import Text.Parsec.Combinator
import Text.Parsec.Prim -- (ParsecT, Stream, tokenPrim, tokens, skipMany)
import Text.Parsec.Pos (SourcePos, newPos, updatePosString)

type Pos = (Int, Int)

type CharPos = (Char, Pos)

type Token  = (Tok, Pos)

data Tok = Number Integer
         | UpperID String
         | LowerID String
         | Reserved String           
         | Comment String
         | ContComment String
         | Junk
           deriving (Show)

type Parser = Parsec [CharPos] ()

-- instance (Monad m) => Stream [tok] m CharPos where
--     uncons []     = return $ Nothing
--     uncons (t:ts) = return $ Just (t,ts)
--     {-# INLINE uncons #-}

-- | Pair each character with its source position.
prelex :: String -> [(Char, Pos)]
prelex xs = snd $ mapAccumL step (1,0) xs
    where
      step (r,c) x | x == '\n' = ((r+1, 0), charInfo)
                   | otherwise = ((r, c+1), charInfo)
          where charInfo = (x, (r,c))

updateCharPos :: SourcePos -> CharPos -> SourcePos
updateCharPos _s (c, (line, col)) = newPos [c] line col

-- TODO: This is reinventing the wheel but I don't know a better way.
satisfy :: (Stream s m CharPos) => (Char -> Bool) -> ParsecT s u m CharPos
satisfy f           = tokenPrim (\cp -> show [fst cp])
                                (\pos c _cs -> updateCharPos pos c)
                                (\cp -> if f (fst cp) then Just cp else Nothing)

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
string s = tokens display updatePos (zip s (repeat (0,0))) -- TODO: Hack!
    where display = map fst
          updatePos pos cs = foldl' updateCharPos pos cs

-- TODO: find out how to get CharPos into Tokens

-- tokenize :: [(Char, Pos)] -> [Token]
-- tokenize s = manyTill p eof
--     where p =     (spaces >> return Junk)
--               <|> liftA UpperID upperID
idChar :: (Stream s m CharPos) => ParsecT s u m CharPos
idChar = alphaNum <|> char '_'

upperID :: (Stream s m CharPos) => ParsecT s u m [CharPos]
upperID = liftA2 (:) upper $ many idChar

lowerID :: (Stream s m CharPos) => ParsecT s u m [CharPos]
lowerID = liftA2 (:) lower $ many idChar 

hudsonReservedWords = ["and", "assert", "class", "constant", "do", "else",
                       "false", "fun", "function", "if", "inherit", "is", "not",
                       "null", "or", "procedure", "ref", "return", "then",
                       "this", "true", "variable", "while"]

reserved :: (Stream s m CharPos) => ParsecT s u m [CharPos]
reserved = choice [try $ string r | r <- hudsonReservedWords]


-- tokenP :: Show t => t -> GenParser ((Int,Int),t) () t
-- tokenP x
--   = token showTok posFromTok testTok
--   where
--     showTok (pos,t)     = show t
--     posFromTok (pos,t)  = pos
--     testTok (pos,t)     = if (x == t) then Just t else Nothing

-- tokSymbols = liftM Symbol $
--              choice [ oneOf "()?-+"
--                     , choice [try $ string ps | ps <- [":=", ">=", "<="]]
--                     , oneOf "<>=:"
                            
-- type TokParser a = GenParser Token () a

-- mytoken :: (Tok -> Maybe a) -> TokParser a
-- mytoken test = token showToken posToken testToken
--     where
--       showToken (pos,tok)   = show tok
--       posToken  (pos,tok)   = pos
--       testToken (pos,tok)   = test tok

-- identifier :: TokParser String
-- identifier = mytoken (\tok -> case tok of 
--                                 Identifier name -> Just name
--                                 other           -> Nothing)

-- reserved :: String -> TokParser ()
-- reserved name = mytoken (\tok -> case tok of
--                                    Reserved s   | s == name  -> Just ()
--                                    other        -> Nothing)

test p s = parseTest p (prelex s)