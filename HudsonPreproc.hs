-- | Handle continuation comments and convert significant whitespace
-- into delimited blocks.

module HudsonPreproc where

import Text.ParserCombinators.Parsec hiding (newline, space)
import Control.Applicative hiding (optional, many, (<|>))
import Control.Monad
import Data.List (mapAccumL)
import Data.Foldable (foldrM)
instance Applicative (GenParser tok st) where
    pure  = return
    (<*>) = ap
-- is, do, then, else
-- deOffside Nothing = Nothing
-- deOffside =
--   r <- choice [reserved s | s <- offsideStarts]
--   newline
  
--     where offsideStarts = ["do", "else", "is", "then"]

reserved r = try $ string r <* notFollowedBy letter

normalize i = parseResult deComment i
          >>= parseResult deSpace
          >>= parseResult deLine
-- >>= deOffside

parseResult p i = either (const Nothing) Just $ parse p "" i

normComment = contComment
          <|> comment
          <|> newline
          <|> anyString
    where
      contComment = try (string "##") >> manyTill anyChar newline >> return ""
      comment = char '#' >> manyTill anyChar newline >> return "\n"

deComment = liftM concat $ many normComment

newline = (optional (char '\r') >> string "\n") <?> "newline"

normSpaces = normTrailingSpaces
         <|> normIndentSpaces
         <|> anyString

deSpace = liftM concat $ many normSpaces

normTrailingSpaces = try $ many1 (char ' ') >> newline

normIndentSpaces = do
  indent <- liftM sourceColumn getPosition
  if indent > 1
     then many1 (char ' ') >> return " "
     else many1 (char ' ')

anyString = liftM (:"") anyChar

normLines = many1 newline >> return "\n"

deLine = liftM concat $ many (normLines <|> anyString)