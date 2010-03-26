-- | Handle continuation comments and convert significant whitespace
-- into delimited blocks.

module HudsonPreproc where

import Text.ParserCombinators.Parsec hiding (newline)
import Control.Applicative hiding (optional, many)
import Control.Monad

instance Applicative (CharParser a) where
        pure = return
        (<*>) = ap

newline = optional (char '\r') >> char '\n' <?> "newline"
contComment = try (string "##") >> manyTill anyChar newline
comment = char '#' >> manyTill anyChar newline

reserved name = try (string name) <* notFollowedBy alphaNum
