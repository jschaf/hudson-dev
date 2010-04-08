{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HudsonScanner
    (removeJunk,
     tokenize,
     Token,
     tokenizeHudsonFile,
     Tok(..),
    ) where

import Control.Applicative (Applicative, liftA, liftA2)
import Control.Monad.Identity

import Data.Char
import Data.Function (on)
import Data.List (mapAccumL, foldl')

import qualified Data.Map as Map

import Text.Parsec ((<?>), parseTest, ParseError)
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

-- TODO:
--
-- Try to not rewrite all the Parsec Char parsers
--

data CharPos = CharPos {cpChar :: Char, cpPos :: SourcePos}
             deriving (Show)

instance Eq CharPos where
    (==) = (==) `on` cpChar

instance Ord CharPos where
    compare = compare `on` cpChar

type Token  = (Tok, SourcePos)

data Tok = Number_Tok       Integer
         | Reserved_Tok     Keyword
         | Operator_Tok     Operator
         | Separator_Tok    Separator
         | String_Tok       String
         | UpperID_Tok      String
         | LowerID_Tok      String
         | ObjMemberID_Tok  String 
         | ContComment_Tok  String
         | Comment_Tok      String
         | Newline_Tok
         | Junk_Tok
           deriving (Show)

data Keyword = And_KW       | Assert_KW   | Class_KW  | Constant_KW | Do_KW
             | Else_KW      | False_KW    | Fun_KW    | Function_KW | If_KW
             | Inherit_KW   | Is_KW       | Not_KW    | Null_KW     | Or_KW
             | Procedure_KW | Ref_KW      | Return_KW | Then_KW     | This_KW
             | True_KW      | Variable_KW | While_KW
               deriving (Show)

data Operator = Plus_Op    | Minus_Op    | Multiply_Op | Divide_Op
              | Modulus_Op | Equality_Op | TypeTest_Op | Greater_Eq_Op
              | Greater_Op | Less_Eq_Op  | Less_Op     | Concatenate_Op
                deriving (Show)

data Separator = Assign_Sep | Colon_Sep | Comma_Sep | LParen_Sep | RParen_Sep
                 deriving (Show)

tokenizeHudsonFile :: String -> IO (Either ParseError [Token])
tokenizeHudsonFile fname = do
  input <- readFile fname
  let lexed = prelex input fname
  return $ parse tokenize fname lexed

removeJunk :: [Token] -> [Token]
removeJunk = filter f
    where f (Junk_Tok, _) = False
          f _ = True

toString :: [CharPos] -> String
toString = map cpChar

-- | Pair each character with its source position.
prelex :: String -> String -> [CharPos]
prelex xs sname = prelex' (initialPos sname) xs

-- | Pair each character with a source position beginning with the
-- supplied intial position.
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

char c   = satisfy (==c) <?> show [c]
space    = satisfy isSpace <?> "space"
spaces   = many1 space <?> "white space"
upper    = satisfy isUpper <?> "uppercase letter"
lower    = satisfy isLower <?> "lowercase letter"
alpha    = satisfy isAlpha <?> "letter"
alphaNum = satisfy isAlphaNum <?> "letter or digit"
digit    = satisfy isDigit <?> "digit"
anyChar  = satisfy (const True)
newline  = optional (char '\r') >> string "\n"

string s = do
  pos <- getPosition
  tokens (map cpChar) (foldl' updatePos) (prelex' pos s)

tokenize = manyTill p eof
    where p = choice [spaceJunk, newline_tok, identifier, operator, separator,
                      integer, stringLiteral, contComment, comment]

spaceJunk = spaces >>= (\(x:xs) -> return (Junk_Tok, cpPos x))

integer = do
  dss@(d:ds) <- many1 digit
  let n = toNum dss
  return (Number_Tok n, cpPos d)

toNum :: [CharPos] -> Integer
toNum digits = foldl' convert 0 digits
    where convert x d = 10*x + toInteger (digitToInt $ cpChar d)

-- | List cons for applicative functors.
(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr 5 <:>

mkToken p f = do xss@(x:xs) <- p
                 return (f xss, cpPos x)

mkToken' p f d = do pos <- getPosition
                    xs <- p
                    if null xs
                      then return (d, pos)
                      else return (f xs, cpPos $ head xs)

hudsonKeywords = [("and", And_KW),           ("assert", Assert_KW),
                  ("class", Class_KW),       ("constant", Constant_KW),
                  ("do", Do_KW),             ("else", Else_KW),
                  ("false", False_KW),       ("fun", Fun_KW),
                  ("function", Function_KW), ("if", If_KW),
                  ("inherit", Inherit_KW),   ("is", Is_KW),
                  ("not", Not_KW),           ("null", Null_KW),
                  ("or", Or_KW),             ("procedure", Procedure_KW),
                  ("ref", Ref_KW),           ("return", Return_KW),
                  ("then", Then_KW),         ("this", This_KW),
                  ("true", True_KW),         ("variable", Variable_KW),
                  ("while", While_KW)]

keywordMap = Map.fromList hudsonKeywords

objMember = char '.' <:> lower <:> many idChar <?> "class method or variable name"

ident = alpha <:> many idChar

idChar = alphaNum <|> char '_' <|> char '.'

identifier = mkToken (objMember <|> ident) toTok
    where 
      toTok xs = maybe (toID xs) Reserved_Tok (Map.lookup (toString xs) keywordMap)

      toID ts@(t:_) | isUpper (cpChar t) = UpperID_Tok $ toString ts
                    | isLower (cpChar t) = LowerID_Tok $ toString ts
                    | cpChar t == '.'    = ObjMemberID_Tok $ toString ts
                    | otherwise          = error "Need upper or lowercase"


hudsonOperators = [("+", Plus_Op),     ("-",  Minus_Op),
                   ("*", Multiply_Op), ("/",  Divide_Op),
                   ("%", Modulus_Op),  ("=",  Equality_Op),
                   ("?", TypeTest_Op), (">=", Greater_Eq_Op),
                   (">", Greater_Op),  ("<=", Less_Eq_Op),
                   ("<", Less_Op),     ("&",  Concatenate_Op)]

-- TODO: Generalize this, possibly using mkToken
tryOperator (s, o) = do (x:_) <- try (string s)
                        return (Operator_Tok o, cpPos x)

operator = msum $ map tryOperator hudsonOperators

hudsonSeparators = [(":=", Assign_Sep), (":", Colon_Sep),
                    (",", Comma_Sep),   ("(", LParen_Sep),
                    (")", RParen_Sep)]

trySeparator (s, o) = do (x:_) <- try (string s)
                         return (Separator_Tok o, cpPos x)

separator = msum $ map trySeparator hudsonSeparators

contComment = mkToken' (try (string "##") >> manyTill anyChar newline)
                       (ContComment_Tok . toString)
                       (ContComment_Tok "")

comment = mkToken' (char '#' >> manyTill anyChar newline)
                   (Comment_Tok . toString)
                   (Comment_Tok "")

newline_tok = mkToken newline (return Newline_Tok)

-- TODO: This is messy because I couldn't find a clean way to convert
-- chars from the escMap into CharPos.  

-- | Parse a string literal (i.e. wrapped in quotations)
stringLiteral = do pos <- getPosition
                   ss@(s:_) <- between (char '"')
                                       (char '"' <?> "end of string")
                                       (many $ stringChar pos)
                   return (String_Tok $ toString ss, decColumn $ cpPos s)
             <?> "literal string"
    where 
      -- | Decrease the column number to account for the opening
      -- quotation mark which is skipped.
      decColumn s = newPos (sourceName s) (sourceLine s) (sourceColumn s - 1)

      stringChar pos = stringLetter <|> stringEscape pos <?> "string character"

      -- | Letters that aren't escape characters or special codes.
      stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))
      
      -- | Parse a backslash and return the escaped character.
      stringEscape :: SourcePos -> CharPosParser CharPos
      stringEscape pos = char '\\' >> charEsc pos
      
      -- | Return a matched escape character.
      charEsc :: SourcePos -> CharPosParser CharPos
      charEsc pos = choice (map parseEsc (escMap pos)) <?> "escape code"
      
      -- | Parse the character in an escape code and return its full
      -- representation.
      parseEsc :: (Char, CharPos) -> CharPosParser CharPos
      parseEsc (c,code) = char c >> return code
      
      -- | A map from escape characters to full escape codes.
      escMap :: SourcePos -> [(Char, CharPos)]
      escMap pos = zip "abfnrtv\\\"\'"
                       (map (flip CharPos pos) "\a\b\f\n\r\t\v\\\"\'")

type CharPosParser t = Parsec [CharPos] () t

test p s = parseTest p (prelex s "")
