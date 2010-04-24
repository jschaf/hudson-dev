{-# LANGUAGE FlexibleContexts #-}

module HudsonScanner
    ( removeJunk
    , removeUnnecessary
    , keywordString
    , tss
    , tokenize
    , Token
    , tokenizeHudsonFile
    , tokenizeString
    , Tok(..)
    , Separator(..)
    , Operator(..)
    , Keyword(..)
    , (<:>)
    ) where

import Control.Monad (msum, liftM)
import Control.Applicative (Applicative, liftA, liftA2, (<$>))
import Control.Monad.Identity (Identity)

import Data.Char
import Data.Function (on)
import Data.List (mapAccumR, mapAccumL, foldl')

import qualified Data.Map as Map

import Text.Parsec ((<?>), parseTest, ParseError)
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

import Text.Printf

-- TODO:
--
-- Try to not rewrite all the Parsec Char parsers.

data CharPos = CharPos {cpChar :: Char, cpPos :: SourcePos}
             deriving (Show)

instance Eq CharPos where
    (==) = (==) `on` cpChar

instance Ord CharPos where
    compare = compare `on` cpChar

type Token  = (Tok, SourcePos)

data Tok = NumberTok       Integer   -- ^ a literal integer
         | ReservedTok     Keyword   -- ^ a reserved keyword
         | OperatorTok     Operator  -- ^ a Hudson operator (i.e. > or +)
         | SeparatorTok    Separator -- ^ a Hudson separator (i.e. :=)
         | StringTok       String    -- ^ a literal string
         | UpperIDTok      String    -- ^ an uppercased ID
         | LowerIDTok      String    -- ^ a lowercased ID
         | ObjMemberIDTok  String    -- ^ an object member
         | ContCommentTok  String    -- ^ a continuation comment
         | CommentTok      String    -- ^ a comment
         | IndentTok                 -- ^ an indent token
         | OutdentTok                -- ^ an outdent token
         | NewlineTok                -- ^ a newline
         | JunkTok                   -- ^ junk (i.e. whitespace)
           deriving (Show)

data Keyword = AndKW       | AssertKW   | ClassKW  | ConstantKW | DoKW
             | ElseKW      | FalseKW    | FunKW    | FunctionKW | IfKW
             | InheritKW   | IsKW       | NotKW    | NullKW     | OrKW
             | ProcedureKW | RefKW      | ReturnKW | ThenKW     | ThisKW
             | TrueKW      | VariableKW | WhileKW
               deriving (Enum, Eq, Show)

data Operator = PlusOp    | MinusOp    | MultiplyOp | DivideOp
              | ModulusOp | EqualityOp | TypeTestOp | GreaterEqOp
              | GreaterOp | LessEqOp   | LessOp     | ConcatenateOp
              | NotEqOp
                deriving (Enum, Eq, Show)

data Separator = AssignSep | ColonSep | CommaSep | LParenSep | RParenSep
                 deriving (Eq, Show)

keywordString :: Keyword -> String
keywordString = map toLower . init . init . show

tokenizeHudsonFile :: String -> IO (Either ParseError [Token])
tokenizeHudsonFile fname = do
  input <- readFile fname
  let lexed = prelex input fname
  return $ offside <$> removeUnnecessary <$> parse tokenize fname lexed

-- | Remove junk tokens
removeJunk :: [Token] -> [Token]
removeJunk = filter f
    where f (JunkTok, _) = False
          f _ = True

-- | Remove Junk and comment tokens.
removeUnnecessary = filter f
    where f (JunkTok, _) = False
          f (CommentTok _, _) = False
          f (ContCommentTok _, _) = False
          f _ = True

-- | Insert Indent and Outdent tokens into the list.  Like foldr, but
-- tracks the last indented node that wasn't a newline or continuation
-- comment.
offside :: [Token] -> [Token]
offside [] = []
offside ts = off [] ts
    where
      -- | Traverse the tokens and insert indent and outdent tokens.
      -- Keep a stack of column positions so we can properly outdent
      -- multiple levels.
      off :: [Token]            -- ^ the stack of indents
          -> [Token]            -- ^ the list of tokens
          -> [Token]

      -- Subtract one because we use the first token to initialize the
      -- stack, but it doesn't change indentation.
      off stk [] = replicate (length stk - 1) (OutdentTok, pos . last $ ts)

      off stk (x@(NewlineTok, _):xs)       = x:(off stk xs)
      -- Tokens that we don't want and don't affect indentation.
      off stk (x@(JunkTok, _):xs)          = off stk xs
      off stk (x@(ContCommentTok _, _):xs) = off stk xs
      off stk (x@(CommentTok _, _):xs)     = off stk xs

      -- We need a start token for indentation that affects
      -- indentation.
      off [] tss@(t:ts) = off [t] tss -- TODO: Check to see if it's in col 1

      -- Compare the relative indentation of the current token @t@ to
      -- the top of the indentation stack @s@.  When less, insert the
      -- proper number of outdents, the rest of the line and then
      -- continue.  When greater, insert one indent token, the rest of
      -- the line and then continue after pushing the current token on
      -- the indentation stack.  When equal, insert the current line
      -- and then continue.
      off stk@(s:ss) tss@(t:ts)
          | t `dedents` s = outdents ++ lineCode ++ off remaining restCode
          | t `indents` s = (IndentTok, pos t) : lineCode ++ off (t:stk) restCode
          | otherwise     = lineCode ++ off stk restCode
          where
            (closed, remaining) = span (dedents t) stk
            -- | A list with an outdent for each level of indentation
            -- closed.
            outdents = replicate (length closed) (OutdentTok, pos t)
            
            (lineCode, restCode) = spanToNewline tss

      x `dedents` s = col x < col s
      x `indents` s = col x > col s
      col = sourceColumn . pos
      line = sourceLine . pos
      pos = snd

      spanToNewline ts = span f ts
          where f (NewlineTok, _) = False
                f _               = True

-- | Return the string representation of a list of CharPos.
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
                      (\pos c cs -> updatePos pos c)
                      (\cp -> if f (cpChar cp) then Just cp else Nothing)

char c   = satisfy (==c) <?> show [c]
space    = satisfy (==' ') <?> "space"
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
    where p = choice [newlinetok, spaceJunk, identifier, operator, separator,
                      integer, stringLiteral, contComment, comment]

spaceJunk = spaces >>= (\(x:xs) -> return (JunkTok, cpPos x))

integer = do
  dss@(d:ds) <- many1 digit
  let n = toNum dss
  return (NumberTok n, cpPos d)

toNum :: [CharPos] -> Integer
toNum digits = foldl' convert 0 digits
    where convert x d = 10*x + toInteger (digitToInt $ cpChar d)

-- | List cons for applicative functors.
(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr 5 <:>

-- | Return a token with the results of parser @p@ wrapped in @f@,
-- with the source position set to the first character parsed.
mkToken p f = do xss@(x:xs) <- p
                 return (f xss, cpPos x)

-- | Same as @mkToken@, but use @d@ as a default value if not input is
-- consumed (e.g. an empty comment).
mkToken' p f d = do pos <- getPosition
                    xs <- p
                    if null xs
                      then return (d, pos)
                      else return (f xs, cpPos $ head xs)

-- | Association list of hudson keywords strings with the associated
-- constructor.
hudsonKeywords = [(keywordString k, k) | k <- [AndKW .. WhileKW]]

keywordMap = Map.fromList hudsonKeywords

-- | Match an object method.
objMember = char '.' <:> lower <:> many idChar <?> "class method or variable name"

-- | Match any identifier. 
ident = alpha <:> many idChar

idChar = alphaNum <|> char '_'

-- | Parse an identifier and classify it as reserved word, lowercase
-- identifier, uppercase identifier or object method name.
identifier = mkToken (objMember <|> ident) toTok
    where
      toTok xs = maybe (toID xs) ReservedTok (Map.lookup (toString xs) keywordMap)

      toID ts@(t:_) | isUpper (cpChar t) = UpperIDTok $ toString ts
                    | isLower (cpChar t) = LowerIDTok $ toString ts
                    | cpChar t == '.'    = ObjMemberIDTok $ toString ts
                    | otherwise          = error "Need upper or lowercase"

-- | Association list of hudson operators with an abstract
-- representation.
hudsonOperators = [("+", PlusOp),     ("-",  MinusOp),
                   ("*", MultiplyOp), ("/=", NotEqOp),
                   ("%", ModulusOp),  ("=",  EqualityOp),
                   ("?", TypeTestOp), (">=", GreaterEqOp),
                   (">", GreaterOp),  ("<=", LessEqOp),
                   ("<", LessOp),     ("&",  ConcatenateOp),
                   ("/", DivideOp)
                  ]

-- TODO: Generalize this, possibly using mkToken
tryOperator (s, o) = do (x:_) <- try (string s)
                        return (OperatorTok o, cpPos x)

operator = msum $ map tryOperator hudsonOperators

hudsonSeparators = [(":=", AssignSep), (":", ColonSep),
                    (",", CommaSep),   ("(", LParenSep),
                    (")", RParenSep)]

trySeparator (s, o) = do (x:_) <- try (string s)
                         return (SeparatorTok o, cpPos x)

separator = msum $ map trySeparator hudsonSeparators

-- | Tokenize a continuation comment.
contComment = mkToken' (try (string "##") >> manyTill anyChar newline)
                       (ContCommentTok . toString)
                       (ContCommentTok "")

manyTill' p end = scan
    where scan  = end <|> (p <:> scan)

-- | Tokenize a regular comment.
comment = mkToken' (char '#' >> manyTill anyChar (lookAhead newline)) -- TODO: eof too?
                   (CommentTok . toString)
                   (CommentTok "")
    where
      incLastSource :: [CharPos] -> SourcePos
      incLastSource cs = flip incSourceColumn 1 . cpPos . last $ cs

newlinetok = mkToken newline (return NewlineTok)

-- TODO: This is messy because I couldn't find a clean way to convert
-- chars from the escMap into CharPos.

-- | Parse a string literal (i.e. wrapped in quotations)
stringLiteral = do pos <- getPosition
                   ss@(s:_) <- between (char '"')
                                       (char '"' <?> "end of string")
                                       (many $ stringChar pos)
                   return (StringTok $ toString ss, decColumn $ cpPos s)
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

example = liftM fromRight $ tokenizeHudsonFile "test1.hud"
    where fromRight (Right a)  = a

tokenizeString s = removeUnnecessary <$> offside <$> parse tokenize "" (prelex s "")

tss s = parse tokenize "" (prelex s "")