{-# LANGUAGE FlexibleContexts #-}

module HudsonScanner
    ( removeJunk
    , removeUnnecessary
    , tokenize
    , Token
    , tokenizeHudsonFile
    , tokenizeString
    , Tok(..)
    , Separator(..)
    , Operator(..)
    , Keyword(..)
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
               deriving (Eq, Show)

data Operator = PlusOp    | MinusOp    | MultiplyOp | DivideOp
              | ModulusOp | EqualityOp | TypeTestOp | GreaterEqOp
              | GreaterOp | LessEqOp   | LessOp     | ConcatenateOp
                deriving (Eq, Show)

data Separator = AssignSep | ColonSep | CommaSep | LParenSep | RParenSep
                 deriving (Eq, Show)

tokenizeHudsonFile :: String -> IO (Either ParseError [Token])
tokenizeHudsonFile fname = do
  input <- readFile fname
  let lexed = prelex input fname
  return $ removeUnnecessary <$> parse tokenize fname lexed

-- | Remove junk tokens
removeJunk :: [Token] -> [Token]
removeJunk = filter f
    where f (JunkTok, _) = False
          f _ = True

-- | Remove Junk and comment tokens.
removeUnnecessary = filter f
    where f (JunkTok, _) = False
          f (CommentTok _, _) = False
          f _ = True

debugView :: [Token] -> [String]
debugView = map display
    where display (t, s) = printf "(%s, (%d,%d))" (show t) (sourceLine s) (sourceColumn s)

-- | Insert Indent and Outdent tokens into the list.  Like foldr, but
-- tracks the last indented node that wasn't a newline or continuation
-- comment.
offside :: [Token] -> [Token]
offside [] = []
offside ts = off (head ts) [] ts
    where
      -- TODO: remove acc because the same information is in stk

      -- | Traverse the tokens and insert indent and outdent tokens.
      -- Keep a stack of column positions so we can properly outdent
      -- multiple levels.
      off :: Token -> [Int] -> [Token] -> [Token]
      off acc stk [] = replicate (length stk) (OutdentTok, pos . last $ ts)
      off acc stk (x@(NewlineTok, _):xs)       = x:(off acc stk xs)
      off acc stk (x@(JunkTok, _):xs)          = x:(off acc stk xs)
      off acc stk (x@(ContCommentTok _, _):xs) = x:(off acc stk xs)
      off acc stk (x@(CommentTok _, _):xs)     = x:(off acc stk xs)

      off acc stk (x:xs)
          | line x == line acc = x:(off acc stk xs)
          | col x < col acc    = outdents x stk ++
                                 x : (off x (dropWhile (>col x) stk) xs)
          | col x > col acc    = (IndentTok, pos x) : x : (off x (col x : stk) xs)
          | otherwise          = x:(off acc stk xs)

      outdents :: Token -> [Int] -> [Token]
      outdents tok stk = replicate (length $ takeWhile (> col tok) stk)
                                   (OutdentTok, pos tok)
      col = sourceColumn . pos
      line = sourceLine . pos
      pos = snd

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

mkToken p f = do xss@(x:xs) <- p
                 return (f xss, cpPos x)

mkToken' p f d = do pos <- getPosition
                    xs <- p
                    if null xs
                      then return (d, pos)
                      else return (f xs, cpPos $ head xs)

hudsonKeywords = [("and", AndKW),           ("assert", AssertKW),
                  ("class", ClassKW),       ("constant", ConstantKW),
                  ("do", DoKW),             ("else", ElseKW),
                  ("false", FalseKW),       ("fun", FunKW),
                  ("function", FunctionKW), ("if", IfKW),
                  ("inherit", InheritKW),   ("is", IsKW),
                  ("not", NotKW),           ("null", NullKW),
                  ("or", OrKW),             ("procedure", ProcedureKW),
                  ("ref", RefKW),           ("return", ReturnKW),
                  ("then", ThenKW),         ("this", ThisKW),
                  ("true", TrueKW),         ("variable", VariableKW),
                  ("while", WhileKW)]

keywordMap = Map.fromList hudsonKeywords

objMember = char '.' <:> lower <:> many idChar <?> "class method or variable name"

ident = alpha <:> many idChar

idChar = alphaNum <|> char '_' <|> char '.'

-- | Parse an identifier and classify it as reserved word, lowercase
-- identifier or uppercase identifier.
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
                   ("*", MultiplyOp), ("/",  DivideOp),
                   ("%", ModulusOp),  ("=",  EqualityOp),
                   ("?", TypeTestOp), (">=", GreaterEqOp),
                   (">", GreaterOp),  ("<=", LessEqOp),
                   ("<", LessOp),     ("&",  ConcatenateOp)]

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

-- | Tokenize a regular comment.
comment = mkToken' (char '#' >> manyTill anyChar newline)
                   (CommentTok . toString)
                   (CommentTok "")

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

tokenizeString s = removeUnnecessary <$> parse tokenize "" (prelex s "")