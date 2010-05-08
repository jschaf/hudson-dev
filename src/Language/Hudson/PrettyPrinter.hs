module Language.Hudson.PrettyPrinter where

import Text.PrettyPrint.HughesPJ

import Control.Monad (liftM2)

import Language.Hudson.AST
import Language.Hudson.Parser (parseFile, parseString', parseString)

import Control.Applicative ((<$>))

-- TODO
--
-- Write a combinator that automatically adds continuation comments
-- for parameters.
--
-- Space between top level declarations

prettyBlocks :: [Block] -> Doc
prettyBlocks bs | null bs   = empty
                | otherwise = (vsep $ map prettyBlock bs) <> newline

defaultHudsonIndent :: Int
defaultHudsonIndent = 4

withCode :: Doc -> [Block] -> Doc
withCode doc code = withCode' defaultHudsonIndent doc code

withCode' :: Int -> Doc -> [Block] -> Doc
withCode' n doc code
    | null code = doc
    | otherwise = doc $+$ (vcat $ map (nest n . prettyBlock) code)

keyNameSep :: String              -- ^ keyword
           -> String              -- ^ name
           -> Doc                 -- ^ middle part
           -> String              -- ^ separator
           -> (Doc -> Doc -> Doc) -- ^ how to combine the name and middle
           -> Doc
keyNameSep keyword name middle separator combine =
    (text keyword <+> text name) `combine` middle <+> text separator

newline :: Doc
newline = char '\n'

prettyBlock :: Block -> Doc
prettyBlock b = case b of
                  (BlockStmt s) -> prettyStmt s
                  (BlockDecl d) -> prettyDecl d

vsep :: [Doc] -> Doc
vsep = foldl1 ($+$)


prettyStmt :: Stmt -> Doc
prettyStmt s =
    case s of
      Assignment v e -> prettyExpr v <+> text ":=" <+> prettyExpr e
      ProcCall v ps  -> text v <> (parens . commaDelimit $ map prettyExpr ps)
      ObjCall e      -> prettyExpr e
      If {}          -> prettyIf s
      While cond ws  -> (text "while" <+> prettyExpr cond <+> text "do")
                          `withCode` ws <> newline
      Return m       -> maybe (text "return")
                                (\v -> text "return" <+> prettyExpr v)
                                m
      Assert e       -> text "assert" <+> prettyExpr e
      Null           -> text "null"
      BlockComment c -> text c -- TODO: maybe reflow text?

-- | Handle else-if chain so the if statementes aren't nested each
-- time.
prettyIf :: Stmt -> Doc         -- TODO: Limit this to if, might need GADTs
prettyIf (If cond ts elses) =
    -- TODO: check for single-line ifs
    case elses of
      [] -> ifStart
      -- To place an else-if chain on one line, the else clause may
      -- only contain one statement, the if-statement. Because the
      -- if-statment takes over the scope from the else-statement,
      -- there is no clean way to dedent to get back to the
      -- else-statement's scope.
      BlockStmt i@If{} : [] -> ifStart $$ (dedent $ text "else" <+> prettyIf i)
      _                     -> ifStart $$ text "else" `withCode` elses
    where
        dedent = nest (negate $ length "else ")
        ifStart = (text "if" <+> prettyExpr cond <+> text "then") `withCode` ts
prettyIf _ = error "May only use if statement for pretty if."

prettyType :: Maybe String -> Doc
prettyType t = maybe empty (\s -> colon <+> text s) t

commaDelimit :: [Doc] -> Doc
commaDelimit = hsep . punctuate (char ',')

prettyParams :: [Param] -> Doc
prettyParams xs = parens . commaDelimit $ map prettyParam xs

prettyParam :: Param -> Doc
prettyParam (Param isRef name ptype) =
    case isRef of
      False -> text name <> prettyType ptype
      True -> text "ref" <+> text name <+> prettyType ptype

prettyDecl :: Decl -> Doc
prettyDecl d =
    case d of
      VarDecl name vtype expr   -> pVar "variable" name vtype expr
      ConstDecl name ctype expr -> pVar "constant" name ctype expr
      FuncDecl name params code -> pMethod "function" name params code <> newline
      ProcDecl name params code -> pMethod "procedure" name params code <> newline
      ClassDecl name parent
                implements code -> pClass name parent implements code <> newline
    where 
      pVar keyword name vtype expr =
          let exprDoc = maybe empty (\e -> text ":=" <+> prettyExpr e) expr
          in keyNameSep keyword name (prettyType vtype) "" (<>) <> exprDoc
            
      pMethod method name params code =
          keyNameSep method name (prettyParams params) "is" (<>) `withCode` code

      pClass name parent implements code =
          keyNameSep "class" name (pInherit parent <+> pSubs implements) "is" (<+>)
                         `withCode` code

      pInherit i = maybe empty (\x -> text "inherit" <+> text x) i
      pSubs xs | null xs   = empty
               | otherwise = char '<' <+> commaDelimit (map text xs)

prettyActuals :: [Expr] -> Doc
prettyActuals as = parens . commaDelimit $ map prettyExpr as

-- TODO: Doesn't handle precedence rules correctly
prettyExpr :: Expr -> Doc
prettyExpr x =
    case x of
      LiteralInt i         -> integer i
      LiteralStr s         -> doubleQuotes $ text s
      LiteralBool True     -> text "true"
      LiteralBool False    -> text "false"
      LiteralNull          -> text "null"
      LiteralThis          -> text "this"
      ClassLookup c        -> text c
      Negate e             -> char '-' <> prettyExpr e
      Not e                -> text "not" <+> prettyExpr e
      Binary b l r         -> hsep [prettyExpr l, prettyBinary b, prettyExpr r]
      VarLookup s          -> text s
      FuncCall f as        -> text f <> prettyActuals as
      ClassCall c as       -> text c <> prettyActuals as
      ParenExpr e          -> parens $ prettyExpr e
      ObjFieldExpr n e     -> prettyExpr e <> text n
      ObjMethodExpr n as e -> hcat [prettyExpr e, text n, prettyActuals as]
      LambdaExpr ps e      -> hsep [text "fun", prettyParams ps,
                                      text "is", prettyExpr e]

prettyBinary :: BinaryOp -> Doc
prettyBinary b =
    case b of
      Add              -> char '+'
      Sub              -> char '-'
      Mult             -> char '*'
      Div              -> char '/'
      Mod              -> char '%'
      Equal            -> char '='
      NotEqual         -> text "/="
      LessThan         -> char '<'
      TypeTest         -> char '?'
      LessThanEqual    -> text "<="
      GreaterThan      -> text ">"
      GreaterThanEqual -> text ">="
      And              -> text "and"
      Or               -> text "or"
      Concat           -> char '&'

prettifyString :: String -> Either String String
prettifyString ss = render . prettyBlocks <$> parseString ss

-- printString = putStrLn . prettifyString

prettifyFile :: FilePath -> IO (Either String String)
prettifyFile fname = do
  ps <- parseFile fname
  return $ render . prettyBlocks <$> ps

printPretty :: FilePath -> IO ()
printPretty fname = prettifyFile fname >>= either putStrLn putStrLn

roundTripTest :: FilePath -> IO (Bool)
roundTripTest fname = liftM2 (==) p1 p2
    where
      p1 :: IO (Either String [Block])
      p1 = do ps <- parseFile fname
              writeFile "p1.out" (show ps)
              return ps

      p2 :: IO (Either String [Block])
      p2 = do p <- prettifyFile fname
              let es = case p of
                         Left err -> Left err
                         Right ps -> parseString' fname ps
              writeFile "p2.hud" (either id id p)
              writeFile "p2.out" (show es)
              return es
