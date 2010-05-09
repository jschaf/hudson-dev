module Language.Hudson.PrettyPrinter where

import Text.PrettyPrint.HughesPJ

import Control.Monad (liftM2)

import Language.Hudson.AST
import Language.Hudson.Parser (parseFile, parseString', parseString)

import Control.Applicative ((<$>))

-- TODO
--
-- Allow withCode to use single line if-stmts, functions, etc.
--
-- Write a combinator that automatically adds continuation comments
-- for parameters.

prettyBlocks :: [Block] -> Doc
prettyBlocks bs | null bs   = empty
                | otherwise = vsep (map prettyBlock bs) <> newline

defaultHudsonIndent :: Int
defaultHudsonIndent = 4

withCode :: Doc -> [Block] -> Doc
withCode doc code = case code of
                      []   -> doc
                      -- Maybe use one line declarations.  Need to
                      -- ensure that it is not applied to multiline
                      -- statements
                      -- 
                      -- [_c] -> doc <+> nestedBlocks
                      _    -> doc $+$ nestedBlocks
    where
      nestedBlocks = vcat $ map (nest defaultHudsonIndent . prettyBlock) code

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
prettyIf (If cond ts elses elseDoc) =
    ifStart $$ case elses of
                 [] -> empty
                 -- To place an else-if chain on one line, the else
                 -- clause may only contain one statement, the
                 -- if-statement. This limitation arises because the
                 -- if-statment takes over the scope from the
                 -- else-statement and there is no clean way to dedent
                 -- to get back to the else-statement's scope.
                 BlockStmt i@If{} : [] -> prettyElseIf i
                                
                 _otherwise            -> comment $$ text "else" `withCode` elses
    where
      ifStart = (text "if" <+> prettyExpr cond <+> text "then") `withCode` ts
      comment = maybe empty text elseDoc
prettyIf _ = error "May only use if statement for pretty if."

-- We need a separate function because @nest@, and therefore
-- @withCode@, will only unnest (given a negative argument) until it
-- bumps into another doc.  In other words, the most negative value
-- for nest is the horizontal distance between the current doc and
-- preceeding doc.
prettyElseIf :: Stmt -> Doc
prettyElseIf (If cond ts elses elseDoc) =
    elseIfStart $$ case elses of
                     []                    -> empty
                     BlockStmt i@If{} : [] -> prettyElseIf i
                     _otherwise            -> comment $$ text "else" `withCode` elses
    where
      elseIfStart = (text "else if" <+> prettyExpr cond <+> text "then") `withCode` ts
      comment = maybe empty text elseDoc

prettyElseIf _ = error "May only if statement for prettyElseIf"
            
    
prettyType :: Maybe String -> Doc
prettyType t = maybe empty (\s -> colon <+> text s) t

commaDelimit :: [Doc] -> Doc
commaDelimit = hsep . punctuate (char ',')

prettyParams :: [Param] -> Doc
prettyParams = parens . commaDelimit . map prettyParam

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
      FuncDecl name params code -> pMethod "function" name params code
      ProcDecl name params code -> pMethod "procedure" name params code
      ClassDecl name parent
                implements code -> pClass name parent implements code
    where 
      pVar keyword name vtype expr =
          let exprDoc = maybe empty (\e -> text ":=" <+> prettyExpr e) expr
          in keyNameSep keyword name (prettyType vtype) "" (<>) <> exprDoc
            
      pMethod method name params code =
          keyNameSep method name (prettyParams params) "is" (<>)
                         `withCode` code <> newline

      pClass name parent implements code =
          keyNameSep "class" name (pInherit parent <+> pSubs implements) "is" (<+>)
                         `withCode` code <> newline

      pInherit i = maybe empty (\x -> text "inherit" <+> text x) i
      pSubs xs | null xs   = empty
               | otherwise = char '<' <+> commaDelimit (map text xs)

prettyActuals :: [Expr] -> Doc
prettyActuals = parens . commaDelimit . map prettyExpr

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
      Binary TypeTest l r  -> prettyExpr l <> char '?' <> prettyExpr r
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
