module Language.Hudson.PrettyPrinter where

import Text.PrettyPrint.HughesPJ

import Control.Monad (liftM2)

import Language.Hudson.Parser (parseFile, parseString', parseString, Block(..),
                               Stmt(..), Decl(..), Param(..), Expr(..), BinaryOp(..))

import Control.Applicative ((<$>))

-- TODO
--
-- Write a combinator that automatically adds continuation comments
-- for parameters.
--
-- Space between top level declarations

prettyBlocks :: [Block] -> Doc
prettyBlocks bs | null bs   = empty
                | otherwise = vsep $ map prettyBlock bs

hudsonIndent :: Int
hudsonIndent = 4

withCode :: Doc -> [Block] -> Doc
withCode doc code = doc $$ (vcat $ map (nest hudsonIndent . prettyBlock) code)

keyNameSep :: String              -- ^ keyword
           -> String              -- ^ name
           -> Doc                 -- ^ middle part
           -> String              -- ^ separator
           -> (Doc -> Doc -> Doc) -- ^ how to combine the name and middle
           -> Doc
keyNameSep keyword name middle sep combine =
    (text keyword <+> text name) `combine` middle <+> text sep

prettyBlock :: Block -> Doc
prettyBlock b = case b of
                  (BlockStmt s) -> prettyStmt s
                  (BlockDecl d) -> prettyDecl d

vsep :: [Doc] -> Doc
vsep = foldl1 ($+$)


prettyStmt :: Stmt -> Doc
prettyStmt s =
    case s of
      (Assignment v e) -> prettyExpr v <+> text ":=" <+> prettyExpr e
      (ProcCall v ps)  -> text v <> (parens . commaDelimit $ map prettyExpr ps)
      (ObjCall e)      -> prettyExpr e
      i@(If{})         -> prettyIf i
      (While cond ws)  -> text "while" <+> prettyExpr cond <+> text "do"
                          `withCode` ws
      (Return m)       -> maybe (text "return")
                                (\v -> text "return" <+> prettyExpr v)
                                m
      (Assert e)       -> text "assert" <+> prettyExpr e
      Null             -> text "null"

prettyIf (If{}) = text "blaH"
-- prettyStmt i@(If _ _ []) = prettyIf nestBlocks (const empty) i

-- -- | Handle else-if chain so the if statementes aren't nested each
-- -- time.
-- prettyStmt i@(If cond ts (BlockStmt If{} : es)) =
--     text "if" <+> prettyExpr cond <+> text "then" $$ nestBlocks ts
--     $$ text "else if" <+> prettyExpr cond <+> text "then" $$ nestBlocks ts
--     prettyIf nestBlocks f i
--         where
--           f :: [Block] -> Doc
--           f (BlockStmt b@If{} : bs) = text "else" <+> prettyIf dedent elseNest b $$ prettyBlocks bs

--           dedent :: [Block] -> Doc
--           dedent = nest (negate . length $ "else ") . nestBlocks

--           elseNest :: [Block] -> Doc
--           elseNest bs = nest (negate $ length "else ") (text "else") $$ dedent bs

-- prettyStmt i@(If{}) =
--     prettyIf nestBlocks ((text "else" $$) . nestBlocks) i

-- prettyIf :: ([Block] -> Doc) -> ([Block] -> Doc) -> Stmt -> Doc
-- prettyIf ifToDoc elseToDoc (If cond ts es) =
--     text "if" <+> prettyExpr cond <+> text "then" $$ ifToDoc ts $$ elseToDoc es
-- prettyIf _ _ _ = error "prettyIf can only be called on an if statement"

prettyType :: Maybe String -> Doc
prettyType t = maybe empty (\s -> colon <+> text s) t

commaDelimit = hsep . punctuate (char ',')

prettyParams :: [Param] -> Doc
prettyParams xs = parens . commaDelimit $ map prettyParam xs

prettyParam (Param isRef name ptype) =
    case isRef of
      False -> text name <> prettyType ptype
      True -> text "ref" <+> text name <+> prettyType ptype

prettyDecl d =
    case d of
      (VarDecl name vtype expr)   -> pVar "variable" name vtype expr
      (ConstDecl name ctype expr) -> pVar "constant" name ctype expr
      (FuncDecl name params code) -> pMethod "function" name params code
      (ProcDecl name params code) -> pMethod "procedure" name params code
      (ClassDecl name parent
                 implements code) -> pClass name parent implements code
    where 
      pVar keyword name vtype expr =
          let exprDoc = maybe empty (\e -> text ":=" <+> prettyExpr e) expr
          in keyNameSep keyword name (prettyType vtype) ":=" (<>) <+> exprDoc
            
      pMethod method name params code =
          keyNameSep method name (prettyParams params) "is" (<>) `withCode` code

      pClass name parent implements code =
          keyNameSep "class" name (pInherit parent <+> pSubs implements) "is" (<+>)
                         `withCode` code

      pInherit i = maybe empty (\d -> text "inherit" <+> text d) i
      pSubs xs | null xs   = empty
               | otherwise = char '<' <+> commaDelimit (map text xs)

prettyActuals as = parens . commaDelimit $ map prettyExpr as

-- TODO: Doesn't handle precedence rules correctly
prettyExpr x =
    case x of
      (LiteralInt i)         -> integer i
      (LiteralStr s)         -> doubleQuotes $ text s
      (LiteralBool True)     -> text "true"
      (LiteralBool False)    -> text "false"
      (LiteralNull)          -> text "null"
      (LiteralThis)          -> text "this"
      (ClassLookup c)        -> text c
      (Negate e)             -> char '-' <> prettyExpr e
      (Not e)                -> text "not" <+> prettyExpr e
      (Binary b l r)         -> hsep [prettyExpr l, prettyBinary b, prettyExpr r]
      (VarLookup s)          -> text s
      (FuncCall f as)        -> text f <> prettyActuals as
      (ClassCall c as)       -> text c <> prettyActuals as
      (ParenExpr e)          -> parens $ prettyExpr e
      (ObjFieldExpr n e)     -> prettyExpr e <> text n
      (ObjMethodExpr n as e) -> hcat [prettyExpr e, text n, prettyActuals as]
      (LambdaExpr ps e)      -> hsep [text "fun", prettyParams ps,
                                      text "is", prettyExpr e]

prettyBinary b = case b of
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

prettifyString :: String -> String
prettifyString ss = either id (render . prettyBlocks) (parseString ss)

printString = putStrLn . prettifyString

prettifyFile :: FilePath -> IO (Either String String)
prettifyFile fname = do
  ps <- parseFile fname
  return $ render . prettyBlocks <$> ps

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
