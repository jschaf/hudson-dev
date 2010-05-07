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
prettyBlocks [] = empty
prettyBlocks bs = vsep $ map prettyBlock bs

nestBlocks :: [Block] -> Doc
nestBlocks bs = nestBlocks' 4 bs

nestBlocks' :: Int -> [Block] -> Doc
nestBlocks' i bs = vcat . map (nest i . prettyBlock) $ bs

prettyBlock :: Block -> Doc
prettyBlock (BlockStmt s) = prettyStmt s
prettyBlock (BlockDecl d) = prettyDecl d

vsep :: [Doc] -> Doc
vsep = foldl1 ($+$)

prettyStmt :: Stmt -> Doc

prettyStmt (Assignment v e) =
    prettyExpr v <+> text ":=" <+> prettyExpr e

prettyStmt (ProcCall v ps) =
    text v <> (parens . sep . punctuate (char ',') . map prettyExpr $ ps)

prettyStmt (ObjCall e) = prettyExpr e

prettyStmt i@(If _ _ []) = prettyIf nestBlocks (const empty) i

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


prettyStmt i@(If{}) =
    prettyIf nestBlocks ((text "else" $$) . nestBlocks) i

prettyStmt (While cond ws) =
    text "while" <+> prettyExpr cond <+> text "do" $$ nestBlocks ws

prettyStmt (Return (Just e)) = text "return" <+> prettyExpr e
prettyStmt (Return Nothing) = text "return"
prettyStmt (Assert e) = text "assert" <+> prettyExpr e
prettyStmt Null = text "null"

prettyIf :: ([Block] -> Doc) -> ([Block] -> Doc) -> Stmt -> Doc
prettyIf ifToDoc elseToDoc (If cond ts es) =
    text "if" <+> prettyExpr cond <+> text "then" $$ ifToDoc ts $$ elseToDoc es
prettyIf _ _ _ = error "prettyIf can only be called on an if statement"    

assign = text ":="
prettyType Nothing = empty
prettyType (Just s) = colon <+> text s

commaDelimit = hsep . punctuate (char ',')

prettyParams :: [Param] -> Doc
prettyParams xs = parens . commaDelimit $ map prettyParam xs

prettyParam (Param isRef name ptype) =
    case isRef of
      False -> text name <> prettyType ptype
      True -> text "ref" <+> text name <+> prettyType ptype

prettyMaybe = maybe (text "")

prettyVar' keyword name type' expr =
    text keyword <+> text name <> prettyType type'
             <+> prettyMaybe ((text ":=" <+>) . prettyExpr) expr

prettyDecl (VarDecl name vtype expr) = prettyVar' "variable" name vtype expr

prettyDecl (ConstDecl name ctype expr) = prettyVar' "constant" name ctype expr


prettyDecl (FuncDecl name params code) =
    text "function" <+> text name <> prettyParams params <+> text "is"
    $$ nestBlocks code

prettyDecl (ProcDecl name params code) =
    text "procedure" <+> text name <> prettyParams params <+> text "is"
    $$ nestBlocks code

prettyDecl (ClassDecl name parent implements code) =
    text "class" <+> text name <+> pInherit parent <+> pSubs implements <+> text "is"
    $$ nestBlocks code
    where pInherit Nothing = empty
          pInherit (Just p) = text "inherit" <+> text p
          pSubs [] = empty
          pSubs xs =  char '<' <+> commaDelimit (map text xs)

prettyActuals = parens . commaDelimit . map prettyExpr

-- TODO: Doesn't handle precedence rules correctly
prettyExpr x = case x of
                 (LiteralInt i)         -> integer i
                 (LiteralStr s)         -> doubleQuotes $ text s
                 (LiteralBool True)     -> text "true"
                 (LiteralBool False)    -> text "false"
                 (LiteralNull)          -> text "null"
                 (LiteralThis)          -> text "this"
                 (ClassLookup c)        -> text c
                 (Negate e)             -> char '-' <> prettyExpr e
                 (Not e)                -> text "not" <+> prettyExpr e
                 (Binary b l r)         -> prettyExpr l <+> prettyBinary b <+> prettyExpr r
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
            
if0 = "if true then k()"

if1 = "if true then\n   k()\nelse\n   j()"

if2 = "if true then\n\
      \   e()\n\
      \else if f() then\n\
      \   j()\n\
      \   m()\n\
      \else if l() then\n\
      \   p()\n\
      \   r()\n\
      \else k()"
