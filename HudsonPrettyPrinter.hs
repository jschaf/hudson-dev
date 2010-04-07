module HudsonPP where

import Text.PrettyPrint.HughesPJ

import HudsonParser
import HudsonPreproc (parseResult)

-- TODO
--
-- Write a combinator that automatically adds continuation comments
-- for parameters.
--
-- Space between top level declarations
prettyBlocks :: [Block] -> Doc
prettyBlocks bs = fsep $ map prettyBlock bs

nestBlocks :: [Block] -> Doc
nestBlocks bs = vcat . map (nest 4 . prettyBlock) $ bs

prettyBlock :: Block -> Doc
prettyBlock (BlockStmt s) = prettyStmt s
prettyBlock (BlockDecl d) = prettyDecl d

vsep :: [Doc] -> Doc
vsep = foldl1 ($+$)

prettyStmt :: Stmt -> Doc

prettyStmt (Assignment v e) =
    text v <+> text ":=" <+> prettyExpr e

prettyStmt (ProcCall v ps) =
    text v <> (parens . sep . punctuate (char ',') . map prettyExpr $ ps)

prettyStmt (If cond ts []) =
    text "if" <+> prettyExpr cond <+> text "then" $$ nestBlocks ts

prettyStmt (If cond ts es) =
    text "if" <+> prettyExpr cond <+> text "then" $$ nestBlocks ts $$
    text "else" $$ nestBlocks es

prettyStmt (While cond ws) =
    text "while" <+> prettyExpr cond <+> text "do" $$ nestBlocks ws

prettyStmt (Return (Just e)) = text "return" <+> prettyExpr e
prettyStmt (Return Nothing) = text "return"
prettyStmt (Assert e) = text "assert" <+> prettyExpr e
prettyStmt Null = text "null"

assign = text ":="
prettyType Nothing = text ""
prettyType (Just s) = colon <+> text s

commaDelimit = sep . punctuate (char ',')

prettyParams :: [Param] -> Doc
prettyParams xs = parens . commaDelimit $ map prettyParam xs

prettyParam (Param False name ptype) = text name <+> prettyType ptype
prettyParam (Param True  name ptype) = text "ref" <+> text name <+> prettyType ptype

prettyDecl (VarDecl varName vtype expr) =
    text "variable" <+> prettyType vtype <+> assign <+> prettyExpr expr

prettyDecl (ConstDecl name ctype expr) =
    text "constant" <+> prettyType ctype <+> assign <+> prettyExpr expr

prettyDecl (FuncDecl name params code) =
    text "function" <+> text name <> prettyParams params <+> text "is"
    $$ nestBlocks code

prettyDecl (ProcDecl name params code) =
    text "procedure" <+> text name <> prettyParams params <+> text "is"
    $$ nestBlocks code

prettyDecl (ClassDecl name inherit subtypes code) =
    text "class" <+> text name <+> pInherit inherit <+> pSubs subtypes
    $$ prettyBlocks code
    where pInherit Nothing = empty
          pInherit (Just p) = text "inherit" <+> text p
          pSubs [] = empty
          pSubs xs = hsep . punctuate (char ',') $ map text xs

prettyExpr (LiteralInt i) = integer i
prettyExpr (LiteralStr s) = text s
prettyExpr (LiteralBool True) = text "true"
prettyExpr (LiteralBool False) = text "false"
prettyExpr (LiteralNull) = text "null"
prettyExpr (Negate e) = char '-' <> prettyExpr e
prettyExpr (Not e) = text "not" <> prettyExpr e
prettyExpr (Binary b l r) = prettyExpr l <+> prettyBinary b <+> prettyExpr r
prettyExpr (VarID s) = text s
prettyExpr (FuncCall s) = text s <> text "()"
prettyExpr (TypeTest e s) = prettyExpr e <> char '?' <> text s

prettyBinary :: BinaryOp -> Doc
prettyBinary Add = char '+'
prettyBinary Sub = char '-'
prettyBinary Mult = char '*'
prettyBinary Div = char '/'
prettyBinary Mod = text "mod"
prettyBinary Equal = char '='
prettyBinary NotEqual = text "/="
prettyBinary LessThan = char '<'
prettyBinary LessThanEqual = text "<="
prettyBinary GreaterThan = text ">"
prettyBinary GreaterThanEqual = text ">="
prettyBinary And = text "and"
prettyBinary Or = text "or"
prettyBinary Concat = char '&'

prettify s = maybe "" (render . prettyBlocks) $ parseInput s

prettifyFile fname = do
  input <- readFile fname
  putStrLn $ prettify input

pp = putStrLn . prettify
pf = prettifyFile