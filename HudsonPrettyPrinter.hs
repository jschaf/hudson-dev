module HudsonPP where

import Text.PrettyPrint.HughesPJ
import Control.Monad

import HudsonParser (parseFile, parseString', parseString, Block(..), Stmt(..), Decl(..), Param(..), Expr(..), BinaryOp(..))
import Control.Applicative ((<$>), liftA)

-- TODO
--
-- Write a combinator that automatically adds continuation comments
-- for parameters.
--
-- Space between top level declarations

prettyBlocks :: [Block] -> Doc
prettyBlocks bs = vsep $ map prettyBlock bs

nestBlocks :: [Block] -> Doc
nestBlocks bs = vcat . map (nest 4 . prettyBlock) $ bs

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

prettyStmt (If cond ts []) =
    text "if" <+> prettyExpr cond <+> text "then" $$ nestBlocks ts

-- TODO: Special case if-else blocks

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

commaDelimit = hsep . punctuate (char ',')

prettyParams :: [Param] -> Doc
prettyParams xs = parens . commaDelimit $ map prettyParam xs

prettyParam (Param False name ptype) = text name <> prettyType ptype
prettyParam (Param True  name ptype) = text "ref" <+> text name <+> prettyType ptype


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

prettyDecl (ClassDecl name inherit subtypes code) =
    text "class" <+> text name <+> pInherit inherit <+> pSubs subtypes <+> text "is"
    $$ nestBlocks code
    where pInherit Nothing = empty
          pInherit (Just p) = text "inherit" <+> text p
          pSubs [] = empty
          pSubs xs =  char '<' <+> commaDelimit (map text xs)

prettyActuals = parens . commaDelimit . map prettyExpr

prettyExpr (LiteralInt i)         = integer i
prettyExpr (LiteralStr s)         = doubleQuotes $ text s
prettyExpr (LiteralBool True)     = text "true"
prettyExpr (LiteralBool False)    = text "false"
prettyExpr (LiteralNull)          = text "null"
prettyExpr (LiteralThis)          = text "this"
prettyExpr (ClassLookup c)        = text c
prettyExpr (Negate e)             = char '-' <> prettyExpr e
prettyExpr (Not e)                = text "not" <+> prettyExpr e
prettyExpr (Binary b l r)         = prettyExpr l <+> prettyBinary b <+> prettyExpr r
prettyExpr (VarLookup s)          = text s
prettyExpr (FuncCall f as)        = text f <> prettyActuals as
prettyExpr (ClassCall c as)       = text c <> prettyActuals as
prettyExpr (ParenExpr e)          = parens $ prettyExpr e
prettyExpr (ObjFieldExpr n e)     = prettyExpr e <> text n
prettyExpr (ObjMethodExpr n as e) = prettyExpr e <> text n <> prettyActuals as
prettyExpr (LambdaExpr ps e)      = text "fun" <+> prettyParams ps <+> text "is" <+> prettyExpr e

prettyBinary Add              = char '+'
prettyBinary Sub              = char '-'
prettyBinary Mult             = char '*'
prettyBinary Div              = char '/'
prettyBinary Mod              = char '%'
prettyBinary Equal            = char '='
prettyBinary NotEqual         = text "/="
prettyBinary LessThan         = char '<'
prettyBinary TypeTest         = char '?'
prettyBinary LessThanEqual    = text "<="
prettyBinary GreaterThan      = text ">"
prettyBinary GreaterThanEqual = text ">="
prettyBinary And              = text "and"
prettyBinary Or               = text "or"
prettyBinary Concat           = char '&'

prettifyFile :: FilePath -> IO (Either String String)
prettifyFile fname = do
  ps <- parseFile fname
  return $ render . prettyBlocks <$> ps

printPretty fname = do
  ps <- prettifyFile fname
  either putStrLn putStrLn ps

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
            
  
