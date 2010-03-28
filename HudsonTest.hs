module HudsonTest where

import Text.ParserCombinators.Parsec
import HudsonParser
import Test.HUnit
import HudsonPreproc (parseResult)
param a b c = Just Param {ref = a, paramName = b, paramType = c}
func a b c = Just FuncDecl {funcName = a, funcParams = b, funcCode = c}

testParamSimple = "Simple param" ~:
                  param False "param" Nothing ~=?
                  parseResult parseParam "param"

testParamRef = "Param with a ref" ~:
               param True "param_3A" Nothing ~=?
               parseResult parseParam "ref param_3A"

testParamType = "Param with a ref and type" ~:
                param True "param_3A" (Just "Type") ~=?
                parseResult parseParam "ref param_3A : Type"


testParamNewline = "Param with a bad newline" ~:
                   Nothing ~=? parseResult parseParam "\nref"

paramTests = TestLabel "param Tests" $
             TestList [testParamSimple, testParamRef, testParamType, testParamNewline]

testFuncSimple = "Simple Func" ~:
                 func "simple" [] [BlockStmt $ Return $ Just $ LiteralInt 2] ~=?
                 parseResult parseFuncDecl "function simple () is\n   return 2"
                 

funcTests = TestLabel "func Tests" $
            TestList [testFuncSimple]

allTests = TestList [paramTests, funcTests]

main = runTestTT allTests