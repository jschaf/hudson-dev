module HudsonTest where

import Text.ParserCombinators.Parsec
import HudsonParser
import Test.HUnit

param a b c = Just $ Param {ref = a, paramName = b, pType = c}

parseResult p i = either (const Nothing) Just $ parse p "Hudson" i

testParamSimple = "Simple param" ~:
                  param False "param" Nothing ~=?
                  parseResult parseParam "param"

testParamRef = "Param with a ref" ~:
               param True "param_3A" Nothing ~=?
               parseResult parseParam "ref param_3A"

testParamType = "Param with a ref and type" ~:
                param True "param_3A" (Just "Type") ~=?
                parseResult parseParam "ref param_3A : Type"

paramTests = TestList [testParamSimple, testParamRef, testParamType]