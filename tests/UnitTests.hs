module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.Parsec

import WhileParser

testConstant123 :: Test.HUnit.Test
testConstant123 = TestCase $ assertEqual "Constant 123"
   (parse constant "Error" "123") (Right 123 :: Either ParseError Int)

testConstant589 :: Test.HUnit.Test
testConstant589 = TestCase $ assertEqual "Constant 589"
   (parse constant "Error" "589") (Right 589 :: Either ParseError Int)

testConstantChars :: Test.HUnit.Test
testConstantChars = TestCase $
   case parse constant "Error" "c2" of
       Left _ -> assertBool "SUCCESS" True
       Right _ -> assertFailure "Failure"

testBoolTrue :: Test.HUnit.Test
testBoolTrue = TestCase $ assertEqual "parseBool true"
   (parse parseBool "Error" "true") (Right True :: Either ParseError Bool)

testBoolTrue2 :: Test.HUnit.Test
testBoolTrue2 = TestCase $
   case parse parseBool "Error" "true2" of
       Left _ -> assertBool "SUCCESS" True
       Right _ -> assertFailure "Failure" 

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList [TestLabel "testConstants123" testConstant123,
                    TestLabel "testConstant589" testConstant589,
                    TestLabel "testConstantChars" testConstantChars,
                    TestLabel "testBoolTrue" testBoolTrue,
                    TestLabel "testBoolTrue2" testBoolTrue2]

main :: IO ()
main = defaultMain tests