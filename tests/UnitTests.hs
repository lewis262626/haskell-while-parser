module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.Parsec

import WhileParser

-- should be true
testConstant123 :: Test.HUnit.Test
testConstant123 = TestCase $
   case parse parseConstant "Error" "123" of
      Left _ -> assertFailure "Failure"
      Right _ -> assertBool "Success" True

-- should be true
testConstant589 :: Test.HUnit.Test
testConstant589 = TestCase $
   case parse parseConstant "Error" "589" of
      Left _ -> assertFailure "Failure"
      Right _ -> assertBool "Success" True

-- should be failure
testConstantChars :: Test.HUnit.Test
testConstantChars = TestCase $
   case parse parseConstant "Error" "c2" of
       Left _ -> assertBool "SUCCESS" True
       Right _ -> assertFailure "Failure" 
-- should be true
testBoolTrue :: Test.HUnit.Test
testBoolTrue = TestCase $
   case parse parseBool "Error" "true" of
      Left _ -> assertFailure "Failure"
      Right _ -> assertBool "Success" True

-- should be failure
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