
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Control.Exception

import Calculator

simpleTestPlus :: Assertion
simpleTestPlus = assertEqual "Check simple operations +"  
                 (Just 3)
                 (calculateLine "1+2")

simpleTestMinus :: Assertion
simpleTestMinus = assertEqual "Check simple operations -"  
                 (Just 1)
                 (calculateLine "2-1")

simpleTestMul :: Assertion
simpleTestMul = assertEqual "Check simple operations *"  
                 (Just 6)
                 (calculateLine "2 * 3")


simpleTestDiv :: Assertion
simpleTestDiv = assertEqual "Check simple operations /"  
                 (Just 1.5)
                 (calculateLine "3 / 2")



simpleTests = [testCase "simpleTestPlus" simpleTestPlus, testCase "simpleTestMunus" simpleTestMinus, testCase "simpleTestMul" simpleTestMul, testCase "simpleTestDiv" simpleTestDiv]

priorityTest1 :: Assertion
priorityTest1 = assertEqual "Check + *"  
                 (calculateLine "1+2*3")
                 (Just 7)

priorityTest2 :: Assertion
priorityTest2 = assertEqual "Check + -"  
                 (calculateLine "1+2-3")
                 (Just 0)

priorityTest3 :: Assertion
priorityTest3 = assertEqual "Check + /"  
                 (calculateLine "1+3/3")
                 (Just 2)

priorityTest4 :: Assertion
priorityTest4 = assertEqual "Check + *"  
                 (calculateLine "2*3+1")
                 (Just 7)

priorityTest5 :: Assertion
priorityTest5 = assertEqual "Check + -"  
                 (calculateLine "2-3 + 1")
                 (Just 0)

priorityTest6 :: Assertion
priorityTest6 = assertEqual "Check + /"  
                 (calculateLine "3/3 + 1")
                 (Just 2)

priorityTest7 :: Assertion
priorityTest7 = assertEqual "Check - *" 
                 (Just 1)
                 (calculateLine "5-2*2")

priorityTest8 :: Assertion
priorityTest8 = assertEqual "Check - +" 
                 (Just 5)
                 (calculateLine "5-2+2")

priorityTest9 :: Assertion
priorityTest9 = assertEqual "Check - /" 
                 (Just 4)
                 (calculateLine "5-2/2")

priorityTest10 :: Assertion
priorityTest10 = assertEqual "Check - *" 
                 (Just (-1))
                 (calculateLine "2*2-5")

priorityTest11 :: Assertion
priorityTest11 = assertEqual "Check - +" 
                 (Just (-1))
                 (calculateLine "2+2-5")

priorityTest12 :: Assertion
priorityTest12 = assertEqual "Check - /" 
                 (Just (-4))
                 (calculateLine "2/2-5")

priorityTest13 :: Assertion
priorityTest13 = assertEqual "Check * +" 
                 (Just 5)
                 (calculateLine "2*2+1")

priorityTest14 :: Assertion
priorityTest14 = assertEqual "Check * -" 
                 (Just 3)
                 (calculateLine "2*2-1")

priorityTest15 :: Assertion
priorityTest15 = assertEqual "Check * /" 
                 (Just 16)
                 (calculateLine "4*20/5")

priorityTest16 :: Assertion
priorityTest16 = assertEqual "Check * +" 
                 (Just 5)
                 (calculateLine "1+2*2")

priorityTest17 :: Assertion
priorityTest17 = assertEqual "Check * -" 
                 (Just 3)
                 (calculateLine "7-2*2")

priorityTest18 :: Assertion
priorityTest18 = assertEqual "Check * /" 
                 (Just 16)
                 (calculateLine "20/5*4")

priorityTest19 :: Assertion
priorityTest19 = assertEqual "Check / +" 
                 (Just 3)
                 (calculateLine "2/2 + 2")

priorityTest20 :: Assertion
priorityTest20 = assertEqual "Check / -" 
                 (Just 1)
                 (calculateLine "4/2 - 1")

priorityTest21 :: Assertion
priorityTest21 = assertEqual "Check / *" 
                 (Just 16)
                 (calculateLine "12/3*4")

priorityTest22 :: Assertion
priorityTest22 = assertEqual "Check / +" 
                 (Just 3)
                 (calculateLine "2 + 2/2")

priorityTest23 :: Assertion
priorityTest23 = assertEqual "Check / -" 
                 (Just 1)
                 (calculateLine "3-4/2")

priorityTest24 :: Assertion
priorityTest24 = assertEqual "Check / *" 
                 (Just 16)
                 (calculateLine "4*12/3")



priorityTests = [testCase "priorityTest1" priorityTest1, testCase "priorityTest2" priorityTest2, testCase "priorityTest3" priorityTest3,
                 testCase "priorityTest4" priorityTest4, testCase "priorityTest5" priorityTest5, testCase "priorityTest6" priorityTest6,
                 testCase "priorityTest7" priorityTest7, testCase "priorityTest8" priorityTest8, testCase "priorityTest9" priorityTest9,
                 testCase "priorityTest10" priorityTest10, testCase "priorityTest11" priorityTest11, testCase "priorityTest12" priorityTest12,
                 testCase "priorityTest13" priorityTest13, testCase "priorityTest14" priorityTest14, testCase "priorityTest15" priorityTest15,
                 testCase "priorityTest16" priorityTest16, testCase "priorityTest17" priorityTest17, testCase "priorityTest18" priorityTest18,
                 testCase "priorityTest19" priorityTest19, testCase "priorityTest20" priorityTest20, testCase "priorityTest21" priorityTest21,
                 testCase "priorityTest22" priorityTest22, testCase "priorityTest23" priorityTest23, testCase "priorityTest24" priorityTest24]

parenthesesTest1 :: Assertion
parenthesesTest1 = assertEqual "Check () before"
                   (Just 9)
                   (calculateLine "(1+2)*3")

parenthesesTest2 :: Assertion
parenthesesTest2 = assertEqual "Check () middle"
                   (Just 10)
                   (calculateLine "1+(1+2)*3")

parenthesesTest3 :: Assertion
parenthesesTest3 = assertEqual "Check () in end"
                   (Just 9)
                   (calculateLine "3*(1+2)")

parenthesesTest4 :: Assertion
parenthesesTest4 = assertEqual "Check (())"
                   (Just 141)
                   (calculateLine "3*(1+2*(2+3*(3+4)))")

parenthesesTest5 :: Assertion
parenthesesTest5 = assertEqual "Check (())"
                   (Just 141)
                   (calculateLine "(((3+4)*3+2)*2+1)*3")

parenthesesTest6 :: Assertion
parenthesesTest6 = assertEqual "Check () only"
                   (Just 7)
                   (calculateLine "(3+4)")

parenthesesTest7 :: Assertion
parenthesesTest7 = assertEqual "Check ()() only"
                   (Just 77)
                   (calculateLine "(3+4)*(5+6)")

parenthesesTest8 :: Assertion
parenthesesTest8 = assertEqual "Check (()()) only"
                   (Just (18*4))
                   (calculateLine "2 * ((3+4)+(5+6)) * 2")

parenthesesTest9 :: Assertion
parenthesesTest9 = assertEqual "Check () only 2"
                   (Just (-1))
                   (calculateLine "(-1)")

parenthesesTests = [testCase "parenthesesTest1" parenthesesTest1, testCase "parenthesesTest2" parenthesesTest2, testCase "parenthesesTest3" parenthesesTest3,
                    testCase "parenthesesTest4" parenthesesTest4, testCase "parenthesesTest5" parenthesesTest5, testCase "parenthesesTest6" parenthesesTest6,
                    testCase "parenthesesTest7" parenthesesTest7, testCase "parenthesesTest8" parenthesesTest8, testCase "parenthesesTest9" parenthesesTest9]

negativeTest1 :: Assertion
negativeTest1 = assertEqual "Check -1"
                   (Just (-1))
                   (calculateLine "-1")

negativeTest2 :: Assertion
negativeTest2 = assertEqual "Check neg +"
                   (Just (-1))
                   (calculateLine "-3+2")

negativeTest3 :: Assertion
negativeTest3 = assertEqual "Check neg -"
                   (Just (-5))
                   (calculateLine "0-3-2")

negativeTest4 :: Assertion
negativeTest4 = assertEqual "Check neg *"
                   (Just (-6))
                   (calculateLine "-3*2")

negativeTest5 :: Assertion
negativeTest5 = assertEqual "Check neg /"
                   (Just (-3))
                   (calculateLine "-6/2")

negativeTest6 :: Assertion
negativeTest6 = assertEqual "Check neg ()"
                   (Just (-1))
                   (calculateLine "-(3-2)")

negativeTests = [testCase "negativeTest1" negativeTest1, testCase "negativeTest2" negativeTest2, testCase "negativeTest3" negativeTest3,
                 testCase "negativeTest4" negativeTest4, testCase "negativeTest5" negativeTest5, testCase "negativeTest6" negativeTest6]

exceptionTest1 :: Assertion
exceptionTest1 = do
               result <- try $ evaluate $ calculateLine "1/0"
                    :: IO (Either InvalidCommand (Maybe NumberType))
               case result of
                    Left exception -> assertBool (show exception) True
                    Right value -> assertFailure "No exception"

exceptionTest2 :: Assertion
exceptionTest2 = do
               result <- try $ evaluate $ calculateLine "1,0"
                    :: IO (Either InvalidCommand (Maybe NumberType))
               case result of
                    Left exception -> assertBool (show exception) True
                    Right value -> assertFailure "No exception"

exceptionTest3 :: Assertion
exceptionTest3 = do
               result <- try $ evaluate $ calculateLine "|"
                    :: IO (Either InvalidCommand (Maybe NumberType))
               case result of
                    Left exception -> assertBool (show exception) True
                    Right value -> assertFailure "No exception"

exceptionTest4 :: Assertion
exceptionTest4 = do
               result <- try $ evaluate $ calculateLine ""
                    :: IO (Either InvalidCommand (Maybe NumberType))
               case result of
                    Left exception -> assertFailure "Exception"
                    Right value -> assertBool (show value) True 

exceptionTests = [testCase "exceptionTest1" exceptionTest1, testCase "exceptionTest2" exceptionTest2, testCase "exceptionTest3" exceptionTest3, 
                  testCase "exceptionTest4" exceptionTest4]

main :: IO ()
main = do
     defaultMain ( simpleTests ++ priorityTests ++ parenthesesTests ++ negativeTests ++ exceptionTests)
       
       
