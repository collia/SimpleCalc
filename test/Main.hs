
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Calculator

simpleTestPlus :: Assertion
simpleTestPlus = assertEqual "Check simple operations +"  
                 (calculateLine "1+2")
                 3

simpleTestMinus :: Assertion
simpleTestMinus = assertEqual "Check simple operations -"  
                 (calculateLine "2-1")
                 1

simpleTestMul :: Assertion
simpleTestMul = assertEqual "Check simple operations *"  
                 (calculateLine "2 * 3")
                 6

simpleTestDiv :: Assertion
simpleTestDiv = assertEqual "Check simple operations /"  
                 (calculateLine "3 / 2")
                 1.5


simpleTests = [testCase "simpleTestPlus" simpleTestPlus, testCase "simpleTestMunus" simpleTestMinus, testCase "simpleTestMul" simpleTestMul, testCase "simpleTestDiv" simpleTestDiv]

priorityTest1 :: Assertion
priorityTest1 = assertEqual "Check + *"  
                 (calculateLine "1+2*3")
                 7

priorityTest2 :: Assertion
priorityTest2 = assertEqual "Check + -"  
                 (calculateLine "1+2-3")
                 0

priorityTest3 :: Assertion
priorityTest3 = assertEqual "Check + /"  
                 (calculateLine "1+3/3")
                 2

priorityTest4 :: Assertion
priorityTest4 = assertEqual "Check + *"  
                 (calculateLine "2*3+1")
                 7

priorityTest5 :: Assertion
priorityTest5 = assertEqual "Check + -"  
                 (calculateLine "2-3 + 1")
                 0

priorityTest6 :: Assertion
priorityTest6 = assertEqual "Check + /"  
                 (calculateLine "3/3 + 1")
                 2

priorityTest7 :: Assertion
priorityTest7 = assertEqual "Check - *" 
                 1
                 (calculateLine "5-2*2")

priorityTest8 :: Assertion
priorityTest8 = assertEqual "Check - +" 
                 5
                 (calculateLine "5-2+2")

priorityTest9 :: Assertion
priorityTest9 = assertEqual "Check - /" 
                 4
                 (calculateLine "5-2/2")

priorityTest10 :: Assertion
priorityTest10 = assertEqual "Check - *" 
                 (-1)
                 (calculateLine "2*2-5")

priorityTest11 :: Assertion
priorityTest11 = assertEqual "Check - +" 
                 (-1)
                 (calculateLine "2+2-5")

priorityTest12 :: Assertion
priorityTest12 = assertEqual "Check - /" 
                 (-4)
                 (calculateLine "2/2-5")

priorityTest13 :: Assertion
priorityTest13 = assertEqual "Check * +" 
                 5
                 (calculateLine "2*2+1")

priorityTest14 :: Assertion
priorityTest14 = assertEqual "Check * -" 
                 3
                 (calculateLine "2*2-1")

priorityTest15 :: Assertion
priorityTest15 = assertEqual "Check * /" 
                 16
                 (calculateLine "4*20/5")

priorityTest16 :: Assertion
priorityTest16 = assertEqual "Check * +" 
                 5
                 (calculateLine "1+2*2")

priorityTest17 :: Assertion
priorityTest17 = assertEqual "Check * -" 
                 3
                 (calculateLine "7-2*2")

priorityTest18 :: Assertion
priorityTest18 = assertEqual "Check * /" 
                 16
                 (calculateLine "20/5*4")

priorityTest19 :: Assertion
priorityTest19 = assertEqual "Check / +" 
                 3
                 (calculateLine "2/2 + 2")

priorityTest20 :: Assertion
priorityTest20 = assertEqual "Check / -" 
                 1
                 (calculateLine "4/2 - 1")

priorityTest21 :: Assertion
priorityTest21 = assertEqual "Check / *" 
                 16
                 (calculateLine "12/3*4")

priorityTest22 :: Assertion
priorityTest22 = assertEqual "Check / +" 
                 3
                 (calculateLine "2 + 2/2")

priorityTest23 :: Assertion
priorityTest23 = assertEqual "Check / -" 
                 1
                 (calculateLine "3-4/2")

priorityTest24 :: Assertion
priorityTest24 = assertEqual "Check / *" 
                 16
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
                   9
                   (calculateLine "(1+2)*3")

parenthesesTest2 :: Assertion
parenthesesTest2 = assertEqual "Check () middle"
                   10
                   (calculateLine "1+(1+2)*3")

parenthesesTest3 :: Assertion
parenthesesTest3 = assertEqual "Check () in end"
                   9
                   (calculateLine "3*(1+2)")

parenthesesTest4 :: Assertion
parenthesesTest4 = assertEqual "Check (())"
                   141
                   (calculateLine "3*(1+2*(2+3*(3+4)))")

parenthesesTest5 :: Assertion
parenthesesTest5 = assertEqual "Check (())"
                   141
                   (calculateLine "(((3+4)*3+2)*2+1)*3")

parenthesesTest6 :: Assertion
parenthesesTest6 = assertEqual "Check () only"
                   7
                   (calculateLine "(3+4)")


parenthesesTests = [testCase "parenthesesTest1" parenthesesTest1, testCase "parenthesesTest2" parenthesesTest2, testCase "parenthesesTest3" parenthesesTest3,
                    testCase "parenthesesTest4" parenthesesTest4, testCase "parenthesesTest5" parenthesesTest5, testCase "parenthesesTest6" parenthesesTest6]

main :: IO ()
main = defaultMain  (simpleTests ++ priorityTests ++ parenthesesTests)
       
       
