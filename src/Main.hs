
import Data.Functor
import Control.Applicative
import Control.Monad 
import System.IO
import Control.Monad.Writer.Lazy
import Data.Char
import Data.List
import Control.Monad.Error

import Calculator

type ErrorMessage = String
type CommandTree = [String]
--type LoggingData = WriterT CommandTree IO String
type LoggingMessage =  String

parseCommand :: String -> Writer LoggingMessage CommandTree
parseCommand str = do
               tell $ "Parsed" ++ (show tree)
               return tree
               where tree = words str

calculateCommand :: Num a => CommandTree -> Writer LoggingMessage a
calculateCommand command = do
                 tell " Try:"
                 return 123

calculate :: Num a => String -> Writer LoggingMessage a
calculate command = parseCommand command >>= calculateCommand 
                  
mainLoop :: IO()
mainLoop = forever $ do
    hSetBuffering stdout NoBuffering
    command <- putStr "> " *> getLine 
    let result = runWriter $ calculate command 
    putStrLn $ show $ fst result
    putStrLn $ "Log: " ++ (snd result)
 

{-testErrorInt :: Int -> Either ErrorMessage Int
testErrorInt 0 = throwError "Is zero"
testErrorInt a = return (a-1)

testErrorStringInt :: String -> Either ErrorMessage Int
testErrorStringInt "zero" = return 0
testErrorStringInt "one" = return 1
testErrorStringInt _ = throwError "Incorrect string"

testErrorIntString :: Int -> Either ErrorMessage String
testErrorIntString 0 = return "zero"
testErrorIntString 1 = return "one"
testErrorIntString 2 = return "two"
testErrorIntString _ = throwError "Incorrect value"-}

testLogging1 :: Int -> Writer LoggingMessage Int
testLogging1 0 = do
             tell "Finish"
             return 0
testLogging1 a = do
              tell ("1: Get: " ++ show a ++ "\n")
              testLogging1 (a -1)

testLogging2 :: Int -> Writer LoggingMessage Int
testLogging2 0 = do
             tell "set 5 \n" 
             return 5
testLogging2 a = do
              tell ("2: Get: " ++ show a ++ "\n")
              return $ (a +1)

takeWhileEnd :: (a -> Bool) -> [a] -> [a] 
takeWhileEnd f = (reverse . takeWhile f . reverse)


data Subtree a = SubtreeElem a | SubtreeParent [Subtree a] deriving(Show)
parceParentnes :: String -> [Subtree String]
parceParentnes a 
               | not $ null $ filter (\x ->  elem x "()") a = [SubtreeElem (takeWhile (/= '(') a)] ++
                                [SubtreeParent (parceParentnes 
                                  ((tail . init)
                                    (dropWhile (/= '(') 
                                      (dropWhileEnd (/= ')') a))))] ++
                                [SubtreeElem (takeWhileEnd (/= ')') a)]
              | otherwise =  [SubtreeElem a]

main :: IO()
main = 
     --mainLoop
     --calcTreeTest
    -- divideTextLineTest
     dividedCommandToTreeTest
{-    do
    print $ parceParentnes "(1 + ( 2+ 3 ) * 4 + 5 ) -6 + 7"
--    print $ parceParentnes "(1 + ( 2+ 3 ) * (4 + 5)) -6 + 7"
    print $ ((tail . init) (dropWhile (/= '(') (dropWhileEnd (/= ')') " 2+ 3 ) * (4 + 5")))-}
    --print $ not $ null $ filter (\x ->  elem x "()") "(1 + ( 2+ 3 ) *4 ) -5 + 6" 
    --print $ takeWhileEnd (/= ')')  "(1 + ( 2+ 3 ) * 4 + 5 ) -6 + 7"
     --mainTest
     --print $ (last [Num "123", Var "a", Sign "+", Num "123"]) .++ "3"
     
{-     putStrLn $ case result of
        Left error -> "Hm... Error occured: " ++ error
        Right number -> "number is " ++ show number
        where result = testErrorIntString 1 >>= 
                       testErrorStringInt >>= 
                       testErrorInt -}
{-       do
       let result = runWriter $ testLogging2 1 >>= testLogging1
       putStrLn  $ show $ fst result
       putStrLn  $ "LOG: \n" ++ (snd result)-}

