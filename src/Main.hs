
import Data.Functor
import Control.Applicative
import Control.Monad 
import System.IO
import Control.Monad.Writer.Lazy
import Data.Char
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
 

testErrorInt :: Int -> Either ErrorMessage Int
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
testErrorIntString _ = throwError "Incorrect value"

main :: IO()
main = 
     --mainLoop
     --calcTreeTest
     -- divideTextLineTest
     dividedCommandToTreeTest
     --mainTest
     --print $ (last [Num "123", Var "a", Sign "+", Num "123"]) .++ "3"
     
{-     putStrLn $ case result of
        Left error -> "Hm... Error occured: " ++ error
        Right number -> "number is " ++ show number
        where result = testErrorIntString 1 >>= 
                       testErrorStringInt >>= 
                       testErrorInt -}

