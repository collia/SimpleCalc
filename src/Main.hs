
import Control.Applicative
import Control.Monad
import System.IO
import Control.Monad.Writer.Lazy
import Data.Char
import Data.List
import Control.Monad.Error
import Control.Exception

import Calculator

{-calculate :: String -> IO String
calculate x = 
          (show $ eval $ calculateLine x) `Control.Exception.catch` possibleErrors
          where 
                possibleErrors :: InvalidCommand -> IO String
                possibleErrors  error = return $ "Error happens: " ++ show error
-}
mainLoop :: IO()
mainLoop = forever $ do
    hSetBuffering stdout NoBuffering
    command <- putStr "> " *> getLine 
   -- result <- calculate command
   -- putStrLn $ result
    result <- try $ evaluate $ calculateLine command
                    :: IO (Either InvalidCommand (Maybe NumberType))
    case result of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right (Just value) -> print value
        Right Nothing -> putStrLn ""
 
testMaybe :: Int -> Maybe Int
testMaybe 0 = Nothing
testMaybe 1 = Just 1
testMaybe 100 = Just 100
testMaybe x = (\x y -> x+y) <$> testMaybe ( (x - 1) ) <*> testMaybe ( (x + 1) )


main :: IO()
main = 
     mainLoop
     --calcTreeTest
    -- divideTextLineTest
     --dividedCommandToTreeTest
    --mainTest
    --print $ testMaybe 2


