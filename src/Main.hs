
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
 

main :: IO()
main = 
     --mainLoop
     --calcTreeTest
    -- divideTextLineTest
     dividedCommandToTreeTest
     --mainTest
    


