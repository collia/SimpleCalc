
import Control.Applicative
import Control.Monad
import System.IO
import Control.Monad.Writer.Lazy
import Data.Char
import Data.List
import Control.Monad.Error


import Calculator


mainLoop :: IO()
mainLoop = forever $ do
    hSetBuffering stdout NoBuffering
    command <- putStr "> " *> getLine 
    putStrLn $ show $ calculateLine command
 
 


main :: IO()
main = 
     mainLoop
     --calcTreeTest
    -- divideTextLineTest
     --dividedCommandToTreeTest


