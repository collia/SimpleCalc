{-# LANGUAGE DeriveDataTypeable #-}

module Calculator (
       calculateLine,
       NumberType,
       CalculatingErrorMessage,
       InvalidCommand,
       calcTreeTest,
       divideTextLineTest,
       dividedCommandToTreeTest,
       mainTest
) where

--import Data.Functor
--import Control.Applicative
import Control.Monad 
--import System.IO
import Control.Monad.Writer.Lazy
import Data.Char
--import Control.Monad.Writer.Lazy

import Control.Exception
--import Data.String.Utils
import Data.Typeable


type NumberType = Double
type CalculatingErrorMessage = String
--type LoggingMessage =  String

data InvalidCommand = InvalidCommand CalculatingErrorMessage deriving (Show, Typeable)

instance Exception InvalidCommand

data Operation =  Plus | Minus  | Multiply |  Dividion  deriving(Show, Eq, Ord)
--data Operation =   Minus  | Plus | Multiply |  Dividion  deriving(Show, Eq, Ord)
data Num value => OperTree value = Data value | Oper Operation  (OperTree value) (OperTree value) | EmptyTree
                                   deriving (Show)
data DividedContent a = Var a | Num a | Sign a |  Null  deriving (Show)
data Subtree a = SubtreeElem a | SubtreeParent [Subtree a] deriving(Show)



calcTree :: OperTree NumberType ->  NumberType
calcTree (Oper Dividion left (Data 0.0)) = throw $ InvalidCommand "Divide on zero"
calcTree (Oper oper left right) =
         case oper of
              Multiply  -> calcTree left * calcTree right
              Dividion  -> calcTree left / calcTree right
              Plus  -> calcTree left + calcTree right
              Minus  -> calcTree left - calcTree right
calcTree (Data value) = value

calcTreeTest =         do
        print $  Oper Plus (Data 1) (Data 2)
        print $ calcTree $ Oper Plus (Oper Multiply (Data 2) (Data 3)) (Data 1)
        print $ calcTree $ Oper Plus (Data 1) (Oper Multiply (Data 2.32) (Data 3))

            

infixr 5  .++  
(.++) ::  Show a => DividedContent [a] -> DividedContent [a] ->  DividedContent [a] 
Var var .++ Var ys = Var (var ++ ys )
Num var .++ Num ys = Num (var ++ ys )
Sign var .++ Sign ys =Sign ( var ++ ys ) 
-- Error var .++ Error ys =Error ( var ++ ys ) 
-- Error var .++ _ =Error  var  
-- _ .++ Error var =Error  var  
Null .++ ys = ys
ys .++ Null = ys
a .++ b = throw $ InvalidCommand ("Can't concate: " ++ (show a) ++ " and " ++ (show b) )
-- Error "Incorrect Concatinating" ++ (show a) ++ " and " ++ (show b)


divideTextLine :: [DividedContent String] -> String -> [DividedContent String]
divideTextLine  [] (c:command)
              | isSpace c = divideTextLine [] command
              | isDigit c = divideTextLine [Num [c]] command 
              | isLetter c = divideTextLine [Var [c]] command 
              | c `elem` "-("  = divideTextLine [Sign [c],  Null] command  
              | otherwise = throw $ InvalidCommand ("Wrong symbol: " ++ [c])
--              | otherwise = divideTextLine [Error $ "Wrong symbol: " ++ [c]] command
divideTextLine result (c:command) 
              | isSpace c = divideTextLine result command
              | isDigit c = divideTextLine (init result ++ [(last result .++ Num [c])]) command
              | isLetter c = divideTextLine (init result ++ [(last result .++ Var [c])]) command 
              | c `elem` "+-*/()"  = divideTextLine (result ++ [Sign [c]] ++ [Null]) command 
              | otherwise = throw $ InvalidCommand ("Wrong symbol: " ++ [c])
--              | otherwise = divideTextLine (result ++ [Error $ "Wrong symbol: " ++ [c]] ++ [Null]) command
divideTextLine r [] = r

divideTextLineTest = print $ divideTextLine [] "22+22 - (a*3)"



stringToOperation :: String -> Operation
stringToOperation a 
                 | a == "-" = Minus
                 | a == "+" = Plus
                 | a == "*" = Multiply
                 | a == "/" = Dividion



groupDividedCommand :: [Subtree (DividedContent String)] ->  [DividedContent String] -> ([ Subtree (DividedContent String)], [DividedContent String] )
groupDividedCommand tree (Sign "(" :other) = let result = (groupDividedCommand [] other)
                                                 parent =  fst result
                                                 nextStep = snd result 
                                                 result2 = (groupDividedCommand (tree ++ [SubtreeParent parent]) nextStep)  in
                                             (fst result2 , snd result2)
groupDividedCommand tree (Sign ")" :other) = (tree, other)
groupDividedCommand tree (Null :other) = let result = (groupDividedCommand tree other)
                                             parent = fst result
                                             nextStep = snd result in
                                         (parent, nextStep)
groupDividedCommand tree (a:other) = let result = groupDividedCommand (tree ++ [SubtreeElem a]) other
                                         parent = fst result
                                         nextStep = snd result in
                                     (parent, nextStep)
groupDividedCommand tree [] = (tree, [])



dividedCommandToTree ::  OperTree NumberType -> [Subtree (DividedContent String)] -> OperTree NumberType
dividedCommandToTree EmptyTree ((SubtreeElem (Num command)): other) = dividedCommandToTree (Data (read command :: NumberType)) other 
dividedCommandToTree EmptyTree ((SubtreeElem (Sign "-"   )): other) = dividedCommandToTree (Oper Minus (Data 0) (EmptyTree)) other --(Oper Minus (Data 0) (dividedCommandToTree (EmptyTree) other))
dividedCommandToTree (Data a)  ((SubtreeElem (Sign s     )): other) =  (dividedCommandToTree (Oper (stringToOperation s)  (Data a) (EmptyTree)) other)
dividedCommandToTree (Oper a left EmptyTree) ((SubtreeElem (Num command)): other) = dividedCommandToTree (Oper a left (Data (read command :: NumberType))) other
dividedCommandToTree (Oper a left right)     ((SubtreeElem (Sign s     )): other) =
                                                      if ((stringToOperation s) > a) then 
                                                         (Oper a  left (dividedCommandToTree (Oper (stringToOperation s)  right (EmptyTree)) other ))
                                                      else
                                                         (dividedCommandToTree (Oper (stringToOperation s) (Oper a  left right) EmptyTree) other)
dividedCommandToTree res [] = res
dividedCommandToTree EmptyTree ((SubtreeParent commands): (SubtreeElem (Sign s) ): other) = dividedCommandToTree (Oper (stringToOperation s) (dividedCommandToTree EmptyTree commands) EmptyTree) other 
dividedCommandToTree EmptyTree [(SubtreeParent commands)] = (dividedCommandToTree EmptyTree commands)
dividedCommandToTree (Oper a left EmptyTree) ((SubtreeParent commands): other) = dividedCommandToTree (Oper a left  (dividedCommandToTree EmptyTree commands)) other
dividedCommandToTree (Oper a EmptyTree right) ((SubtreeParent commands): other) = dividedCommandToTree (Oper a (dividedCommandToTree EmptyTree commands) right) other



dividedCommandToTreeTest = do
                           --print $  divideTextLine  [] "(((3+4)*3+2)*2+1)*3"
                           --print $  (  groupDividedCommand [] . divideTextLine  []) "(((3+4)*3+2)*2+1)*3"
                           --print $  (  dividedCommandToTree EmptyTree . fst . groupDividedCommand [] . divideTextLine  []) "(((3+4)*3+2)*2+1)*3"
                           --print $  (   fst . groupDividedCommand [] . divideTextLine  []) "(3+4)"
                           --print $  (  dividedCommandToTree EmptyTree . fst . groupDividedCommand [] . divideTextLine  []) "(3+4)"
                           print $ show $ Plus > Minus
                           print $  (  groupDividedCommand [] . divideTextLine  []) "0-3+4"
                           print $  (  groupDividedCommand [] . divideTextLine  []) "-3+4"
                           print $  (  dividedCommandToTree EmptyTree . fst . groupDividedCommand [] . divideTextLine  []) "0-3+4"
                           print $  (  dividedCommandToTree EmptyTree . fst . groupDividedCommand [] . divideTextLine  []) "-3+4"

mainTest = do
         print $ (calcTree . dividedCommandToTree EmptyTree . fst . groupDividedCommand [] . divideTextLine []) "1+2"
         print $ (calcTree . dividedCommandToTree EmptyTree . fst . groupDividedCommand [] . divideTextLine []) "(((3+4)*3+2)*2+1)*3" 
         --print $ try ((calcTree . dividedCommandToTree EmptyTree . fst . groupDividedCommand [] . divideTextLine []) "1 / 0")



calculateLine :: String -> NumberType
calculateLine command = (calcTree .  dividedCommandToTree EmptyTree . fst . groupDividedCommand [] . divideTextLine []) command


