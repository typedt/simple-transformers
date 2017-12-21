module After
  where

import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

type NewMonad = MaybeT IO

getValidData :: NewMonad String
getValidData = MaybeT $ liftM isValid getLine
  where
    isValid str = case filter isLetter str of
                     [] -> Nothing
                     xs -> Just xs

processData1 :: String -> NewMonad String
processData1 _ = lift getLine

processData2 :: String -> NewMonad String
processData2 str = MaybeT $ liftM (isValid str) getLine
  where
    isValid "a" = Just
    isValid _ = const Nothing

outputError :: IO ()
outputError = putStrLn "Invalid Data!"

outputData :: String -> IO ()
outputData = putStrLn

betterFoo :: IO ()
betterFoo = do
  maybe_final_data <- runMaybeT $ getValidData >>= processData1 >>= processData2
  maybe outputError outputData maybe_final_data

