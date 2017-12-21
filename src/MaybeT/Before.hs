module MaybeT.Before
  where

import Data.Char
import Data.Maybe
import Control.Monad

getValidData :: IO (Maybe String)
getValidData = liftM isValid getLine
  where
    isValid str = case filter isLetter str of
                     [] -> Nothing
                     xs -> Just xs

processData1 :: String -> IO (Maybe String)
processData1 _ = liftM Just getLine

processData2 :: String -> IO (Maybe String)
processData2 str = liftM (isValid str) getLine
  where
    isValid "a" = Just
    isValid _ = const Nothing

outputError :: IO ()
outputError = putStrLn "Invalid Data!"

outputData :: String -> IO ()
outputData = putStrLn

foo :: IO ()
foo = do
  maybe_data <- getValidData
  case maybe_data of
    Just some -> do
      maybe_data1 <- processData1 some
      case maybe_data1 of
        Just some -> do
          maybe_final_data <- processData2 some
          case maybe_final_data of
            Just some -> outputData some
            Nothing -> outputError
        Nothing -> outputError
    Nothing -> outputError

