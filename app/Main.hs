module Main where

import MaybeT.Before
import MaybeT.After

runMaybeTEg :: IO ()
runMaybeTEg = foo >> putStrLn "\n" >> betterFoo

main :: IO ()
main = runMaybeTEg
