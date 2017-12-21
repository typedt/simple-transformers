module Main where

import Before
import After

main :: IO ()
main = putStrLn "Before:" >> foo >> putStrLn "\nAfter:" >> betterFoo
