module Main where

import System.Environment (getArgs) 

import qualified BasicServer as B

main :: IO ()
main = putStrLn "Server starts..." >> B.runServer