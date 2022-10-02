module Main where

import System.Environment (getArgs)

import Database (localConnString, migrateDB)

main :: IO ()
main = migrateDB localConnString