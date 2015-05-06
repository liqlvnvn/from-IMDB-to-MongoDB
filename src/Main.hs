module Main where

import FromFileToDB (fromFileToMongoDB)

main :: IO ()
main = fromFileToMongoDB
