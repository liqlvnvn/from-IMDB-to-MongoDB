{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module FromFileToDB where

import Parsing.ParsingIMDBFileIO (parsing)
import Parsing.ParsingIMDBFile
import Parsing.SignRecognition
import Parsing

import Database.MongoDB
import Data.Tuple.Select

fromFileToMongoDB :: IO ()
fromFileToMongoDB = do
    myData <- parsing
    pipe <- connect (host "127.0.0.1")
    listOfActions (createMongoDocs myData) pipe
    close pipe

-- | List of MongoDB inserts
listOfActions :: [Document] -> Pipe -> IO ()
listOfActions [] _ = return ()
listOfActions (x:xs) pipe = do
    e <- access pipe master "Actors" (insert "actors" x)
    print e
    listOfActions xs pipe

-- | Create a list of MongoDB docs that ready for inserting in DB
createMongoDocs :: [(Person, Birthday, Zodiac, Maybe Zodiac)] -> [Document]
createMongoDocs [] = []
createMongoDocs (x:xs) =
    ["first" =: firstName person,
    "last"  =: lastName person,
    "birthday" =: ["day" =: day birthday,
                  "month" =: monthToString (month birthday),
                  "year" =: year birthday
                  ],
    "zodiac" =: zodiac,
    "inf_zodiac" =: zodiac'
    ] :  createMongoDocs xs
    where person = sel1 x
          birthday = sel2 x
          zodiac = zodiacToString $ sel3 x
          zodiac' = maybe "Nothing" zodiacToString (sel4 x)
