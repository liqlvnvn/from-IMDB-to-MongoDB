{-
Functions for parsing raw file input to [(Person, Birthday)]
Scheme:
1. Raw Input -> [Lines]
2. [Lines] -> [Only lines with names and bd]
3. [Only lines with names and bd] -> [(Name, BD)]
4. parseNameAndBD :: [(Name, BD)] -> [(Person, Birthday)]

input -> nameAndBirthday (step 2) ->  listOfTuples (step 3) -> parseNameAndBD
-}
module Parsing.ParsingIMDBFile where

import Parsing

import Data.List (isPrefixOf, elemIndex)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Control.Arrow ((***))
import Text.Regex.Posix

--
--   Step 1-3

-- | Raw input from file -> ("Lastname, Firstname", "BD, Birthplace")
rawInputToTuple :: String -> [(String, String)]
rawInputToTuple input = listOfTuples . nameAndBirthday . lines $ input
    where
        -- | Step2. Create list of strings:
        -- ["NM: ...", "DB: ...", "NM: ...", ...]
        nameAndBirthday = filter (\x -> "NM: " `isPrefixOf` x
                                     || "DB: " `isPrefixOf` x)

-- | ["NM: ...", "DB: ...", ...] -> [("Raw name string", "Raw bd string")]
listOfTuples :: [String] -> [(String, String)]
listOfTuples [] = []
listOfTuples (x:xs)
    | "NM: " `isPrefixOf` x &&
      "DB: " `isPrefixOf` head xs &&
      validBD birthday = (parseName x, birthday) : listOfTuples xs
    | otherwise = listOfTuples xs
    where birthday = parseBirthday $ head xs

-- | Check birthday string. Only "Day Month Year"
validBD :: String -> Bool
validBD str =
    str =~ "^[0-9]+ (January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{4}$"

parseName :: String -> String
parseName = drop (length "NM: ")

-- | Example: "DB: 5 March 1981, Lisbon, Portugal" -> "5 March 1981"
parseBirthday :: String -> String
parseBirthday string = separateBdFromBp sliced
    where sliced = drop (length "DB: ") string

-- | Example: "5 March 1981, Lisbon, Portugal" -> "5 March 1981"
separateBdFromBp :: String -> String
separateBdFromBp [] = []
separateBdFromBp string =
    case isTherePunc of
        Nothing -> string
        Just n  -> fst $ splitAt n string
    where isTherePunc = elemIndex ',' string

--
--   Step 4

parseNameAndBD :: [(String, String)] -> [(Person, Birthday)]
parseNameAndBD = map (parseNameToPerson *** parseDBToBirthday)

parseNameToPerson :: String -> Person
parseNameToPerson str =
    if length list == 1  -- True = No lastname; False = fullname
    then Person { firstName = head list, lastName = "" }
    else Person { firstName = list !! 1, lastName = head list }
    where list = splitOn ", " str

parseDBToBirthday :: String -> Birthday
parseDBToBirthday str =
    Birthday { day = day', month = month', year = year' }
    where list   = splitOn " " str
          day'   = read d :: Int
          month' = getMonth (list !! 1)
          year'  = read (list !! 2) :: Int
          d = stripLeadingWhitespace (head list)
          stripLeadingWhitespace = dropWhile isSpace

getMonth :: String -> Month
getMonth mon =
    case mon of
        "January"   -> January
        "February"  -> February
        "March"     -> March
        "April"     -> April
        "May"       -> May
        "June"      -> June
        "July"      -> July
        "August"    -> August
        "September" -> September
        "October"   -> October
        "November"  -> November
        "December"  -> December
        _           -> error "Wrong month"

monthToString :: Month -> String
monthToString mon =
    case mon of
        January   -> "January"
        February  -> "February"
        March     -> "March"
        April     -> "April"
        May       -> "May"
        June      -> "June"
        July      -> "July"
        August    -> "August"
        September -> "September"
        October   -> "October"
        November  -> "November"
        December  -> "December"
