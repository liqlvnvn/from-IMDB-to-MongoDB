module Parsing where

data Person = Person { firstName :: String
                     , lastName  :: String
                     } deriving Show

data Birthday = Birthday { day   :: Int
                         , month :: Month
                         , year  :: Int
                         } deriving Show

data Month = January | February | March | April | May | June | July | August
           | September | October | November | December
           deriving (Show, Eq)

data Zodiac = Aries | Taurus | Gemini | Cancer | Leo | Virgo | Libra | Scorpius
            | Sagittarius | Capricorn | Aquarius | Pisces
            deriving Show

fileName = "biographies.list"
