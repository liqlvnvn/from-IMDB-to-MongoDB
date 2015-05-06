module Parsing.ParsingIMDBFileIO where

import Control.Exception (catch, SomeException)

import Parsing.ParsingIMDBFile
import Parsing.SignRecognition (addSign)
import Parsing

parsing :: IO [(Person, Birthday, Zodiac, Maybe Zodiac)]
parsing = do
    input <- catch (readFile fileName)
        $ \err -> print (err :: SomeException) >> return ""
    return $ addSign $ parseNameAndBD $ rawInputToTuple input
