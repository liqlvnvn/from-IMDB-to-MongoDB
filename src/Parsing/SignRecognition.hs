module Parsing.SignRecognition where

import Parsing

addSign :: [(Person, Birthday)] -> [(Person, Birthday, Zodiac, Maybe Zodiac)]
addSign = map (\ x -> (fst x, snd x, recognizeTheSign $ snd x,
                                     influencingSign $ snd x))

-- | Using table from https://en.wikipedia.org/wiki/Zodiac
recognizeTheSign :: Birthday -> Zodiac
recognizeTheSign date
    | (m == March     && d >= 21) ||
      (m == April     && d <= 20) = Aries
    | (m == April     && d >= 21) ||
      (m == May       && d <= 21) = Taurus
    | (m == May       && d >= 22) ||
      (m == June      && d <= 21) = Gemini
    | (m == June      && d >= 22) ||
      (m == July      && d <= 22) = Cancer
    | (m == July      && d >= 23) ||
      (m == August    && d <= 21) = Leo
    | (m == August    && d >= 22) ||
      (m == September && d <= 23) = Virgo
    | (m == September && d >= 24) ||
      (m == October   && d <= 23) = Libra
    | (m == October   && d >= 24) ||
      (m == November  && d <= 22) = Scorpius
    | (m == November  && d >= 23) ||
      (m == December  && d <= 22) = Sagittarius
    | (m == December  && d >= 23) ||
      (m == January   && d <= 20) = Capricorn
    | (m == January   && d >= 21) ||
      (m == February  && d <= 19) = Aquarius
    | (m == February  && d >= 20) ||
      (m == March     && d <= 20) = Pisces
    | otherwise = error "Fail to recognize zodiac sign"
    where m = month date
          d = day date

-- | Using extended zodiac bound by 7 days
-- to create transition zone between 2 signs
influencingSign :: Birthday -> Maybe Zodiac
influencingSign date
    | (m == March     && d >= 14 && d <= 20) ||
      (m == April     && d >= 21 && d <= 27) = Just Aries
    | (m == April     && d >= 14 && d <= 20) ||
      (m == May       && d >= 22 && d <= 28) = Just Taurus
    | (m == May       && d >= 15 && d <= 21) ||
      (m == June      && d >= 22 && d <= 28) = Just Gemini
    | (m == June      && d >= 15 && d <= 21) ||
      (m == July      && d >= 23 && d <= 29) = Just Cancer
    | (m == July      && d >= 16 && d <= 22) ||
      (m == August    && d >= 22 && d <= 28) = Just Leo
    | (m == August    && d >= 15 && d <= 21) ||
      (m == September && d >= 24 && d <= 30) = Just Virgo
    | (m == September && d >= 17 && d <= 23) ||
      (m == October   && d >= 24 && d <= 30) = Just Libra
    | (m == October   && d >= 17 && d <= 23) ||
      (m == November  && d >= 23 && d <= 29) = Just Scorpius
    | (m == November  && d >= 17 && d <= 22) ||
      (m == December  && d >= 23 && d <= 29) = Just Sagittarius
    | (m == December  && d >= 16 && d <= 22) ||
      (m == January   && d >= 21 && d <= 27) = Just Capricorn
    | (m == January   && d >= 14 && d <= 20) ||
      (m == February  && d >= 20 && d <= 26) = Just Aquarius
    | (m == February  && d >= 13 && d <= 19) ||
      (m == March     && d >= 21 && d <= 27) = Just Pisces
    | otherwise = Nothing
    where m = month date
          d = day date

zodiacToString :: Zodiac -> String
zodiacToString zod =
    case zod of
        Aries       -> "Aries"
        Taurus      -> "Taurus"
        Gemini      -> "Gemini"
        Cancer      -> "Cancer"
        Leo         -> "Leo"
        Virgo       -> "Virgo"
        Libra       -> "Libra"
        Scorpius    -> "Scorpius"
        Sagittarius -> "Sagittarius"
        Capricorn   -> "Capricorn"
        Aquarius    -> "Aquarius"
        Pisces      -> "Pisces"
