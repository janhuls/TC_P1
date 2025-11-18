module DateTime where

import ParseLib.Derived
--import ParseLib.Core --Mag Dit ??!!!!

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime
  { date :: Date,
    time :: Time,
    utc :: Bool
  }
  deriving (Eq, Ord, Show)

data Date = Date
  { year :: Year,
    month :: Month,
    day :: Day
  }
  deriving (Eq, Ord, Show)

newtype Year = Year {runYear :: Int} deriving (Eq, Ord, Show)

newtype Month = Month {runMonth :: Int} deriving (Eq, Ord, Show)

newtype Day = Day {runDay :: Int} deriving (Eq, Ord, Show)

data Time = Time
  { hour :: Hour,
    minute :: Minute,
    second :: Second
  }
  deriving (Eq, Ord, Show)

newtype Hour = Hour {runHour :: Int} deriving (Eq, Ord, Show)

newtype Minute = Minute {runMinute :: Int} deriving (Eq, Ord, Show)

newtype Second = Second {runSecond :: Int} deriving (Eq, Ord, Show)

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseutc


parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = Year <$> natural

parseMonth :: Parser Char Month
parseMonth = Month <$> natural

parseDay :: Parser Char Day
parseDay = Day <$> natural

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = Hour <$> natural

parseMinute :: Parser Char Minute
parseMinute = fmap Minute natural

parseSecond :: Parser Char Second
parseSecond = Second <$> natural


parseutc :: Parser Char Bool
parseutc = (== 'Z') <$> anySymbol

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p s = undefined {- findEmpty (runParser p s)
  where
    findEmpty :: [(b, [a])] -> Maybe b
    findEmpty []          = Nothing
    findEmpty ((r, []):_) = Just r
    findEmpty (_:xs)      = findEmpty xs -}

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime (run parseDateTime s)

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
