module DateTime where

import ParseLib.Derived

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

newtype Year = Year {runYear :: Int4Digits} deriving (Eq, Ord, Show)

newtype Month = Month {runMonth :: Int2Digits} deriving (Eq, Ord, Show)

newtype Day = Day {runDay :: Int2Digits} deriving (Eq, Ord, Show)

data Time = Time
  { hour :: Hour,
    minute :: Minute,
    second :: Second
  }
  deriving (Eq, Ord, Show)

newtype Hour = Hour {runHour :: Int2Digits} deriving (Eq, Ord, Show)

newtype Minute = Minute {runMinute :: Int2Digits} deriving (Eq, Ord, Show)

newtype Second = Second {runSecond :: Int2Digits} deriving (Eq, Ord, Show)

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseutc


parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = Year <$> parse4Digits

parseMonth :: Parser Char Month
parseMonth = Month <$> parse2Digits

parseDay :: Parser Char Day
parseDay = Day <$> parse2Digits

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = Hour <$> parse2Digits

parseMinute :: Parser Char Minute
parseMinute = fmap Minute parse2Digits

parseSecond :: Parser Char Second
parseSecond = Second <$> parse2Digits


parseutc :: Parser Char Bool
parseutc = True <$ symbol 'Z' <<|> (False <$ epsilon)


parse2Digits :: Parser Char Int2Digits
parse2Digits = collapse2Digits <$> newdigit <*> newdigit

collapse2Digits :: Int -> Int -> Int2Digits
collapse2Digits a b = Int2Digits $ a * 10 + b

parse4Digits :: Parser Char Int4Digits
parse4Digits = collapse4Digits <$> newdigit <*> newdigit <*> newdigit <*> newdigit

collapse4Digits :: Int -> Int -> Int -> Int -> Int4Digits
collapse4Digits a b c d = Int4Digits $ 1000 * a + 100 * b + 10 * c + d


-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p s = findEmpty (parse p s)
  where
    findEmpty :: [(b, [a])] -> Maybe b
    findEmpty []          = Nothing
    findEmpty ((r, []):_) = Just r
    findEmpty (_:xs)      = findEmpty xs

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime {date = Date {year = yr, month = mo, day = d},
                         time = Time {hour = hr, minute = min, second = sec},
                         utc = u}) = show (runYear yr) ++
                          concatMap show
                          [runMonth mo, runDay d ] ++
                          "T" ++
                          concatMap show
                          [runHour hr, runMinute min, runSecond sec]
                          ++ if u then "Z" else ""

newtype Int2Digits = Int2Digits Int deriving (Eq, Ord)
newtype Int4Digits = Int4Digits Int deriving (Eq, Ord)

instance Show Int2Digits where
  show (Int2Digits i) = if i < 10 then '0' : show i else show i

instance Show Int4Digits where
  show (Int4Digits i)
    | i < 10 = '0' : '0' : '0' : show i
    | i < 100 = '0' : '0' : show i
    | i < 1000 = '0' : show i
    | otherwise = show i

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime (run parseDateTime s)

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
