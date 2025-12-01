module DateTime where

import ParseLib.Derived
import Text.Read (Lexeme(String))


-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime
  { date :: Date,
    time :: Time,
    utc :: Bool
  }
  deriving (Eq)

instance Show DateTime where
  show = printDateTime

data Date = Date
  { year :: Year,
    month :: Month,
    day :: Day
  }
  deriving (Eq, Show)

newtype Year = Year {runYear :: Int} deriving (Eq, Ord, Show)

newtype Month = Month {runMonth :: Int} deriving (Eq, Ord, Show)

newtype Day = Day {runDay :: Int} deriving (Eq, Ord, Show)

data Time = Time
  { hour :: Hour,
    minute :: Minute,
    second :: Second
  }
  deriving (Eq, Show)

newtype Hour = Hour {runHour :: Int} deriving (Eq, Ord, Show)

newtype Minute = Minute {runMinute :: Int} deriving (Eq, Ord, Show)

newtype Second = Second {runSecond :: Int} deriving (Eq, Ord, Show)

instance Ord Date where
  compare (Date y1 m1 d1) (Date y2 m2 d2) =
    compare y1 y2 <> compare m1 m2 <> compare d1 d2

instance Ord Time where
  compare (Time h1 m1 s1) (Time h2 m2 s2) =
    compare h1 h2 <> compare m1 m2 <> compare s1 s2

instance Ord DateTime where
  compare (DateTime date1 time1 _) (DateTime date2 time2 _) =
    compare date1 date2 <> compare time1 time2

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


parse2Digits :: Parser Char Int
parse2Digits = collapse2Digits <$> newdigit <*> newdigit

collapse2Digits :: Int -> Int -> Int
collapse2Digits a b = a * 10 + b

parse4Digits :: Parser Char Int
parse4Digits = collapse4Digits <$> newdigit <*> newdigit <*> newdigit <*> newdigit

collapse4Digits :: Int -> Int -> Int -> Int -> Int
collapse4Digits a b c d = 1000 * a + 100 * b + 10 * c + d


-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p s = findEmpty (parse p s)
  where
    findEmpty :: [(b, [a])] -> Maybe b
    findEmpty []          = Nothing
    findEmpty ((r, []):_) = Just r
    findEmpty (_:xs)      = findEmpty xs

-- Exercise 3

print2 :: Int -> String
print2 n = let s = show n in if length s == 1 then '0' : s else s

print4 :: Int -> String
print4 i 
  | i < 10 = '0' : '0' : '0' : show i
  | i < 100 = '0' : '0' : show i
  | i < 1000 = '0' : show i
  | otherwise = show i


printDateTime :: DateTime -> String
printDateTime (DateTime {date = Date {year = Year yr, month = Month mo, day = Day d},
                         time = Time {hour = Hour hr, minute = Minute min, second = Second sec},
                         utc = u}) = 
                          print4 yr ++
                          print2 mo ++
                          print2 d ++
                          "T" ++
                          print2 hr ++
                          print2 min ++
                          print2 sec ++
                          if u then "Z" else ""

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime (run parseDateTime s)

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime d t _) = checkDate d && checkTime t

checkDate :: Date -> Bool
checkDate (Date (Year yr) (Month mo) (Day da)) =
  inRange mo 1 12 &&
  inRange da 1 (daysInMonth yr mo)

daysInMonth :: Int -> Int -> Int
daysInMonth y m =
  case m of
    1  -> 31
    2  -> if isLeapYear y then 29 else 28
    3  -> 31
    4  -> 30
    5  -> 31
    6  -> 30
    7  -> 31
    8  -> 31
    9  -> 30
    10 -> 31
    11 -> 30
    12 -> 31
    _  -> 0

isLeapYear :: Int -> Bool
isLeapYear y =
  (y `mod` 400 == 0) || (y `mod` 4 == 0 && y `mod` 100 /= 0)

checkTime :: Time -> Bool
checkTime (Time (Hour hr) (Minute min) (Second sec)) =
  inRange hr 0 23 &&
  inRange min 0 59 &&
  inRange sec 0 59

inRange :: (Ord a, Eq a) => a -> a -> a -> Bool
inRange x a b = a <= x && b >= x