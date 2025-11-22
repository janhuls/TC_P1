module Calendar where

import DateTime
import GHC.Generics (Generic)
import GradeLib.CustomASTData (CustomData)
import ParseLib.Derived

-- Exercise 6
data Calendar = Calendar
  { prodid :: Prodid,
    version :: Version,
    events :: [Event]
  }
  deriving (Eq, Ord, Show)

newtype Prodid = Prodid String deriving (Eq, Ord, Show, Generic, CustomData)
newtype Version = Version String deriving (Eq, Ord, Show, Generic, CustomData)

data Event = Event
  { dtStamp :: DateTime,
    uid :: UID,
    dtStart :: DateTime,
    dtEnd :: DateTime,
    description :: Description,
    summary :: Summary,
    location :: Location
  }
  deriving (Eq, Ord, Show)

newtype UID = UID String deriving (Eq, Ord, Show, Generic, CustomData)
newtype Description = Descr String deriving (Eq, Ord, Show, Generic, CustomData)
newtype Summary = Summ String deriving (Eq, Ord, Show, Generic, CustomData)
newtype Location = Loc String deriving (Eq, Ord, Show, Generic, CustomData)

-- If you plan on using your own types in Calendar, Event, or Token. Make sure it derives Eq, Generic, and CustomData.
-- Example:
-- data ExampleCustomData = ExampleCustomData
--   deriving (Eq, Ord, Show, Generic, CustomData)

-- Exercise 7
data Token = Token String String --key value
  deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = many lexLine

lexLine :: Parser Char Token
lexLine = Token <$> keyParser <* symbol ':' <*> valueParser <* crlfParser

crlfParser :: Parser Char String
crlfParser = token "\r\n"

keyParser :: Parser Char String
keyParser = many1 (satisfy (/= ':'))

valueParser :: Parser Char String
valueParser = (++) <$> firstLine <*> restLines where
  firstLine = many (satisfy (/= '\r'))
  restLines = concat <$> many
            (   crlfParser
             *> symbol ' '
             *> ((++) "\n" <$> many (satisfy (/= '\r')))
            )

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

parseCalendar' :: String -> Maybe Calendar
parseCalendar' s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
