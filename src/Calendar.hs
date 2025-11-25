module Calendar where

import DateTime
import GHC.Generics (Generic)
import GradeLib.CustomASTData (CustomData)
import ParseLib.Derived

-- Exercise 6
data Calendar = Calendar
  { header :: Header,
    events :: [Event]
  }
  deriving (Eq, Ord, Show)

data Header = Header Prodid Version deriving (Eq, Ord, Show, Generic)
instance CustomData Header
newtype Prodid = Prodid String deriving (Eq, Ord, Show, Generic)
instance CustomData Prodid
newtype Version = Version String deriving (Eq, Ord, Show, Generic)
instance CustomData Version

data Event = Event
  { dtStamp :: DTStamp,
    uid :: UID,
    dtStart :: DTStart,
    dtEnd :: DTEnd,
    description :: Description,
    summary :: Summary,
    location :: Location
  }
  deriving (Eq, Ord, Show)

newtype DTStamp = DTStamp DateTime deriving (Eq, Ord, Show, Generic)
instance CustomData DTStamp
newtype DTStart = DTStart DateTime deriving (Eq, Ord, Show, Generic)
instance CustomData DTStart
newtype DTEnd = DTEnd DateTime deriving (Eq, Ord, Show, Generic)
instance CustomData DTEnd
newtype UID = UID String deriving (Eq, Ord, Show, Generic)
instance CustomData UID
newtype Description = Descr String deriving (Eq, Ord, Show, Generic)
instance CustomData Description
newtype Summary = Summ String deriving (Eq, Ord, Show, Generic)
instance CustomData Summary
newtype Location = Loc String deriving (Eq, Ord, Show, Generic)
instance CustomData Location

-- If you plan on using your own types in Calendar, Event, or Token. Make sure it derives Eq, Generic, and CustomData.
-- Example:
-- data ExampleCustomData = ExampleCustomData
--   deriving (Eq, Ord, Show, Generic, CustomData)

-- Exercise 7
data Token = Token String String --Token key value
  deriving (Eq, Ord, Show)

getKey :: Token -> String
getKey (Token k _) = k

getVal :: Token -> String
getVal (Token _ v) = v

lexCalendar :: Parser Char [Token]
lexCalendar = many lexLine

lexLine :: Parser Char Token
lexLine = Token <$> keyParser <* symbol ':' <*> valueParser <* crlfParser

crlfParser :: Parser Char ()
crlfParser = token "\r\n" *> epsilon

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
parseCalendar = Calendar <$> (parseStart *> parseHeader) <*> many parseEvent <* parseEnd

parseStart :: Parser Token ()
parseStart = symbol (Token "BEGIN" "VCALENDAR") *> epsilon

parseEnd :: Parser Token ()
parseEnd = symbol (Token "END" "VCALENDAR") *> epsilon

parseHeader :: Parser Token Header
parseHeader = (Header <$> parseProdid <*> parseVersion) <|> (flip Header <$> parseVersion <*> parseProdid)

parseProdid :: Parser Token Prodid
parseProdid = Prodid . getVal <$> satisfyKey "PRODID"

parseVersion :: Parser Token Version
parseVersion = Version . getVal <$> satisfyKey "VERSION"

parseEvent :: Parser Token Event
parseEvent = parseEventStart *> parseEventParts <* parseEventEnd

parseEventStart :: Parser Token ()
parseEventStart = symbol (Token "BEGIN" "VEVENT") *> epsilon

parseEventEnd :: Parser Token ()
parseEventEnd = symbol (Token "END" "VEVENT") *> epsilon

parseEventParts :: Parser Token Event
parseEventParts = undefined

parseEventPart :: String -> Parser Token a
parseEventPart s = case s of 
  "UID"         -> UID <$> parseText
  "DTSTAMP"     -> DTStamp <$> parseDateTime'
  "DTSTART"     -> DTStart <$> parseDateTime'
  "DTEND"       -> DTEnd <$> parseDateTime'
  "SUMMARY"     -> Summ <$> parseText
  "DESCRIPTION" -> Descr <$> parseText
  "LOCATION"    -> Loc <$> parseText

parseDateTime' :: Parser Token DateTime
parseDateTime' = undefined

parseText :: Parser Token String
parseText = undefined

satisfyKey :: String -> Parser Token Token
satisfyKey s = satisfy (\t -> getKey t == s)

--choice operator as in ParseLib.core 
(<|>) :: Parser s a -> Parser s a -> Parser s a
p <|> q = choice $ p : [q]


parseCalendar' :: String -> Maybe Calendar
parseCalendar' s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
