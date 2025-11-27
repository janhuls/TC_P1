{-# LANGUAGE DeriveGeneric #-}

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
  deriving (Eq, Ord)

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
    description :: Maybe Description,
    summary :: Maybe Summary,
    location :: Maybe Location
  }
  deriving (Eq, Ord)

newtype DTStamp = DTStamp DateTime deriving (Eq, Ord, Generic)
instance CustomData DTStamp
newtype DTStart = DTStart DateTime deriving (Eq, Ord, Generic)
instance CustomData DTStart
newtype DTEnd = DTEnd DateTime deriving (Eq, Ord, Generic)
instance CustomData DTEnd
newtype UID = UID String deriving (Eq, Ord, Show, Generic)
instance CustomData UID
newtype Description = Descr String deriving (Eq, Ord, Generic)
instance CustomData Description
newtype Summary = Summ String deriving (Eq, Ord, Generic)
instance CustomData Summary
newtype Location = Loc String deriving (Eq, Ord, Generic)
instance CustomData Location

getDescription :: Description -> String
getDescription (Descr a) = a

getSummary :: Summary -> String
getSummary (Summ s) = s

getLocation :: Location -> String
getLocation (Loc p) = p

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
crlfParser = (token "\r\n" <|> token "\n") *> epsilon

keyParser :: Parser Char String
keyParser = many1 (satisfy (/= ':'))

valueParser :: Parser Char String
valueParser = (++) <$> firstLine <*> restLines where
  firstLine = many (satisfy (\c -> c /= '\r' && c /= '\n'))
  restLines = concat <$> many
            (   crlfParser
             *> symbol ' '
             *> ((++) "\n" <$> many (satisfy (\c -> c /= '\r' && c /= '\n')))
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

data EventParts = EventParts
  { epDtStamp    :: Maybe DTStamp
  , epUid        :: Maybe UID
  , epDtStart    :: Maybe DTStart
  , epDtEnd      :: Maybe DTEnd
  , epDescription :: Maybe Description
  , epSummary    :: Maybe Summary
  , epLocation   :: Maybe Location
  }

emptyParts :: EventParts
emptyParts = EventParts Nothing Nothing Nothing Nothing Nothing Nothing Nothing

parseEventPart :: Parser Token (EventParts -> EventParts)
parseEventPart = choice 
    [(\x p -> p { epUid = Just x })       <$> parseUID,
    (\x p -> p { epDtStamp = Just x })    <$> parseDTStamp,
    (\x p -> p { epDtStart = Just x })    <$> parseDTStart,
    (\x p -> p { epDtEnd = Just x })      <$> parseDTEnd,
    (\x p -> p { epSummary = Just x })    <$> parseSummary,
    (\x p -> p { epDescription = Just x })<$> parseDescr,
    (\x p -> p { epLocation = Just x })   <$> parseLocation]

parseEventParts :: Parser Token Event
parseEventParts = do
  updaters <- many parseEventPart
  let parts = foldl (\acc f -> f acc) emptyParts updaters
  case parts of
    EventParts (Just ds) (Just u) (Just st) (Just en) d s l ->
      pure (Event ds u st en d s l)
    _ -> failp  -- niet compleet

parseUID       = UID     <$> parseText
parseDTStamp   = DTStamp <$> parseDateTime'
parseDTStart   = DTStart <$> parseDateTime'
parseDTEnd     = DTEnd   <$> parseDateTime'
parseSummary   = Summ    <$> parseText
parseDescr     = Descr   <$> parseText
parseLocation  = Loc     <$> parseText

parseDateTime' :: Parser Token DateTime --pak token en return DateTime value
parseDateTime' = do
  tok <- satisfy (const True)
  case run parseDateTime (getVal tok) of
    Just dt -> pure dt
    Nothing -> failp

parseText :: Parser Token String --pak token en return text value
parseText = getVal <$> satisfy (const True)

satisfyKey :: String -> Parser Token Token
satisfyKey s = satisfy (\t -> getKey t == s)

--choice operator as in ParseLib.core 
(<|>) :: Parser s a -> Parser s a -> Parser s a
p <|> q = choice $ p : [q]
infixr 3 <|>

parseCalendar' :: String -> Maybe Calendar
parseCalendar' s = run lexCalendar s >>= run parseCalendar

-- Exercise 8

maxLine :: Int -- max lengte lijn
maxLine = 75 --(?) idk wat de max lengte is maar aanpasbaar iig vgm 75

splitOnChar :: Char -> String -> [String] -- splitten op char, handig voor tokenlines builden later
splitOnChar _ "" = [""]
splitOnChar c s =
  case break (== c) s of
    (x, [])     -> [x]
    (x, _:rest) -> x : splitOnChar c rest

splitInto :: Int -> String -> [String] -- splitten naar delen van n tekens
splitInto _ "" = []
splitInto n s  = take n s : splitInto n (drop n s)

-- lijnen bouwen voor eerste deel dat na KEY: volgt
buildFirstChunkLines :: String -> String -> [String]
buildFirstChunkLines key chunk = --waar key is summary of description etc en chunk is t deel dat daarmee volgt
  let prefix = key ++ ":"
      charsleft  = maxLine - length prefix
      (firstPart, rest) = splitAt (max 0 charsleft) chunk
      firstLine = prefix ++ firstPart
      restPieces = splitInto (maxLine - 1) rest
  in firstLine : map (" " ++) restPieces

crlf = "\r\n"

instance Show Event where
  show (Event (DTStamp ds) (UID u) (DTStart dst) (DTEnd de) d s l) = 
    "DTSTAMP:" ++ printDateTime ds ++ crlf ++
    "UID:" ++ u ++ crlf ++ -- moet u zijn ipv show u want anders krijg je quotes
    "DTSTART:" ++ printDateTime dst ++ crlf ++ 
    "DTEND:" ++ printDateTime de ++ crlf ++
    case d of 
     Nothing -> ""
     (Just jd) -> concatMap (++ crlf) (buildFirstChunkLines "DESCRIPTION" (getDescription jd)) 
     ++
    case s of 
     Nothing -> ""
     (Just js) -> concatMap (++ crlf) (buildFirstChunkLines "SUMMARY" (getSummary js)) 
     ++
    case l of 
     Nothing -> ""
     (Just jl) -> concatMap (++ crlf) (buildFirstChunkLines "LOCATION" (getLocation jl)) 

instance Show Calendar where
  show = printCalendar

printCalendar :: Calendar -> String                     
printCalendar (Calendar (Header (Prodid prodidString) (Version versionString)) events) = 
  "BEGIN:VCALENDAR" ++ crlf ++
  "VERSION:" ++ versionString ++ crlf ++
  "PRODID:" ++ prodidString ++ crlf ++
  concatMap (\e -> "BEGIN:VEVENT" ++ crlf ++ show e ++ crlf ++ "END:VEVENT" ++ crlf) events ++
  "END:VCALENDAR"
