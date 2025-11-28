module Features where

import DateTime
import Calendar
import Foreign (toBool)


-- Exercise 9
countEvents :: Calendar -> Int
countEvents c = length $ events c


overlaps :: DateTime -> DateTime -> DateTime -> DateTime -> Bool
overlaps start1 end1 start2 end2 =
  not (end1 <= start2 || end2 <= start1)

checkEvent :: DateTime -> Event -> Bool
checkEvent moment (Event {dtStart = DTStart start, dtEnd = DTEnd end}) = moment < end && moment >= start

findEvents :: DateTime -> Calendar -> [Event]
findEvents moment (Calendar {events = events})= filter (checkEvent moment) events


getDateFromDTStart :: DTStart -> DateTime
getDateFromDTStart (DTStart dt) = dt

getDateFromDTEnd :: DTEnd -> DateTime
getDateFromDTEnd (DTEnd dt) = dt

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar {events = events})= checkOverlappingEvents events where
    checkOverlappingEvents :: [Event] -> Bool
    checkOverlappingEvents (e:es) = all (checkEvent (getDateFromDTStart $ dtStart e)) events && checkOverlappingEvents es


yearDays :: Int -> Int
yearDays y = (y - 1) * 365 + leapYearsUpTo (y - 1)

leapYearsUpTo :: Int -> Int
leapYearsUpTo y = y `div` 4 - y `div` 100 + y `div` 400

monthDays :: Int -> Int -> Int
monthDays year month =
  sum (take (month - 1) (months year))

months :: Int -> [Int]
months y =
  [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  where
    feb = if isLeapYear y then 29 else 28

dateToDays :: Date -> Int
dateToDays (Date (Year y) (Month m) (Day d)) =
    yearDays yr + monthDays yr mo + da
  where
    yr = getIntFromInt4Digits y
    mo = getIntFromInt2Digits m
    da = getIntFromInt2Digits d

timeToMinutes :: Time -> Int
timeToMinutes (Time hr min sec) =
  h * 60 + m + s `div` 60 where
    h = getIntFromInt2Digits $ runHour hr
    m = getIntFromInt2Digits $ runMinute min
    s = getIntFromInt2Digits $ runSecond sec

dateTimeToMinutes :: DateTime -> Int
dateTimeToMinutes (DateTime d t _) =
  dateToDays d * 1440 + timeToMinutes t

eventDurationMinutes :: Event -> Int
eventDurationMinutes (Event {dtStart = DTStart start, dtEnd = DTEnd end}) =
  abs (dateTimeToMinutes end - dateTimeToMinutes start)


timeSpent :: String -> Calendar -> Int
timeSpent summ (Calendar {events = events})= sum [eventDurationMinutes e | e <- events, matchesSummary e] where
    matchesSummary :: Event -> Bool
    matchesSummary (Event {summary = s}) = case s of 
        Just (Summ evs) -> evs == summ
        Nothing -> False
