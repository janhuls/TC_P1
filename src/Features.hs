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
checkOverlapping (Calendar {events = evs}) = any pairsOverlap (pairs evs)
  where
    pairs :: [a] -> [(a,a)]
    pairs xs = [(x,y) | (i,x) <- zip [0..] xs, (j,y) <- zip [0..] xs, i < j]

    pairsOverlap :: (Event, Event) -> Bool
    pairsOverlap (e1, e2) =
      let (DTStart s1) = dtStart e1
          (DTEnd   e1e) = dtEnd e1
          (DTStart s2) = dtStart e2
          (DTEnd   e2e) = dtEnd e2
      in overlaps s1 e1e s2 e2e

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
dateToDays (Date (Year yr) (Month mo) (Day da)) =
    yearDays yr + monthDays yr mo + da

timeToMinutes :: Time -> Int
timeToMinutes (Time hr min sec) =
  h * 60 + m + s `div` 60 where
    h = runHour hr
    m = runMinute min
    s = runSecond sec

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
