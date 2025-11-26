module Features where

import DateTime
import Calendar


-- Exercise 9
countEvents :: Calendar -> Int
countEvents c = length $ events c

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined
