-- You can use this file to test your functions: `cabal run` executes main.
-- For example, if main is set to mainDateTime or mainCalendar:
-- echo "19970610T172345Z" | cabal run
-- cat examples/bastille.ics | cabal run
-- Feel free to use ghci instead, or to change functions here to test whatever you want.
-- We'll ignore anything in this file when grading!


module Main where

import DateTime
import Calendar
import Features
import System.Environment
import System.IO


data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

prettyShow :: Result -> String
prettyShow SyntaxError = "date/time with wrong syntax"
prettyShow (Invalid _) = "good syntax, but invalid date or time values"
prettyShow (Valid x)   = "valid date: " ++ printDateTime x

main :: IO ()
main = do
  disableNewlineTranslations
  mainDateTime  -- Invert comments to test other parser
  -- mainCalendar

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
  where
    processInput = map (run parseDateTime) . lines
    processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
    printOutput  = unlines . map prettyShow

mainCalendar :: IO ()
mainCalendar = interact (show . parseCalendar')

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
  string <- readFileWindows path
  return $ parseCalendar' string

-- These three functions fight Windows newline translations:
-- without them, on Windows machines, "\r\n" will be read as "\n"
-- and "\n" will be written as "\r\n".
-- Test using these functions rather than "readFile", and parse  newlines as "\r\n",
-- to make sure your parser works on all operating systems (i.e. also for your grader)!
disableNewlineTranslations :: IO ()
disableNewlineTranslations = do
  hSetNewlineMode stdin  noNewlineTranslation
  hSetNewlineMode stdout noNewlineTranslation
readFileWindows :: FilePath -> IO String
readFileWindows p = withBinaryFile p ReadMode hGetContents
writeFileWindows :: FilePath -> String -> IO ()
writeFileWindows p s = withBinaryFile p WriteMode (`hPutStr` s)
