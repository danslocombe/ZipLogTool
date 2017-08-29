module Main where

import Decode

import Safe
import Control.Monad
import Data.Maybe
import System.Environment
import Data.List.Split (splitOn)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> shell (parseFile path)
    _ -> putStrLn $ "Usage: " ++ name ++ ".exe InstrumentedLogs_0.log"

name :: String
name = "blagh"

shell :: IO [LogLine] -> IO ()
shell linesIO = do
  lines <- linesIO
  let lineCount = length lines
      lineSize = length $ fromLogLine $ head lines
  putStrLn "> For a list of cammands, use \"help\""
  forever $ do
    putStrLn $ "> " ++ name ++ " loaded with " ++ show lineCount ++ " lines with " ++ show lineSize ++ " fields"
    command <- getLine
    let command' = splitOn " " command
    case command' of
      [""] -> return ()
      nonempty -> parseCommand lines nonempty
    return ()
    

parseCommand :: [LogLine] -> [String] -> IO ()
parseCommand ll [] = return ()
parseCommand ll (c:args) = 
  case c of
    "find" -> doFind ll args
    "show" -> doShow ll args
    "schema"   -> doSchema ll arg1
    _ -> putStrLn $ "Unknown command \"" ++ c ++ "\", use \"help\" to show commands"
    where arg1 = fromMaybe "" $ listToMaybe args

doSchema :: [LogLine] -> String -> IO ()
doSchema _ "" = putStrLn "Usage: schema {fieldId | all}"
doSchema ls "all" = undefined
doSchema ls arg = case readMay arg :: Maybe Int of
  Nothing -> return ()
  Just x  -> showSchema ls x

doShow :: [LogLine] -> [String] -> IO ()
doShow _ []   = putStrLn "Usage: show {fieldId | all} [logid | full]"
doShow _ [""] = putStrLn "Usage: show {fieldId | all} [logid | full]"
doShow ls (lineS:specS) = do
  let lineM = readMay lineS :: Maybe Int
  case lineM of
                 -- Hack, we are searching with the empty list as our query
    Just line -> let results@(result:_) = map snd $ findLinesN ls line []
                 in mapM_ (putStrLn . makePretty) (case specS of
                   []       -> [result]
                   ["full"] -> results
                   (s:_)    -> (case readMay s of
                                 Just n  -> [results !! n]
                                 Nothing -> []))
    Nothing   -> putStrLn $ "Could not parse \"" ++ lineS ++ "\" as a valid line field number"

doFind :: [LogLine] -> [String] -> IO ()
doFind [] _ = putStrLn "Logs are empty!"
doFind _ [] = putStrLn "Usage: find {fieldId} {search1} {search2} ..."
doFind _ [""] = putStrLn "Usage: find {fieldId} {search1} {search2} ..."
doFind logs@(l:ls) (lineS:find:finds) = do
  let lineM = readMay lineS :: Maybe Int
  case lineM of
    Just line -> let result = findLinesN logs line (find:finds)
                 in mapM_ (\(i, r) -> putStrLn $ "Match in log line:" ++ show i ++ "\n" ++ makePretty r) result
    Nothing   -> putStrLn $ "Could not parse \"" ++ lineS ++ "\" as a valid line field number"

doFind _ _ = putStrLn "Invalid args"
           
stdFile :: FilePath
stdFile = "C:\\Users\\t-dasloc\\AutoSuggestProcessorInstrumentationLog_16.log"
