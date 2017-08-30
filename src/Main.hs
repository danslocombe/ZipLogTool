module Main where

import Decode

import Safe
import Control.Monad
import Data.Maybe
import System.Environment
import Data.List.Split (splitOn)
import System.Console.ANSI
import System.IO
import Data.Either (isLeft, isRight)
import Control.Exception (try)
import Codec.Compression.Zlib.Internal (DecompressError)
import qualified Data.ByteString.Lazy.Char8 as C8ByteString

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> shell (parseFile path)
    _ -> putStrLn $ "Usage: " ++ name ++ ".exe InstrumentedLogs_0.log"

name :: String
name = "ZipLogTool"

shell :: IO [LogLine] -> IO ()
shell linesIO = do
  lines <- linesIO
  valid <- validFields lines
  let lineCount = length lines
      lineSize = length $ fromLogLine $ head lines
  putStrLn $ "> " ++ name ++ " loaded with " ++ show lineCount ++ " lines with " ++ show lineSize ++ " fields"
  putStrLn "> For a list of cammands, use \"help\""
  forever $ do
    prettyBar valid
    putStr ">>> "
    hFlush stdout
    -- This will break on tiny <4 char consoles
    setCursorColumn 4
    command <- getLine
    putStr "\n"
    let command' = splitOn " " command
    case command' of
      [""] -> return ()
      nonempty -> parseCommand valid lines nonempty
    return ()

parseFieldId :: [Bool] -> String -> Maybe Int
parseFieldId valid s = (readMay s :: Maybe Int) >>= \x ->
  if valid !! x
    then Just x
    else Nothing

parseCommand :: [Bool] -> [LogLine] -> [String] -> IO ()
parseCommand _ _ [] = return ()
parseCommand valid ll (c:args) = 
  case c of
    "find" -> doFind valid ll args
    "show" -> doShow valid ll args
    "info" -> undefined
    "raw"  -> doRaw ll args
    "schema"   -> doSchema valid ll arg1
    _ -> putStrLn $ "Unknown command \"" ++ c ++ "\", use \"help\" to show commands"
    where arg1 = fromMaybe "" $ listToMaybe args

doSchema :: [Bool] -> [LogLine] -> String -> IO ()
doSchema _ _ "" = putStrLn "Usage: schema {fieldId | all}"
doSchema valid ls "all" = mapM_ (showSchema ls) 
  [i | (i, x) <- zip [0..] valid, x]
doSchema valid ls arg = case parseFieldId valid arg of
  Nothing -> putStrLn $ "Invalid field id \"" ++ arg ++ "\""
  Just x  -> showSchema ls x

doRaw :: [LogLine] -> [String] -> IO ()
doRaw _ [] = putStrLn "Usage: raw {fieldId | all}"
doRaw _ [""] = putStrLn "Usage: raw {fieldId | all}"
doRaw ls ["all"] = mapM_ (\(i, f) -> putStrLn $ "Field: " ++ show i ++ "\n" ++ f ++ "\n") 
                     (zip [0..] $ fromLogLine $ head ls)

doShow :: [Bool] -> [LogLine] -> [String] -> IO ()
doShow _ _ []   = putStrLn "Usage: show {fieldId | all} [logid | full]"
doShow _ _ [""] = putStrLn "Usage: show {fieldId | all} [logid | full]"
doShow valid ls (lineS:specS) = do
  let lineM = parseFieldId valid lineS
  case lineM of
                 -- Hack, we are searching with the empty list as our query
    Just line -> let results@(result:_) = map snd $ findLinesN ls line []
                 in mapM_ (putStrLn . makePretty) (case specS of
                   []       -> [result]
                   ["full"] -> results
                   (s:_)    -> (case readMay s of
                                 Just n  -> [results !! n]
                                 Nothing -> []))
    Nothing   -> putStrLn $ "Invalid field id \"" ++ lineS ++ "\""

doFind :: [Bool] -> [LogLine] -> [String] -> IO ()
doFind _ [] _ = putStrLn "Logs are empty!"
doFind _ _ [] = putStrLn "Usage: find {fieldId} {search1} {search2} ..."
doFind _ _ [""] = putStrLn "Usage: find {fieldId} {search1} {search2} ..."
doFind valid logs@(l:ls) (lineS:find:finds) = do
  let lineM = parseFieldId valid lineS
  case lineM of
    Just line -> let result = findLinesN logs line (find:finds)
                 in mapM_ (\(i, r) -> putStrLn $ "Match in log line:" ++ show i ++ "\n" ++ makePretty r) result
    Nothing   -> putStrLn $ "Invalid field id \"" ++ lineS ++ "\""

doFind _ _ _ = putStrLn "Invalid args"
           
stdFile :: FilePath
stdFile = "C:\\Users\\t-dasloc\\AutoSuggestProcessorInstrumentationLog_16.log"

validFields :: [LogLine] -> IO [Bool]
validFields ls@(LogLine head:_) = 
  let n = length head
  in mapM (validField ls) [0..n-1]

validField :: [LogLine] -> Int -> IO Bool
validField ls i = do
  or <$> mapM (validGZip . (!! i) . fromLogLine)
     -- Bit hacky, will break on single-field logs
     (filter ((> 1) . length . fromLogLine) ls)

validGZip :: String -> IO Bool
validGZip s = 
  isRight <$> (try 
      (do 
      decoded <- return $ readZip s
      seq decoded $ return decoded) 
      :: IO (Either DecompressError String)) 

prettyBar :: [Bool] -> IO ()
prettyBar bs = do
  putStr "["
  mapM_ (\(b, i) -> do
    if b
      then setSGR [SetColor Foreground Vivid Green]
      else setSGR [SetColor Foreground Vivid Red]
    putStr $ (show i) ++ "--"
    ) $ zip bs [0..]
  setSGR [Reset]
  putStr "]\n"
