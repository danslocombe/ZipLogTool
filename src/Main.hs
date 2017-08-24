module Main where

import Data.List
import Data.Maybe
import Data.List.Split
import Codec.Binary.Base64.String
import System.Process
import Control.Monad.Par
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as C8ByteString
import qualified Codec.Compression.GZip as GZip

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

newtype LogLine = LogLine [String] deriving Show
newtype Base64 = Base64 String
-- newtype GZip = GZip String

fromBase64 :: Base64 -> String
fromBase64 (Base64 x) = x

fromLogLine :: LogLine -> [String]
fromLogLine (LogLine x) = x

stdFile :: FilePath
stdFile = "C:\\Users\\t-dasloc\\AutoSuggestProcessorInstrumentationLog_16.log"

parseFile :: FilePath -> IO [LogLine]
parseFile path = do
  fileRaw <- readFile path
  return $ LogLine . (splitOn ",") <$> (splitOn "\n" fileRaw)

unzipField :: LogLine -> Int -> IO String
unzipField (LogLine line) i = callCSharp $ Base64 $ line !! i

unzipFieldPure :: LogLine -> Int -> String
unzipFieldPure (LogLine line) i = readZip $ line !! i

decodeField :: LogLine -> Int -> String
decodeField (LogLine line) i = decode $ line !! i

tmpFilePath :: FilePath
tmpFilePath = "C:/Users/t-dasloc/Documents/Base64UnZip/bin/Debug/hasktmp.log"

csPath :: FilePath
csPath = "C:/Users/t-dasloc/Documents/Base64UnZip/bin/Debug/Base64UnZip.exe"

callCSharp :: Base64 -> IO String
callCSharp = readProcess csPath [] . fromBase64

readZip :: String -> String
readZip = C8ByteString.unpack . GZip.decompress . C8ByteString.pack . decode

test :: Int -> IO ()
test i = do
  x <- parseFile stdFile
  let ll@(LogLine y) = head x
  if i >= length y
  then do
    putStrLn $ y !! i
    z <- unzipField ll i
    putStrLn z
  else return ()

x = parseFile stdFile

findRev :: IO ()
findRev = do
  xs <- parseFile stdFile
  let field = 13
  zs <- mapM_ (\ll@(LogLine s) -> do
    z <- unzipField ll field
    if isInfixOf "RevIndex" z
      then do
        putStrLn z
      else
        return ()
    ) xs
  putStrLn "Done!"

makePretty :: String -> String
makePretty s = fromMaybe "" (C8ByteString.unpack <$> json)
  where
    json = Aeson.encodePretty <$> (Aeson.decode $ C8ByteString.pack s :: Maybe Aeson.Object)

findLinesX :: Int -> String -> IO [(Int, String)]
findLinesX field search = findLines <$> x <*> return field <*> return search

findLinesXPrintPretty :: Int -> String -> IO ()
findLinesXPrintPretty field search = join $ (liftM2 forM_) lines $ return (\line -> putStrLn ((makePretty . snd) line))
  where lines = findLinesX field search

findLines :: [LogLine] -> Int -> String -> [(Int, String)]
findLines lines field search =
  catMaybes $ map (\(ll@(LogLine y), i) ->
    if i < length y
    then let
      u = unzipFieldPure ll field
      in if isInfixOf search u
         then Just (i, u)
         else Nothing
    else Nothing
  ) (zip lines [1..])

splitN :: Int -> [a] -> [[a]]
splitN n xs = map (\j -> 
  [x| (x, i) <- zip xs [1..],
    i >= (j-1) * k &&
    i < j * k
  ]) [1..n]
  where
    k = 1 + ((length xs) `div` n)

threads = 4

findLinesPar :: [LogLine] -> Int -> String -> [String]
findLinesPar lines field search = map snd $ concat $ runPar $ do
  let threadLines = splitN threads lines
  parMap (\xs -> findLines xs field search) threadLines

main :: IO ()
main = replicateM_ 500 $ do 
  x' <- x
  let fields = snd <$> findLines x' 13 "Rev"
  seq fields (putStrLn "Done!")
