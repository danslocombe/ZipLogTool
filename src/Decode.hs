module Decode where

import Data.List
import Data.Maybe
import Data.List.Split
import Codec.Binary.Base64.String
import System.Process
import Control.Monad.Par
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as C8ByteString
import qualified Codec.Compression.GZip as GZip
import Control.Exception

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

newtype LogLine = LogLine [String] deriving Show
newtype Base64 = Base64 String
-- newtype GZip = GZip String

fromBase64 :: Base64 -> String
fromBase64 (Base64 x) = x

fromLogLine :: LogLine -> [String]
fromLogLine (LogLine x) = x

parseFile :: FilePath -> IO [LogLine]
parseFile path = do
  fileRaw <- readFile path
  return $ LogLine . (splitOn ",") <$> (splitOn "\n" fileRaw)

unzipFieldPure :: LogLine -> Int -> String
unzipFieldPure (LogLine line) i = readZip $ line !! i

decodeField :: LogLine -> Int -> String
-- decodeField (LogLine line) i = decode $ line !! i
decodeField (LogLine line) i = readZip $ line !! i

readZip :: String -> String
readZip = C8ByteString.unpack . GZip.decompress . C8ByteString.pack . decode

makePretty :: String -> String
makePretty s = fromMaybe "" (C8ByteString.unpack <$> json)
  where
    json = Aeson.encodePretty <$> (Aeson.decode $ C8ByteString.pack s :: Maybe Aeson.Object)


-- findLinesX :: Int -> String -> IO [(Int, String)]
-- findLinesX field search = findLines <$> x <*> return field <*> return search

-- findLinesXPrintPretty :: Int -> String -> IO ()
-- findLinesXPrintPretty field search = join $ (liftM2 forM_) lines $ return (\line -> putStrLn ((makePretty . snd) line))
  -- where lines = findLinesX field search

-- findLinesXFV :: Int -> String -> String -> IO [String]
-- findLinesXFV fieldId field value = (liftM4 findLinesFV) x (return fieldId) (return field) (return value)

-- findLinesXFVPrintPretty :: Int -> String -> String -> IO ()
-- findLinesXFVPrintPretty fieldId field value = join $ (liftM2 forM_) lines $ return (\line -> putStrLn ((makePretty) line))
  -- where lines = findLinesXFV fieldId field value

findLinesN :: [LogLine] -> Int -> [String] -> [(Int, String)]
findLinesN lines field finds = 
  catMaybes $ map (\(ll@(LogLine y), i) ->
    if field < length y
    then let
      u = unzipFieldPure ll field :: String
      in if (all (\x -> x `isInfixOf` u) finds)
         then Just (i, u)
         else Nothing
    else Nothing
  ) (zip lines [0..])

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

showSchema :: [LogLine] -> Int -> IO ()
showSchema lls fieldId = do
  let y@(LogLine z) = head lls
      n = length z
      fields = map (\i -> decodeField y i) [fieldId]
      jsons = catMaybes $ map (\f -> Aeson.decode $ C8ByteString.pack f) fields
      strs = map objStruct jsons
  mapM putStrLn strs
  putStrLn "Done"

objStruct :: Aeson.Object -> String
objStruct x = concatMap 
  (\k -> fromMaybe "" $ f k <$> Map.lookup k x) keys
  where keys = Map.keys x :: [Text.Text]
      
        f k v = let k' = Text.unpack k in
            k' ++ " :: " ++ (showType v) ++ ", "

showType :: Aeson.Value -> String
showType v = case v of
  Aeson.Object o -> "{ " ++ objStruct o ++ " }"
  Aeson.Array xs -> 
    "[ " ++ (fromMaybe "" $ showType <$> Vector.headM xs) ++  " ]"
  Aeson.String s -> "String"
  Aeson.Number n -> "Int"
  Aeson.Bool b -> "Bool"
  Aeson.Null -> "Null"
