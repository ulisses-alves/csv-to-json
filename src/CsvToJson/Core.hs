module CsvToJson.Core
( parseCsvToJson
) where

import CsvToJson.Csv (Csv, CsvHeader, CsvBody, CsvItem, parseCsv)
import CsvToJson.Substitute (strsub)
import Data.List (intercalate)

parseCsvToJson :: IO ()
parseCsvToJson = interact $ csvToJson . parseCsv

csvToJson :: Csv -> String
csvToJson (headers, rows) = sub [arrayItems] "[%]"
  where
    objects = map (jsonObject headers) rows
    arrayItems = intercalate "," objects

jsonObject :: CsvHeader -> CsvItem -> String
jsonObject headers row = sub [body] "{%}"
  where
    body = intercalate "," . zipWith pair headers $ row
    pair header item = sub [header, item] "\"%\":\"%\""

sub :: [String] -> String -> String
sub = strsub '%'
