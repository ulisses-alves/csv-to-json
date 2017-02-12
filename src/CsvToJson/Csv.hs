module CsvToJson.Csv
( Csv
, CsvHeader
, CsvBody
, CsvItem
, parseCsv
) where

import CsvToJson.Split (split)

type Csv = (CsvHeader, CsvBody)
type CsvHeader = [String]
type CsvBody = [CsvItem]
type CsvItem = [String]

parseCsv :: String -> Csv
parseCsv content = (headers, items)
  where
    (headers:items) = map (split ',') . lines . filter (/='\r') $ content
