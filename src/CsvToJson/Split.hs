module CsvToJson.Split
( split
) where

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split token target = case break (==token) target of
    (first, [])         -> [first]
    (first, t:second) -> first : split token second
