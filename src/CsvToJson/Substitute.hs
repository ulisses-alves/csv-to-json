module CsvToJson.Substitute
( strsub
) where

strsub :: Char -> [String] -> String -> String
strsub _ _ [] = []
strsub _ [] text = text
strsub token (a:args) text = case break (==token) text of
    (first, [])         -> first
    (first, (t:second)) -> first ++ a ++ strsub token args second
