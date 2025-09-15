{-
things for representing and parsing csv data, using the grammar from https://datatracker.ietf.org/doc/html/rfc418
-}

module Csv where

import Parsing

type RawField = String
type RawRecord = [RawField]
type RawCsv = [RawRecord]

-- parse the contents of a csv file, lexing included, leaving the fields as text
csvParser :: CParser RawCsv
csvParser = repeatSep recordParser (popToken '\n') <* (popToken '\n' `backup` pure '\n')

-- parse a line/record in a csv file
recordParser :: CParser RawRecord
recordParser = repeatSep fieldParser (popToken ',')

-- parse a field/'cell' in a csv file
fieldParser :: CParser RawField
fieldParser = quoted `backup` unquoted
  where
    -- parse a field surrounded by quotes
    quoted :: CParser RawField
    quoted = popToken '"' >> quoted' <* popToken '"'
    -- parse the contents of a quoted field
    quoted' :: CParser RawField
    quoted' =
      (popSeq "\"\"" >> quoted' >>= \xs -> pure ('"':xs)) `backup`
      (popToken '"' >> pure []) `backup`
      (pop >>= \x -> quoted' >>= \xs -> pure (x:xs))
    -- parse a field without quotes
    unquoted :: CParser RawField
    unquoted = repeatP (filt notSpecial pop)
    -- predicate for characters that do not require special treatment in a csv file
    notSpecial :: Char -> Bool
    notSpecial ',' = False
    notSpecial '\n' = False
    notSpecial '"' = False
    notSpecial _ = True
