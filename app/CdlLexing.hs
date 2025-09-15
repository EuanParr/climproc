{-
a basic NetCDF CDL lexer derived from https://manpages.ubuntu.com/manpages/bionic/man1/ncgen.1.html
and https://github.com/Unidata/netcdf-c/blob/main/ncgen/ncgen.l
-}

module CdlLexing(token, CdlToken (..)) where

import qualified Data.Char

import Parsing

data CdlToken = Identifier String | StringLiteral String | Number Rational | Hex Integer | OpenBracket | CloseBracket | OpenParen | CloseParen | Colon | Semicolon | Equals | Star | Comma | DATA | GROUP | TYPES | DIMENSIONS | VARIABLES deriving (Show, Eq)

-- the lexer - parses a character sequence into a token sequence
token :: CParser [CdlToken]
token =
  (oneWhitespace >> token)
  `backup`
  (popToken '{' >> (:) <$> pure OpenBracket <*> token)
  `backup`
  (popToken '}' >> (:) <$> pure CloseBracket <*> token)
  `backup`
  (popToken '(' >> (:) <$> pure OpenParen <*> token)
  `backup`
  (popToken ')' >> (:) <$> pure CloseParen <*> token)
  `backup`
  (popToken ':' >> (:) <$> pure Colon <*> token)
  `backup`
  (popToken ';' >> (:) <$> pure Semicolon <*> token)
  `backup`
  (popToken '=' >> (:) <$> pure Equals <*> token)
  `backup`
  (popToken '*' >> (:) <$> pure Star <*> token)
  `backup`
  (popToken ',' >> (:) <$> pure Comma <*> token)
  `backup`
  (popToken '"' >> (:) <$> fmap StringLiteral stringLiteral <*> token)
  `backup`
  (popSeq "data:" >> (:) <$> pure DATA <*> token)
  `backup`
  (popSeq "group:" >> (:) <$> pure GROUP <*> token)
  `backup`
  (popSeq "types:" >> (:) <$> pure TYPES <*> token)
  `backup`
  (popSeq "dimensions:" >> (:) <$> pure DIMENSIONS <*> token)
  `backup`
  (popSeq "variables:" >> (:) <$> pure VARIABLES <*> token)
  `backup`
  (eotP >> pure [])
  `backup`
  (popSeq "//" >> lineComment >> token)
  `backup`
  (popSeq "0x" >> (:) <$> fmap Hex hexP <*> token)
  `backup`
  (popSeq "0X" >> (:) <$> fmap Hex hexP <*> token)
  `backup`
  ((:) <$> fmap Number expP <*> token)
  `backup`
  ((:) <$> fmap Identifier identifier <*> token)

stringLiteral :: CParser String
stringLiteral =
  (popToken '"' >> pure [])
  `backup`
  (popToken '\\' >> stringEscapeSequence >>= \x -> stringLiteral >>= \y -> pure (x ++ y))
  `backup`
  ((:) <$> pop <*> stringLiteral)

-- escape sequences are currently left as-is but recognised so escaped quotes do not end strings
stringEscapeSequence :: CParser String
stringEscapeSequence = pop >>= \x -> pure [x]

lineComment :: CParser ()
lineComment = stringBefore (popToken '\n') >> pop >> pure ()

identifier :: CParser String
identifier = ini >>= \x -> repeatP rest >>= \xs -> pure (x : xs)
  where
    ini = filt iniChar pop `backup` (popToken '\\' >> filt iniEsc pop)
    iniChar x = ('A' <= x && x <= 'Z')
                || ('a' <= x && x <= 'z')
                || (x == '_')
                || not (Data.Char.isAscii x)
    iniEsc x = ('0' <= x && x <= '9')
    rest = filt restChar pop `backup` (popToken '\\' >> filt restEsc pop)
    restChar x = iniChar x
                 || ('0' <= x && x <= '9')
                 || elem x ".@+-"
    restEsc x = elem x " !\"#$%&'()*,:;<=>?[\\]`{|}~"


