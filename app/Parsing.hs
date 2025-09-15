{-
a system for combinator-based deterministic parsing with failure traces, and some parsing combinators
-}

module Parsing where

import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Char
import qualified Data.Text
import qualified Data.Text.Encoding
import Data.Word(Word8)
import qualified System.IO

-- stacks used for failure traces
newtype Context = Context {context :: [String]} deriving Show

-- a modification of Wadler's parser monad, takes a context and a sequence of input tokens and returns either a failure stack including the context or a parsed value and the input sequence less the consumed prefix
data Parser a b = Parser {parser :: (Context -> [a] -> Either Context (b, [a]))}

instance Functor (Parser a) where
  fmap = Control.Monad.liftM

instance Applicative (Parser a) where
  pure x = Parser (\_ -> \ts -> Right (x, ts))
  (<*>) = Control.Monad.ap

instance Monad (Parser a) where
  return = pure
  (Parser xm) >>= f =
    Parser (\ctx -> \ts -> do
               (x, ts') <- xm ctx ts
               parser (f x) ctx ts')

-- parsers from strings and blobs, useful for lexing
type CParser a = Parser Char a
type BParser a = Parser Word8 a

-- raise a parsing failure as an error
raiseParsingError :: Context -> b
raiseParsingError (Context ctx) = error $ showErr ctx
  where
    showErr [] = "Parsing error, no details available."
    showErr (x:xs) = "Parsing error: " ++ x ++ showCtx xs
    showCtx [] = []
    showCtx (x:xs) = "\n  in: " ++ x ++ showCtx xs

-- apply a parser, error if it fails
parse :: Parser a b -> [a] -> b
parse xm txt = case parser xm (Context []) txt of
                 Right (v, _) -> v
                 Left ctx -> raiseParsingError ctx

-- load a UTF-8 file and parse the resulting character sequence
parseFileUtf8 :: CParser a -> String -> IO a
parseFileUtf8 psr fileName = do
  fileContents <- System.IO.readFile fileName
  pure $ parse psr fileContents

-- load a UTF-16 little-endian file
readFileUtf16Le :: String -> IO String
readFileUtf16Le fileName = fmap f $ Data.ByteString.readFile fileName
  where
    f = drop 1 . Data.Text.unpack . Data.Text.Encoding.decodeUtf16LE

-- load a UTF-16 little-endian file and parse the resulting character sequence
parseFileUtf16Le :: CParser a -> String -> IO a
parseFileUtf16Le psr fileName = do
  fileContents <- readFileUtf16Le fileName
  pure $ parse psr fileContents

-- prepare a parsing failure result
failRaw :: Context -> String -> Either Context (b, [a])
failRaw ctx err = Left $ Context (err : context ctx)

---- elementary parser combinators follow

-- fail to parse with the given error description
noParse :: String -> Parser a b
noParse err = Parser (\ctx -> \_ -> failRaw ctx err)

-- push a context layer
pushCtx :: String -> Parser a b -> Parser a b
pushCtx ctxLayer (Parser mx) = Parser f
  where
    f ctx xs = mx (Context (ctxLayer : context ctx)) xs

-- recognise the end of the token sequence
eotP :: Show a => Parser a ()
eotP = Parser f
  where
    f _ [] = Right ((), [])
    f ctx ts = failRaw ctx ("[eotP] Expected End of Source, got: " ++ show (take 100 ts) ++ "...")

-- apply xm, if that fails, apply ym to the original sequence
backup :: Parser a b -> Parser a b -> Parser a b
backup (Parser xm) (Parser ym) = Parser f
  where
    f ctx ts = case xm ctx ts of
             Right (v, ts') -> Right (v, ts')
             Left _ -> ym ctx ts

-- make a parser preserve the input sequence
nonPopping :: Parser a b -> Parser a b
nonPopping (Parser xm) = Parser f
  where
    f ctx ts = case xm ctx ts of
             Right (v, _) -> Right (v, ts)
             Left e -> Left e

-- consume and return a token
pop :: Parser a a
pop = Parser f
  where
    f _ (t:ts) = Right (t, ts)
    f ctx [] = failRaw ctx "[pop] Unexpected End of Source"

-- consume and return the entire token sequence
takeAll :: Parser a [a]
takeAll = Parser (\_ -> \ts -> Right (ts, []))

-- consume and ignore the given number of tokens
dropP :: Integer -> Parser a ()
dropP m = Parser $ f m
  where
    f 0 _ ts = Right ((), ts)
    f n ctx (_:ts) = f (n-1) ctx ts
    f _ ctx [] = failRaw ctx $ "[dropP" ++ show m ++ "] Unexpected End of Source"

---- parser combinators made from composing the elementary ones follow

-- consume and return the given number of tokens
takeP :: Integer -> Parser a [a]
takeP 0 = return []
takeP n = pop >>= \t -> takeP (n-1) >>= \ts -> return (t:ts)

-- fail if the result of the given parser does not meet satisfy the given predicate
filt :: (Show b) => (b -> Bool) -> Parser a b -> Parser a b
filt p xm = xm >>= \v -> if p v then return v else noParse ("[filt] Condition failed on " ++ show v)

-- consume and return a specific token
popToken :: (Eq a, Show a) => a -> Parser a a
popToken t = pushCtx ("[popToken] Matching " ++ show t) $ filt (t ==) pop

-- consume and return a specific sequence of tokens
popSeq :: (Eq a, Show a) => [a] -> Parser a [a]
popSeq (x:xs) = popToken x >>= \y -> popSeq xs >>= \ys -> return (y:ys)
popSeq [] = return []

-- apply the given parser repeatedly until it fails and return a list of the results
repeatP :: Parser a b -> Parser a [b]
repeatP xm = (xm >>= \y -> repeatP xm >>= \ys -> return (y:ys)) `backup` return []

-- like repeatP but fails unless the given parser succeeds at least once
repeatPos :: Parser a b -> Parser a [b]
repeatPos xm = xm >>= \y -> repeatP xm >>= \ys -> return (y:ys)

-- like repeatP but requires separators (specified by another parser) between the parts consumed by the main given parser, useful for parsing lists
repeatSep :: Parser a b -> Parser a c -> Parser a [b]
repeatSep xm sep = (xm >>= \x -> repeatP (sep >> xm) >>= \xs -> return (x:xs))
                   `backup`
                   return []

-- like repeatSep but fails unless the main given parser succeeds at least once
repeatPosSep :: Parser a b -> Parser a c -> Parser a [b]
repeatPosSep xm sep = (xm >>= \x -> repeatP (sep >> xm) >>= \xs -> return (x:xs))

-- consume a decimal digit and return it as an integer
decDigit :: CParser Integer
decDigit = pop >>= \d -> case d of
  '0' -> return 0
  '1' -> return 1
  '2' -> return 2
  '3' -> return 3
  '4' -> return 4
  '5' -> return 5
  '6' -> return 6
  '7' -> return 7
  '8' -> return 8
  '9' -> return 9
  _ -> noParse "[decDigit] Expected decimal digit"

-- not a parser, combines numbers as single decimal digits of an overall number
decNat :: [Integer] -> Integer
decNat = f 0
  where
    f n [] = n
    f n (x:xs) = f (10 * n + x) xs

-- parse a natural number in decimal form
natP :: CParser Integer
natP = decDigit >>= f
  where
    f n = (decDigit >>= \d -> f (10 * n + d)) `backup` return n

-- consume a sequence of digits and return them as a fractional part of a (decimal) number
decFractional :: [Integer] -> Rational
decFractional [] = 0
decFractional (x:xs) = (fromInteger x + decFractional xs) / 10

-- parse an integer
intP :: CParser Integer
intP = (popToken '-' >> natP >>= \n -> return (- n)) `backup` natP

-- parse a rational number given in decimal form
ratP :: CParser Rational
ratP = (popToken '-' >> p >>= \n -> return (- n)) `backup` p
  where
    p = ((natP `backup` pure 0) >>= \whole -> popToken '.' >> repeatP decDigit >>= \fractional -> return (fromInteger whole + decFractional fractional)) `backup` fmap fromInteger natP

-- parse a rational number given in engineering notation
expP :: CParser Rational
expP = ratP >>= \mantissa ->
                  ((popToken 'e' `backup` popToken 'E') >> ((popToken '+' >> natP) `backup` intP) >>= \expo -> return (mantissa * (10 ^^ expo)))
                  `backup`
                  return mantissa

-- consume and return the smallest prefix that must be removed before the given parser succeeds
stringBefore :: Parser a b -> Parser a [a]
stringBefore p = (nonPopping p >> return []) `backup` (pop >>= \x -> stringBefore p >>= \xs -> return (x:xs))

-- consume a hexadecimal digit and return it as a number
hexDigit :: CParser Integer
hexDigit = pop >>= \d -> case d of
  '0' -> return 0
  '1' -> return 1
  '2' -> return 2
  '3' -> return 3
  '4' -> return 4
  '5' -> return 5
  '6' -> return 6
  '7' -> return 7
  '8' -> return 8
  '9' -> return 9
  'a' -> return 10
  'A' -> return 10
  'b' -> return 11
  'B' -> return 11
  'c' -> return 12
  'C' -> return 12
  'd' -> return 13
  'D' -> return 13
  'e' -> return 14
  'E' -> return 14
  'f' -> return 15
  'F' -> return 15
  _ -> noParse "[hexDigit] Expected hexadecimal digit"

-- parse a natural number in hexadecimal form
hexP :: CParser Integer
hexP = hexDigit >>= f
  where
    f n = (hexDigit >>= \d -> f (16 * n + d)) `backup` return n

-- consume a whitespace character
oneWhitespace :: CParser ()
oneWhitespace = filt Data.Char.isSpace pop >> return ()

-- consume whitespace characters until a non-whitespace character is reached
anyWhitespace :: CParser ()
anyWhitespace = repeatP oneWhitespace >> return ()
