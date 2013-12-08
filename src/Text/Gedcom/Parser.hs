module Text.Gedcom.Parser
  where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Functor.Foldable (Fix(..))
import Text.Parser.Char hiding (alphaNum, anyChar, space)
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Trifecta.Combinators
import Text.Trifecta.Parser

import Text.Gedcom.Types hiding (xref, tag)

space :: Parser Char
space = char ' '

bsFromSet :: String -> Parser ByteString
bsFromSet = sliced . skipSome . oneOf

alpha :: Parser ByteString
alpha = bsFromSet alphaChars

alphaChars :: String
alphaChars = ['A' .. 'Z'] ++ ['a'..'z'] ++ "_"

digitChars :: String
digitChars = ['0'..'9']

anyChar :: Parser ByteString
anyChar = (sliced . skipSome $ p)
  where
    p = noneOf "\n"

xref :: Parser XRef
xref = XRef <$ char '@' <*> p <* char '@'
  where
    p = sliced (alpha *> natural)

payload :: Parser Payload
payload = optional space *> ((X <$> xref) <|> (B <$> anyChar) <|> pure Null)

record :: Int -> Parser Records
record n = f <$ np <* space <*> alpha <*> payload <* newline <*> rs
  where
    np = nat n
    rs = many . record $ n + 1
    f tag p recs = Fix (TreeF (U (Record tag p)) recs)

zero :: Parser Char
zero = char '0'

nat :: Int -> Parser ()
nat n = () <$ string (show n)

gedcom :: Parser Gedcom
gedcom = Gedcom <$ zero <* space <*> xref <* space <*> alpha <* newline <*> some (record 1)
