module Text.Gedcom.Parser
  (gedcom)
  where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 
  ( Parser
  , char
  , isDigit
  , string
  )
import qualified Data.Attoparsec.ByteString.Char8 as AT
import Data.Attoparsec.Combinator
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.CharSet (CharSet)
import qualified Data.CharSet as CS
import Data.Functor.Foldable (Fix(..))
import Text.Gedcom.Types hiding (xref, tag)

eol :: Parser ()
eol = AT.endOfLine

mkPred :: String -> (Char -> Bool)
mkPred xs = flip CS.member cs
  where
    cs = CS.fromList xs

mkPredInv :: String -> (Char -> Bool)
mkPredInv xs = flip CS.notMember cs
  where
    cs = CS.fromList xs

space :: Parser Char
space = char ' '

isAlpha = mkPred alphaChars

alpha :: Parser ByteString
alpha = AT.takeWhile isAlpha

alphaChars :: String
alphaChars = ['A' .. 'Z'] ++ ['a'..'z'] ++ "_"

anyChar :: Parser ByteString
anyChar = AT.takeWhile pred
  where
    pred = mkPredInv "\n"

data XRefState = Di | Ch

xref :: Parser XRef
xref = XRef <$ char '@' <*> AT.scan Ch g <* char '@'
  where
    g Ch x | isAlpha x = Just Ch
           | isDigit x = Just Di
           | otherwise = Nothing
    g Di x = if isDigit x then Just Di else Nothing

payload :: Parser Payload
payload = option Null (space *> ((X <$> xref) <|> (B <$> anyChar)))

record :: Int -> Parser Records
record n = f <$ np <* space <*> alpha <*> payload <* eol <*> rs
  where
    np = nat n
    rs = many . record $ n + 1
    f tag p recs = Fix (TreeF (U (Record tag p)) recs)

zero :: Parser Char
zero = char '0'

nat :: Int -> Parser ()
nat n = () <$ string (C8.pack $ show n)

gedcom :: Parser Gedcom
gedcom = Gedcom <$ zero <* space <*> xref <* space <*> alpha <* eol <*> some (record 1)
