{-# LANGUAGE NoMonomorphismRestriction
  , OverloadedStrings #-}

module Text.Gedcom
  where

import Control.Applicative
import Control.Lens.Tuple (_1, _2)
import Control.Lens.Zoom (zoom)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.CharSet as CS
import Data.CharSet (CharSet)
import Data.List (intercalate)
import Data.Monoid ((<>), mempty)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Text (Text)
import Text.Parser.Char hiding (alphaNum, anyChar)
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token
import Text.Trifecta.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result
import Prelude hiding (tail)

import Text.Gedcom.Zipper
import Text.Gedcom.Tree
import Text.Gedcom.Types

settext :: String -> Parser ByteString
settext = token . sliced . skipSome . oneOf

otherChar :: Parser ByteString
otherChar = settext otherChars

alphaNum :: Parser ByteString
alphaNum = settext alphaNumChars

alpha :: Parser ByteString
alpha = settext alphaChars

otherChars :: String 
otherChars = map chr $ [33,34] ++ [36..47] ++ [58..63] ++ [91..94] ++ [96] ++ [123..126]

alphaChars :: String
alphaChars = ['A' .. 'Z'] ++ ['a'..'z'] ++ "_"

digitChars :: String
digitChars = ['0'..'9']

alphaNumChars :: String
alphaNumChars = digitChars ++ alphaChars

nonAtChars :: String
nonAtChars = alphaNumChars ++ otherChars ++ " #"

anyChar :: Parser ByteString
anyChar = (sliced . skipSome $ (na <|> at)) <* spaces
  where
    na = oneOfSet . CS.fromList $ nonAtChars
    at = char '@'

-- | parse a natural, check the allowable range, then chose the appropriate 
--   focus selecting zipper function
level :: StateT Int Parser (Record -> Zipper Record -> Maybe (Zipper Record))
level = do 
    l <- fromInteger <$> lookAhead natural 
    c <- get
    let cond = l <= c + 1 && l /= 0
    unless cond (lift . raiseErr . err $ c) 
    natural
    put l
    let f = case () of
               _ | l == c     -> graftLeft
               _ | l == c + 1 -> graftRight
               _ | True       -> (\r -> graftLeft r <=< rewind (c - l))
    return f
  where
    err = Err Nothing [] . S.singleton . expected
    expected x = "level not equal to 0 and less than " ++ show (x + 1)

xref :: Parser XRef
xref = token (XRef <$ char '@' <*> alpha <*> natural <* char '@')

payload :: Parser Payload
payload = (X <$> xref) <|> (B <$> anyChar)

record :: StateT (Int, Zipper Record) Parser ()
record = do 
    graft <- zoom _1 level
    record <- lift $ Record <$> alpha <*> payload
    z <- zoom _2 get
    maybe (lift . raiseErr $ e) (zoom _2 . put) $ graft record z
  where
    e = Err Nothing [] (S.singleton "Failed to graft record")

tree :: Parser (Tree Record)
tree = (goTop . snd) <$> execStateT (skipSome record) (1, z)
  where z = Z leaf []

zero :: Parser Char
zero = token $ char '0'

gedcom :: Parser Gedcom
gedcom = Gedcom <$ zero <*> xref <*> alpha <*> tree

parseX p = parseString p mempty
parseF = parseFromFile

