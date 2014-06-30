module Bencode where

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.List as L
import qualified Text.Parsec.Prim as Prim
import qualified Text.Parsec.Char as PChar
import qualified Text.Parsec.ByteString as ParseBS
import qualified Text.Parsec.Error as PError
import qualified Text.Parsec.Combinator as Combinator
import qualified Control.Monad as Monad

{- Bencode supports four different types of values:
    integers
    byte strings
    lists
    dictionaries
-}

-- should BStr take a normal String, or a ByteString? 
-- Not sure.

data BValue = BInt Integer
            | BStr B.ByteString
            | BList [BValue]
            | BDict (M.Map BValue BValue)
            deriving (Show, Eq, Ord)

bencode :: BValue -> String
bencode (BInt int) = "i" ++ show int ++ "e"
bencode (BStr str) = show (B.length str) ++ ":" ++ (unpack str)
bencode (BList xs) = "l" ++ L.concatMap bencode xs ++ "e"
bencode (BDict dict) =
    let f k v result = result ++ bencode k ++ ":" ++ bencode v
    in (M.foldrWithKey f "d" dict) ++ "e"

-- take a BValue and return a ByteString representation of it
strToBS :: BValue -> B.ByteString
strToBS = pack . bencode

-- parse a Bencoded Integer
parseInt :: ParseBS.Parser BValue
parseInt = do 
            PChar.char 'i'
            negative <- Combinator.option ' ' (PChar.char '-')
            digs <- Combinator.many1 PChar.digit
            PChar.char 'e'
            make negative digs

            where 
                make ' ' ['0'] = return (BInt 0)
                make '-' ['0'] = Monad.fail "A zero cannot be negative."
                make _ ('0':xs) = Monad.fail "A number cannot have a leading zero."
                make '-' xs = return (BInt (read ('-':xs) :: Integer))
                make _ xs = return (BInt (read xs :: Integer))
