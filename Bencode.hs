module Bencode where

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M
import qualified Data.List as L


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
            | BDict [M.Map B.ByteString BValue]
            deriving (Show, Eq, Ord)

bencode :: BValue -> String
bencode (BInt int) = "i" ++ show int ++ "e"
bencode (BStr str) = show (B.length str) ++ ":" ++ (unpack str)
bencode (BList xs) = "l" ++ L.concatMap bencode xs ++ "e"