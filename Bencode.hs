module Bencode where

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.List as L


{- Bencode supports four different types of values:
    integers
    byte strings
    lists
    dictionaries
-}

data BValue = BInt Integer
            | BStr String
            | BList [BValue]
            | BDict [M.Map B.ByteString BValue]

bencode :: BValue -> String
bencode (BInt int) = "i" ++ show int ++ "e"
bencode (BStr str) = show (L.length str) ++ ":" ++ (str)