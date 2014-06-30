module DeBencode where

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.List as List


{- Bencode supports four different types of values:
    integers
    byte strings
    lists
    dictionaries
-}

data BValue = BInt Integer
            | BStr B.ByteString
            | BList [BValue]
            | BDict [M.Map B.ByteString BValue]
