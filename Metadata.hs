module Metadata where

import qualified Data.ByteString as B


data Metadata = Metadata {
    announce :: String, -- announce URL
    tLen :: String, -- Torrent length
    pieceCount :: String -- total number of pieces

} deriving (Show)