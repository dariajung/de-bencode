module Piece where

import qualified Data.ByteString as B
import Data.Array.IO
import Data.IORef
import qualified Crypto.Hash.SHA1 as SHA1

data Piece = Piece {
    pDone :: IORef Bool, -- is this piece completely done downloading
    pIndex :: Int, -- the index of the piece
    pSize :: Int, -- the size of the piece

    pBlocks :: IOArray Int B.ByteString, -- a way of organizing 16kb blocks
    pHash :: B.Bytestring -- the SHA1 hash of an individual piece
}