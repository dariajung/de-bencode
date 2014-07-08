module Piece where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Array.IO
import Data.IORef
import qualified Data.Map as Map

data Piece = Piece {
    pDone :: IORef Bool, -- is this piece completely done downloading
    pIndex :: Int, -- the index of the piece
    pSize :: Int, -- the size of the piece
    pBlocks :: IOArray Int B.ByteString, -- a way of organizing 16kb blocks
    pHash :: B.ByteString, -- the SHA1 hash of an individual piece
    pBitfield :: IOUArray Int Bool -- a mini bitfield for a piece
}

-- Use 16384 for block size

-- initialize a piece
genPiece :: Int -> C.ByteString -> Int -> IO Piece
genPiece size hash index = do
    complete <- newIORef False
    blocks <- genEmptyBlockArray (size `div` 16384)
    bitfield <- newArray (0, (size `div` 16384) - 1) False
    return Piece {
        pDone = complete,
        pIndex = index,
        pSize = size,
        pBlocks = blocks,
        pHash = hash,
        pBitfield = bitfield
    }

genEmptyBlockArray :: Int -> IO (IOArray Int B.ByteString)
genEmptyBlockArray blocks = newArray (0, blocks - 1) B.empty