module Metadata where

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M
import qualified Data.List as L
import Bencode
import qualified Config as Config

data Metadata = Metadata {
    announce :: String, -- announce URL
    tLen :: String, -- Torrent length
    pieceCount :: String -- total number of pieces
    --info :: BValue

} deriving (Show)

getBInts :: [(BValue, a)] -> [a]
getBInts [] = []
getBInts ((k, v):xs) = if k == (BStr $ pack "length") then v : getBInts xs else getBInts xs

sumBInts :: [BValue] -> Integer
sumBInts ((BInt x):xs) = x + sumBInts xs
sumBInts [] = 0

-- checks if torrent is for multiple files or a single file
isMult :: IO Bool
isMult = do 
            (BDict dict) <- getBValue "file" Config.torrent
            let info = BStr (pack "info")
                f = BStr (pack "files")
                (BDict infoDict) = dict M.! info
            return $ M.member f infoDict  

-- parse torrents with multiple files
parseDataMultiple :: IO Metadata
parseDataMultiple = do
    (BDict dict) <- (getBValue "file" "torrents/karl_marx.torrent")
    let announce = BStr (pack "announce")
        info = BStr (pack "info")
        f = BStr (pack "files")
        pLen = BStr (pack "piece length")
        (BStr announceUrl) = dict M.! announce
        (BDict infoDict) = dict M.! info
        (BList files) = infoDict M.! f
        (BInt pieceLen) = infoDict M.! pLen
        flattened = concat $ map (\(BDict x) -> M.toList x) files
        totalLength = sumBInts $ getBInts flattened
    return Metadata {
        announce = unpack announceUrl,
        tLen = show totalLength,
        pieceCount = show (ceiling $ (fromIntegral totalLength) / (fromIntegral pieceLen))
    }

-- parse torrents with single files
parseDataSingle :: IO Metadata
parseDataSingle = do
    (BDict dict) <- (getBValue "file" Config.torrent)
    let announce = BStr (pack "announce")
        info = BStr (pack "info")
        len = BStr (pack "length")
        pLen = BStr (pack "piece length")
        (BStr announceUrl) = dict M.! announce 
        (BDict infoDict) = dict M.! info
        (BInt _length) = infoDict M.! len
        (BInt pieceLen) = infoDict M.! pLen
    return Metadata {
        announce = unpack announceUrl,
        tLen = show _length,
        pieceCount = show $ ceiling ((fromIntegral _length) / (fromIntegral pieceLen))
    }  