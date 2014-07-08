
module Protocol where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as Lz
import qualified Data.List as L
import Control.Concurrent
import System.IO
import Data.IORef
import Data.Array.IO
import Data.Binary.Get
import Data.Binary.Put

import qualified Config as Config
import Metadata
import Bencode
import Peer

data Msg = MsgKeepAlive | MsgChoke | MsgUnchoke | MsgInterested | MsgNotInterested | MsgHave | MsgBitfield | MsgRequest 
                        | MsgPiece | MsgCancel | MsgPort deriving (Show, Enum)

sendHandshake :: Handle -> IO ()
sendHandshake handle = do
    handshake <- generateHandshake
    C.hPutStr handle handshake

recvHandshake :: MetaData -> Handle -> IO ActivePeer
recvHandshake torrent handle = do
    metaData <- metadata torrent
    pStrLen <- B.hGet handle 1
    pStr <- B.hGet handle (fromIntegral $ (B.unpack pStrLen) !! 0)
    reserved <- B.hGet handle 8
    peer_id <- B.hGet handle 20
    info_hash <- B.hGet handle 20
    case (C.unpack pStr) == "BitTorrent protocol" of
        True -> do 
                amChoking <- newIORef True
                amInterested <- newIORef False
                peerChoking <- newIORef True
                peerInterested <- newIORef False
                bitField <- newArray (0, (read (pieceCount metaData) :: Int) - 1) False 
                wanted <- newIORef (-1)
                return ActivePeer {
                    pID = peer_id,
                    pHandle = handle,
                    pAmChoking = amChoking,
                    pAmInterested = amInterested,
                    pChoking = peerChoking,
                    pInterested = peerInterested,
                    pBitField = bitField,
                    pWanted = wanted
                }
        False -> error ("Peer is not using the BitTorrent protocol. Exiting.")

recvMessage :: Handle -> IO (Msg, [C.ByteString])
recvMessage handle = do
    numBytes <- B.hGet handle 4
    if (B.length numBytes) < 4
        then do
            threadDelay 1000000
            recvMessage handle
        else do 
            let size = readBEInt numBytes
            case size of
                0 -> return (MsgKeepAlive, [B.empty])
                otherwise -> do
                            msgType <- B.hGet handle 1
                            body <- B.hGet handle size
                            let parsedMsg = parseMessage (fromEnum $ L.sum $ B.unpack msgType) body
                            return parsedMsg

parseMessage :: (Eq a, Num a, Show a) => a -> C.ByteString -> (Msg, [C.ByteString])
parseMessage msgType payload = do
    case msgType of
        0 -> (MsgChoke, [B.empty])
        1 -> (MsgUnchoke, [B.empty])
        2 -> (MsgInterested, [B.empty])
        3 -> (MsgNotInterested, [B.empty])
        4 -> (MsgHave, [payload])
        5 -> (MsgBitfield, [payload])
        6 -> (MsgRequest, [payload])
        7 -> (MsgPiece, [payload])
        8 -> (MsgCancel, [payload])
        otherwise -> error ("Can't read received message. Unknown message id: " ++ (show msgType) ++ ".")      

-- send message to peer
sendMessage :: Handle -> Msg -> [B.ByteString] -> IO ()
sendMessage handle header payload = do  
    putStrLn $ "Sending " ++ (show header) ++ "to peer."
    case header of
        MsgKeepAlive -> B.hPut handle $ writeBEByteStringInt 0
        MsgChoke -> B.hPut handle $ B.concat [writeBEByteStringInt 1, writeDecByteInt 0]
        MsgUnchoke -> B.hPut handle $ B.concat [writeBEByteStringInt 1, writeDecByteInt 1]
        MsgInterested -> B.hPut handle $ B.concat [writeBEByteStringInt 1, writeDecByteInt 2]
        MsgNotInterested -> B.hPut handle $ B.concat [writeBEByteStringInt 1, writeDecByteInt 3]
        MsgHave -> B.hPut handle $ B.append (B.concat [writeBEByteStringInt 5, writeDecByteInt 4]) (B.concat payload)
        -- not sending bitfield
        MsgRequest -> B.hPut handle $ B.append (B.concat [writeBEByteStringInt 13, writeDecByteInt 6]) (B.concat payload)
        -- send piece
        -- not sending cancel
        -- not sending port

generateHandshake :: IO C.ByteString
generateHandshake = do
    info_hash <- getHash
    let pstrlen = B.singleton (fromIntegral 19)
        pstr = C.pack "BitTorrent protocol"
        reserved = B.replicate 8 (fromIntegral 0)
        peer_id = C.pack "-HT0001-560535105852"
        hMsg = B.concat [pstrlen, pstr, reserved, info_hash, peer_id]
    return hMsg

getMetaData :: IO Metadata
getMetaData = do
    multipleFiles <- isMult
    metaData <- if multipleFiles then parseDataMultiple else parseDataSingle
    return metaData

-- REMEMBER THAT MESSAGES ARE BIG ENDIAN

-- read a big endian bytestring representation of int
readBEInt :: B.ByteString -> Int
readBEInt x = fromIntegral $ runGet getWord32be $ Lz.fromChunks $ return x

-- Regular int to big endian char 8 Bytestring
writeBEByteStringInt :: Integral a => a -> C.ByteString
writeBEByteStringInt x = B.concat $ Lz.toChunks $ runPut $ putWord32be $ fromIntegral x

writeDecByteInt :: Integral a => a -> C.ByteString
writeDecByteInt x = B.singleton (fromIntegral x)
