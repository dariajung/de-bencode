
module Protocol where

import Network
import Network.HTTP
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as Lz
import Data.List.Split (chunksOf)
import Data.Char (chr, ord)
import qualified Data.List as L
import qualified Network.Socket.ByteString as NB
import qualified Control.Monad as Monad
import Data.Maybe
import Control.Concurrent
import System.IO
import Text.Printf
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

-- move to diff module
main = do
    peerInfo <- getPeerData
    let (ipAddr, portNum) = head peerInfo
    putStrLn $ "Connecting to " ++ (ipAddr) ++ ":" ++ (show $ portNum)
    handle <- Network.connectTo ipAddr (Network.PortNumber $ fromIntegral portNum)
    hSetBuffering handle LineBuffering
    putStrLn $ "Sending handshake to " ++ (ipAddr) ++ ":" ++ (show $ portNum)
    sendHandshake handle
    recvHandshake handle
    recvMessage handle

sendHandshake :: Handle -> IO ()
sendHandshake handle = do
    handshake <- generateHandshake
    C.hPutStr handle handshake

recvHandshake :: Handle -> IO ActivePeer
recvHandshake handle = do
    metaData <- getMetaData
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

-- form initial request URL to tracker
getRequestURL = do
    metaData <- getMetaData
    hash <- getHash 
    {-- need to figure out how to capture state?
        so for time being, hardcoding peer_id
        or just put into a config file? --}
    let urlEncodedHash = addPercents $ toHex hash
        params = Network.HTTP.urlEncodeVars 
                [("peer_id", "-HT0001-560535105852"), 
                ("left", (tLen metaData)), 
                ("port", "6882"),
                ("compact", "1"),
                ("uploaded", "0"),
                ("downloaded", "0"),
                ("event", "started")]
    return $ (announce metaData) ++ "?" ++ "info_hash=" ++ urlEncodedHash ++ "&" ++ params

-- get back response from tracker
-- move to diff module
getRawResponse = do
    url <- getRequestURL
    Network.HTTP.simpleHTTP (Network.HTTP.getRequest url) >>= fmap (take 1000) . Network.HTTP.getResponseBody

-- Instead of dict, perhaps should create tracker response 
-- data type?
-- move to diff module
trackerResponseToDict = do
    response <- getRawResponse
    (BDict dict) <- getBValue "string" response
    let complete = BStr (C.pack "complete")
        incomplete = BStr (C.pack "incomplete")
        intvl = BStr (C.pack "interval")
        prs = BStr (C.pack "peers")
        (BInt seeders) = dict M.! complete
        (BInt leechers) = dict M.! incomplete
        (BInt interval) = dict M.! intvl
        (BStr peers) = dict M.! prs
    return $ M.fromList [("complete", show seeders), 
                        ("incomplete", show leechers), 
                        ("interval", show interval),
                        ("peers", C.unpack peers)]

-- Get peer data from the tracker response
getPeerData :: IO [([Char], Integer)]
getPeerData = do
    dict <- trackerResponseToDict
    return $ parseBinaryModel (dict M.! "peers")

-- parse peer data presented in binary model
parseBinaryModel peerStr = 
    let dList = chunksOf 6 $ map show (B.unpack $ C.pack peerStr)
        digest [] = []
        digest (x:xs) = (L.intercalate "." $ take 4 x, 
            (read (x !! 4) :: Integer) * 256 + 
            (read (x !! 5) :: Integer)) : digest xs
        in (digest dList)

-- REMEMBER THAT MESSAGES ARE BIG ENDIAN

readBEInt :: B.ByteString -> Int
readBEInt x = fromIntegral $ runGet getWord32be $ Lz.fromChunks $ return x

-- Regular int to big endian char 8 Bytestring
writeBEByteStringInt :: Integral a => a -> C.ByteString
writeBEByteStringInt x = B.concat $ Lz.toChunks $ runPut $ putWord32be $ fromIntegral x

writeDecByteInt :: Integral a => a -> C.ByteString
writeDecByteInt x = B.singleton (fromIntegral x)
