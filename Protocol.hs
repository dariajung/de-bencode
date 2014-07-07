
module Protocol where

import Network
import Network.HTTP
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
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

import qualified Config as Config
import Metadata
import Bencode
import Peer

data Msg = MsgKeepAlive | MsgChoke | MsgUnchoke | MsgInterested | MsgNotInterested | MsgHave | MsgBitfield | MsgRequest 
                        | MsgPiece | MsgCancel | MsgPort deriving (Show, Enum)

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
                        let size = readInt numBytes
                        case size of
                            0 -> return (MsgKeepAlive, [B.empty])
                            otherwise -> do
                                        msgType <- B.hGet handle 1
                                        body <- B.hGet handle size
                                        let parsedMsg = parseMessage (readInt msgType) body
                                        print parsedMsg
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
getRawResponse = do
        url <- getRequestURL
        Network.HTTP.simpleHTTP (Network.HTTP.getRequest url) >>= fmap (take 1000) . Network.HTTP.getResponseBody

-- Instead of dict, perhaps should create tracker response 
-- data type?
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

readInt :: B.ByteString -> Int
readInt x = fromEnum $ L.sum $ B.unpack x
