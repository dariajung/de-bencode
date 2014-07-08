module Torrent where

import Data.IORef
import Data.Array.IO
import Network
import Network.HTTP
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as Lz
import qualified Data.List as L
import qualified Network.Socket.ByteString as NB
import Control.Concurrent
import System.IO
import Text.Printf
import Data.List.Split (chunksOf)
import Control.Monad
import Data.Array.IO
import Data.IORef

import Peer
import Metadata
import Config
import Bencode
import Protocol
import Piece

data Torrent = Torrent {
    metadata :: Metadata, -- the metadata for a torrent
    inactivePeers :: IORef [InactivePeer], -- the list of inactive peers
    activePeers :: IORef [ActivePeer], -- the list of active peers
    pieces ::  IOArray Int Piece -- the pieces for this torrent
}

-- generate torrent data type
generateTorrent :: IO Torrent
generateTorrent = do 
    metainfo <- getMetaData
    lPieces <- mapM (\x -> genPiece (read (pieceLength metainfo) :: Int) 
        (infoPieceHash metainfo x) x) [0..(read (pieceCount metainfo)::Int)]
    inactv <- newIORef []
    actv <- newIORef [] 
    pieceArr <- newListArray (0, (read (pieceCount metainfo) :: Int) - 1) lPieces
    return Torrent {
        metadata = metainfo,
        inactivePeers = inactv,
        activePeers = actv,
        pieces = pieceArr
    }

-- move to diff module
--main torrent = do
--    peerInfo <- getPeerData
--    let (ipAddr, portNum) = head peerInfo
--    putStrLn $ "Connecting to " ++ (ipAddr) ++ ":" ++ (show $ portNum)
--    handle <- Network.connectTo ipAddr (Network.PortNumber $ fromIntegral portNum)
--    hSetBuffering handle LineBuffering
--    putStrLn $ "Sending handshake to " ++ (ipAddr) ++ ":" ++ (show $ portNum)
--    sendHandshake handle
--    activePeer <- recvHandshake handle
--    recvMessage torrent handle

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

-- parseMessage which gives (Msg, [C.ByteString])
--processMessage (msgType, payload) =
--    case msgType of
--        -- return nothing on receiving Keep Alive
--        MsgKeepAlive -> return ()
--        -- Peer is choking, update peer data
--        MsgChoke -> 
