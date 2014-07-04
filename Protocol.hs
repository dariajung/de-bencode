
module Protocol where

import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Base as Base
import Bencode
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List.Split (chunksOf)
import qualified Data.List as L
import Network.Socket as Socket
import qualified Network.Socket.ByteString as NB
import qualified Network as N
import System.IO
import Text.Printf
import Data.Char (chr, ord)
import Data.IORef
import qualified Peer as Peer
import qualified Control.Monad as Monad
import Metadata
import qualified Config as Config

main = do
        peerInfo <- getPeerData
        let (ipAddr, portNum) = head peerInfo
        putStrLn $ "Connecting to " ++ (ipAddr) ++ ":" ++ (show $ portNum)
        handle <- N.connectTo ipAddr (N.PortNumber $ fromIntegral portNum)
        hSetBuffering handle LineBuffering
        putStrLn $ "Sending handshake to " ++ (ipAddr) ++ ":" ++ (show $ portNum)
        sendHandshake handle
        recvHandshake handle

sendHandshake :: Handle -> IO ()
sendHandshake handle = do
                        handshake <- generateHandshake
                        C.hPutStr handle handshake

recvHandshake :: Handle -> IO Peer.ActivePeer
recvHandshake handle = do
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
                        -- pBitField
                        -- pWanted
                        return Peer.ActivePeer {
                            Peer.pID = peer_id,
                            Peer.pHandle = handle,
                            Peer.pAmChoking = amChoking,
                            Peer.pAmInterested = amInterested,
                            Peer.pChoking = peerChoking,
                            Peer.pInterested = peerInterested
                        }
                False -> error ("Peer is not using the BitTorrent protocol. Exiting.")

generateHandshake :: IO C.ByteString
generateHandshake = do
                    info_hash <- getHash
                    let pstrlen = B.singleton (fromIntegral 19)
                        pstr = C.pack "BitTorrent protocol"
                        reserved = B.replicate 8 (fromIntegral 0)
                        peer_id = C.pack "-HT0001-560535105852"
                        hMsg = B.concat [pstrlen, pstr, reserved, info_hash, peer_id]
                    return hMsg

-- form initial request URL to tracker
getRequestURL = do
                multipleFiles <- isMult
                metaData <- if multipleFiles then parseDataMultiple else parseDataSingle
                hash <- getHash 
                {-- need to figure out how to capture state
                    peerID <- 
                    peerID
                    so for time being, hardcoding peer_id
                    or just put into a config file? --}
                let urlEncodedHash = addPercents $ toHex hash
                    params = Base.urlEncodeVars 
                            [("peer_id", "-HT0001-560535105852"), 
                            ("left", (tLen metaData)), 
                            ("port", "6882"),
                            ("compact", "1"),
                            ("uploaded", "0"),
                            ("downloaded", "0"),
                            ("event", "started")]
                return $ (announce metaData) ++ "?" ++ "info_hash=" ++ urlEncodedHash ++ "&" ++ params

-- get back response from tracker
getResponse = do
        url <- getRequestURL
        HTTP.simpleHTTP (HTTP.getRequest url) >>= fmap (take 1000) . HTTP.getResponseBody

trackerResponseToDict = do
                    response <- getResponse
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

getPeerData :: IO [([Char], Integer)]
getPeerData = do
        dict <- trackerResponseToDict
        return $ parseBinaryModel (dict M.! "peers")

-- parse peer data
parseBinaryModel str = 
                    let dList = chunksOf 6 $ map show (B.unpack $ C.pack str)
                        digest [] = []
                        digest (x:xs) = (L.intercalate "." $ take 4 x, (read (x !! 4) :: Integer) * 256 + (read (x !! 5) :: Integer)) : digest xs
                        in (digest dList)
