
module Protocol where

import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Base as Base
import qualified Bencode as Bencode
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

main :: IO ()
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

recvHandshake :: Handle -> IO()
recvHandshake handle = do
                        pStrLen <- B.hGet handle 1
                        pStr <- B.hGet handle (fromIntegral $ (B.unpack pStrLen) !! 0)
                        reserved <- B.hGet handle 8
                        peer_id <- B.hGet handle 20
                        info_hash <- B.hGet handle 20
                        case (C.unpack pStr) == "BitTorrent protocol" of
                            True -> do 
                                    am_choking <- newIORef True
                                    am_interested <- newIORef False
                                    peer_choking <- True
                                    peer_interested <- False


generateHandshake :: IO C.ByteString
generateHandshake = do
                    info_hash <- Bencode.getHash
                    let pstrlen = B.singleton (fromIntegral 19)
                        pstr = C.pack "BitTorrent protocol"
                        reserved = B.replicate 8 (fromIntegral 0)
                        peer_id = C.pack "-HT0001-560535105852"
                        hMsg = B.concat [pstrlen, pstr, reserved, info_hash, peer_id]
                    return hMsg

-- form initial request URL to tracker
getRequestURL = do
                multipleFiles <- Bencode.isMult
                dict <- if multipleFiles then Bencode.parseDataMultiple else Bencode.parseDataSingle
                hash <- Bencode.getHash 
                {-- need to figure out how to capture state
                    peerID <- Bencode.peerID
                    so for time being, hardcoding peer_id
                    or just put into a config file? --}
                let announce = (dict M.! "announce") 
                    left = (dict M.! "length")
                    urlEncodedHash = Bencode.addPercents (Bencode.toHex hash)
                    encoded = Base.urlEncodeVars 
                                [("peer_id", "-HT0001-560535105852"), 
                                ("left", left), 
                                ("port", "6882"),
                                ("compact", "1"),
                                ("uploaded", "0"),
                                ("downloaded", "0"),
                                ("event", "started")]
                return $ announce ++ "?" ++ "info_hash=" ++ urlEncodedHash ++ "&" ++ encoded

-- get back response from tracker
getResponse = do
        url <- getRequestURL
        HTTP.simpleHTTP (HTTP.getRequest url) >>= fmap (take 1000) . HTTP.getResponseBody

trackerResponseToDict = do
                    response <- getResponse
                    (Bencode.BDict dict) <- Bencode.getBValue "string" response
                    let complete = Bencode.BStr (C.pack "complete")
                        incomplete = Bencode.BStr (C.pack "incomplete")
                        intvl = Bencode.BStr (C.pack "interval")
                        prs = Bencode.BStr (C.pack "peers")
                        (Bencode.BInt seeders) = dict M.! complete
                        (Bencode.BInt leechers) = dict M.! incomplete
                        (Bencode.BInt interval) = dict M.! intvl
                        (Bencode.BStr peers) = dict M.! prs
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
