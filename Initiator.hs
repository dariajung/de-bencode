module Initiator where

import Data.IORef
import Data.Array.IO
import Network
import Network.HTTP
import qualified Data.Map as M
import Data.List.Split (chunksOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L

import Bencode
import Metadata
import Peer

data Initiator = Initiator {
    announceURL :: String
}

defaultInitiator :: IO Initiator
defaultInitiator = do 
    metaData <- getMetaData
    hash <- getHash 
    let urlEncodedHash = addPercents $ toHex hash
        params = Network.HTTP.urlEncodeVars 
                [("peer_id", "-HT0001-560535105852"), 
                ("left", (tLen metaData)), 
                ("port", "6882"),
                ("compact", "1"),
                ("uploaded", "0"),
                ("downloaded", "0"),
                ("event", "started")]
    return Initiator {
        announceURL = (announce metaData) ++ "?" ++ "info_hash=" ++ urlEncodedHash ++ "&" ++ params
    }

-- get back response from tracker
getRawResponse :: Initiator -> IO [Char]
getRawResponse initiator = do
    let url = announceURL initiator
    Network.HTTP.simpleHTTP (Network.HTTP.getRequest url) >>= fmap (take 1000) . Network.HTTP.getResponseBody

-- Instead of dict, perhaps should create tracker response 
-- data type?
trackerResponseToDict :: IO (M.Map [Char] String)
trackerResponseToDict = do
    initiator <- defaultInitiator
    response <- getRawResponse initiator
    print response
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

-- generate a list of inactive peers
genInactivePeers :: IO [InactivePeer]
genInactivePeers = do
    peers <- getPeerData
    let create ((ipAddr, portNum):xs) = InactivePeer {Peer.ip = ipAddr, Peer.port = fromIntegral portNum} : create xs
        peerL = create peers
    return peerL

-- Get peer data from the tracker response
getPeerData :: IO [([Char], Integer)]
getPeerData = do
    dict <- trackerResponseToDict
    return $ parseBinaryModel (dict M.! "peers")

-- parse peer data presented in binary model
parseBinaryModel :: [Char] -> [([Char], Integer)]
parseBinaryModel peerStr = 
    let dList = chunksOf 6 $ map show (B.unpack $ C.pack peerStr)
        digest [] = []
        digest (x:xs) = (L.intercalate "." $ take 4 x, 
            (read (x !! 4) :: Integer) * 256 + 
            (read (x !! 5) :: Integer)) : digest xs
        in (digest dList)
