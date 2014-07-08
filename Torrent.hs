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
import Initiator

data Torrent = Torrent {
    metadata :: Metadata, -- the metadata for a torrent
    inactivePeers :: IORef [InactivePeer], -- the list of inactive peers
    activePeers :: IORef [ActivePeer], -- the list of active peers
    pieces ::  IOArray Int Piece, -- the pieces for this torrent
    initiator :: Initiator
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
    _initiator <- defaultInitiator
    return Torrent {
        metadata = metainfo,
        inactivePeers = inactv,
        activePeers = actv,
        pieces = pieceArr,
        initiator = _initiator
    }

-- start everything off
start = do
    torrent <- generateTorrent
    peers <- genInactivePeers
    sequence $ map (forkIO . initiateHandshake torrent) peers -- fork peers
    return ()

initiateHandshake torrent peer = do
    putStrLn $ "Connecting to " ++ (Peer.ip peer) ++ ":" ++ (show $ Peer.port peer)
    handle <- Network.connectTo (Peer.ip peer) (Network.PortNumber $ fromIntegral $ Peer.port peer)
    hSetBuffering handle LineBuffering
    putStrLn $ "Sending handshake to " ++ (Peer.ip peer) ++ ":" ++ (show $ Peer.port peer)
    sendHandshake handle
    activePeer <- recvHandshake (metadata torrent) handle
    addActivePeer activePeer (activePeers torrent)
    loopRecvMsg torrent activePeer

loopRecvMsg torrent peer = forever $ do
    (msg, payload) <- recvMessage (pHandle peer)
    print msg

-- parseMessage which gives (Msg, [C.ByteString])
--processMessage (msgType, payload) =
--    case msgType of
--        -- return nothing on receiving Keep Alive
--        MsgKeepAlive -> return ()
--        -- Peer is choking, update peer data
--        MsgChoke -> 
