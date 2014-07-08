module Peer where

import qualified Data.ByteString as B
import System.IO
import Data.IORef
import Data.Array.IO 

-- Data.Array.IO and Data.IORef because yay mutable state

data ActivePeer = ActivePeer {
    pID :: B.ByteString, -- peer id.
    pHandle :: Handle, -- handle for particular peer.

    pAmChoking :: IORef Bool, -- I am choking peer.
    pAmInterested :: IORef Bool, -- I am interested in peer.
    pChoking :: IORef Bool, -- Peer is choking me.
    pInterested :: IORef Bool, -- Peer is interested in me.

    pBitField :: IOUArray Int Bool, -- array representation of a peer's bitfield
    pWanted :: IORef Int -- index of piece I want from this peer.
}

data InactivePeer = InactivePeer {
    port :: Int,
    ip :: String
}

addActivePeer :: ActivePeer -> IORef [ActivePeer] -> IO ()
addActivePeer peer arr = modifyIORef arr (peer:)

addInactivePeer :: InactivePeer -> IORef [InactivePeer] -> IO ()
addInactivePeer peer arr = modifyIORef arr (peer:)