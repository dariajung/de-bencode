module Peer where

import qualified Data.ByteString as B
import System.IO
import Data.IORef
import Data.Array.IO


data ActivePeer = ActivePeer {
    pID :: B.ByteString, -- peer id.
    pHandle :: Handle, -- handle for particular peer.

    pAmChoking :: IORef Bool, -- I am choking peer.
    pAmInterested :: IORef Bool, -- I am interested in peer.
    pChoking :: IORef Bool, -- Peer is choking me.
    pInterested :: IORef Bool, -- Peer is interested in me.

    pBitField :: IOUArray Int Bool, -- array representation of a bitfield
    pWanted :: IORef Int -- piece I want from this peer.
}

data InactivePeer = InactivePeer {
    port :: Int,
    ip :: String
}