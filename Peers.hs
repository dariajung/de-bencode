import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Base as Base
import qualified Bencode as Bencode
import qualified Data.Map as M
import Data.URLEncoded

getRequestURL = do
                multipleFiles <- Bencode.isMult
                dict <- if multipleFiles then Bencode.parseDataMultiple else Bencode.parseDataSingle
                hash <- Bencode.getHash
                peerID <- Bencode.peerID
                let announce = (dict M.! "announce")
                    left = (dict M.! "length")
                    encoded = Base.urlEncodeVars 
                                [("peer_id", peerID), 
                                ("left", left), 
                                ("port", "6882"),
                                ("compact", "1"),
                                ("uploaded", "0"),
                                ("downloaded", "0"),
                                ("event", "started")]
                return $ announce ++ "?" ++ "info_hash=" ++ hash ++ "&" ++ encoded


main = do
        url <- getRequestURL
        return $ HTTP.simpleHTTP (HTTP.getRequest url)