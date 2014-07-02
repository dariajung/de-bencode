import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Base as Base
import qualified Bencode as Bencode
import qualified Data.Map as M
import Data.URLEncoded

-- form initial request URL to tracker
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

-- get back response from tracker
getResponse = do
        url <- getRequestURL
        HTTP.simpleHTTP (HTTP.getRequest url) >>= fmap (take 1000) . HTTP.getResponseBody

response_data = "d8:completei3e10:incompletei1e8:intervali1800e12:min intervali1800e5:peers24:J\212\183\186\SUB\226P^L\ACKH\\Z6m\212\134\163\174\STX\168$w$e\n"

