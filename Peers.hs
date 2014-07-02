import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Base as Base
import qualified Bencode as Bencode
import qualified Data.Map as M
import Control.Applicative

getRequestURL = do
                multipleFiles <- Bencode.isMult
                dict <- if multipleFiles then Bencode.parseDataMultiple else Bencode.parseDataSingle
                hash <- Bencode.getHash
                peerID <- Bencode.peerID
                let announce = (dict M.! "announce")
                    left = (dict M.! "length")
                    encoded = Base.urlEncodeVars [("info_hash", hash), ("peer_id", peerID), ("left", left)]
                return $ announce ++ "?" ++ encoded


main = do
        url <- getRequestURL
        return $ HTTP.simpleHTTP (HTTP.getRequest url)