import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Base as Base
import qualified Bencode as Bencode
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import Data.List.Split (chunksOf)
import qualified Data.List as L
import Network


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

trackerResponseToDict = do
                    response <- getResponse
                    (Bencode.BDict dict) <- Bencode.getBValue "string" response
                    let complete = Bencode.BStr (pack "complete")
                        incomplete = Bencode.BStr (pack "incomplete")
                        intvl = Bencode.BStr (pack "interval")
                        prs = Bencode.BStr (pack "peers")
                        (Bencode.BInt seeders) = dict M.! complete
                        (Bencode.BInt leechers) = dict M.! incomplete
                        (Bencode.BInt interval) = dict M.! intvl
                        (Bencode.BStr peers) = dict M.! prs
                    return $ M.fromList [("complete", show seeders), 
                                        ("incomplete", show leechers), 
                                        ("interval", show interval),
                                        ("peers", unpack peers)]

getPeerData = do
        dict <- trackerResponseToDict
        return $ parseBinaryModel (dict M.! "peers")



-- parse peer data
parseBinaryModel str = 
                    let dList = chunksOf 6 $ map show (B.unpack $ pack str)
                        digest [] = []
                        digest (x:xs) = (L.intercalate "." $ take 4 x, (read (x !! 4) :: Integer) * 256 + (read (x !! 5) :: Integer)) : digest xs
                        in (digest dList)
