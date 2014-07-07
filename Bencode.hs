{-# LANGUAGE NoMonomorphismRestriction #-}

module Bencode where

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Text.Parsec.Prim as Prim
import qualified Text.Parsec.Char as PChar
import qualified Text.Parsec.ByteString as ParseBS hiding (parseFromFile)
import qualified Text.Parsec.Error as PError
import qualified Text.Parsec.Combinator as Combinator
import qualified Control.Monad as Monad
import Data.Either.Combinators (fromRight)
import Crypto.Hash.SHA1 (hashlazy, hash)
import qualified Data.ByteString.Lazy as Lazy
import Text.Printf (printf)
import qualified System.Random as Random
import Data.List.Split (chunksOf)
import qualified Config as Config
import Metadata

{- Bencode supports four different types of values:
    integers
    byte strings
    lists
    dictionaries
-}

data BValue = BInt Integer
            | BStr B.ByteString
            | BList [BValue]
            | BDict (M.Map BValue BValue)
            deriving (Show, Eq, Ord)

bencode :: BValue -> String
bencode (BInt int) = "i" ++ show int ++ "e"
bencode (BStr str) = show (B.length str) ++ ":" ++ (unpack str)
bencode (BList xs) = "l" ++ L.concatMap bencode xs ++ "e"
bencode (BDict dict) =
    let list = M.toList dict
        applied = map (\(k, v) -> bencode k ++ bencode v) list
        stringify (x:xs) = x ++ stringify xs
        stringify [] = ""
    in "d" ++ stringify applied ++ "e"

-- take a BValue and return a ByteString representation of it
strToBS :: BValue -> B.ByteString
strToBS = pack . bencode

-- parse to a Bencoded Integer
parseToBInt :: ParseBS.Parser BValue
parseToBInt = do 
            PChar.char 'i'
            negative <- Combinator.option ' ' (PChar.char '-')
            digits <- Combinator.many1 PChar.digit
            PChar.char 'e'
            make negative digits
            where 
                make ' ' ['0'] = return (BInt 0)
                make '-' ['0'] = Monad.fail "A zero cannot be negative."
                make _ ('0':xs) = Monad.fail "A number cannot have a leading zero."
                make '-' xs = return (BInt (read ('-':xs) :: Integer))
                make _ xs = return (BInt (read xs :: Integer))

-- parse to a Bencoded ByteString
parseToBStr :: ParseBS.Parser BValue
parseToBStr = do
            digits <- Combinator.many1 PChar.digit
            PChar.char ':'
            make (read digits :: Int)
            where make len = 
                    do bstring <- Combinator.count len PChar.anyChar
                       return (BStr (pack bstring))

-- parse to a Bencoded Dictionary
parseToBDict :: ParseBS.Parser BValue
parseToBDict = do
                PChar.char 'd'
                contents <- Combinator.many1 kvPair
                PChar.char 'e'
                make contents
                where make contents = return (BDict (M.fromList contents))

-- parse to a Bencoded List
parseToBList :: ParseBS.Parser BValue
parseToBList = do
                PChar.char 'l'
                contents <- Combinator.many1 (parseToBInt Prim.<|> parseToBStr Prim.<|> parseToBDict Prim.<|> parseToBList)
                PChar.char 'e'
                make contents
                where make contents = return (BList (contents))

-- parse to a (BValue, BValue) tuple
kvPair :: ParseBS.Parser (BValue, BValue)
kvPair = do 
        key <- parseToBStr
        value <- parseToBInt Prim.<|> parseToBStr Prim.<|> parseToBDict Prim.<|> parseToBList
        return (key, value)

-- returns IO (Either ParseError BValue) where BValue is BDict
readBencodedFile :: String -> IO (Either PError.ParseError BValue)
readBencodedFile = parseFromFile parseToBDict

readBencodedString str = parseFromString parseToBDict (pack str)

{- getBValue

filetype can be "file" or "string"
"file": path to file
"string": actual string to be read

-}
getBValue fileType source = do
                    bencodeInfo <- if fileType == "string" then readBencodedString source else readBencodedFile source
                    let unwrapped = fromRight (BDict (M.fromList [])) bencodeInfo
                    return unwrapped

-- get the info_hash        
getHash :: IO B.ByteString
getHash = do
            (BDict dict) <- getBValue "file" Config.torrent
            let info = BStr (pack "info")
                a@(BDict infoDict) = dict M.! info
                bencoded = strToBS a
                hashed = hash bencoded
            return $ hashed

toHex :: B.ByteString -> String
toHex bytes = unpack bytes >>= printf "%02x"

addPercents :: String -> String
addPercents str = "%" ++ (init $ concat $ map (\x -> x ++ "%") (chunksOf 2 str))

-- following the azeureus style for generating a peer_id
peerID :: IO String
peerID = do
        let peerPrefix = "-HT0001-"
        g <- Random.newStdGen
        return . (++) peerPrefix $ take 12 $ (Random.randomRs ('0', '9') g)

getBInts :: [(BValue, a)] -> [a]
getBInts [] = []
getBInts ((k, v):xs) = if k == (BStr $ pack "length") then v : getBInts xs else getBInts xs

sumBInts :: [BValue] -> Integer
sumBInts ((BInt x):xs) = x + sumBInts xs
sumBInts [] = 0

-- checks if torrent is for multiple files or a single file
isMult :: IO Bool
isMult = do 
            (BDict dict) <- getBValue "file" Config.torrent
            let info = BStr (pack "info")
                f = BStr (pack "files")
                (BDict infoDict) = dict M.! info
            return $ M.member f infoDict

-- parse torrents with multiple files
parseDataMultiple :: IO Metadata
parseDataMultiple = do
                    (BDict dict) <- (getBValue "file" "torrents/karl_marx.torrent")
                    let announce = BStr (pack "announce")
                        info = BStr (pack "info")
                        f = BStr (pack "files")
                        pLen = BStr (pack "piece length")
                        (BStr announceUrl) = dict M.! announce
                        (BDict infoDict) = dict M.! info
                        (BList files) = infoDict M.! f
                        (BInt pieceLen) = infoDict M.! pLen
                        flattened = concat $ map (\(BDict x) -> M.toList x) files
                        totalLength = sumBInts $ getBInts flattened
                    return Metadata {
                        announce = unpack announceUrl,
                        tLen = show totalLength,
                        pieceCount = show (ceiling $ (fromIntegral totalLength) / (fromIntegral pieceLen))
                    }

-- parse torrents with single files
parseDataSingle :: IO Metadata
parseDataSingle = do
            (BDict dict) <- (getBValue "file" Config.torrent)
            let announce = BStr (pack "announce")
                info = BStr (pack "info")
                len = BStr (pack "length")
                pLen = BStr (pack "piece length")
                (BStr announceUrl) = dict M.! announce 
                (BDict infoDict) = dict M.! info
                (BInt _length) = infoDict M.! len
                (BInt pieceLen) = infoDict M.! pLen
            return Metadata {
                announce = unpack announceUrl,
                tLen = show _length,
                pieceCount = show $ ceiling ((fromIntegral _length) / (fromIntegral pieceLen))
            }         

parseFromFile :: Prim.Parsec B.ByteString () a -> String -> IO (Either PError.ParseError a)
parseFromFile p fname
    = do input <- B.readFile fname
         return (Prim.runP p () fname input)

parseFromString p str = do
                        let fname = "return_data"
                        return (Prim.runP p () fname str)
