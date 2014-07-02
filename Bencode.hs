module Bencode where

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.List as L
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

    --let f k v result = result ++ bencode k ++ ":" ++ bencode v
    --    list = map 
    --in (M.foldrWithKey f "d" dict) ++ "e"

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

-- get BDict
getBValue :: IO BValue
getBValue = do
        torrentInfo <- (readBencodedFile "ubuntu.torrent")
        let unwrapped = fromRight (BDict (M.fromList [])) torrentInfo
        return unwrapped

-- get the info_hash        
getHash :: IO String
getHash = do
            (BDict dict) <- getBValue
            let info = BStr (pack "info")
                a@(BDict infoDict) = dict M.! info
                bencoded = strToBS a
                hashed = hash bencoded
            return $ toHex hashed

toHex :: B.ByteString -> String
toHex bytes = unpack bytes >>= printf "%02x"

-- following the azeureus style for generating a peer_id
peerID :: IO String
peerID = do
        let peerPrefix = "-HT0001-"
        g <- Random.newStdGen
        return . (++) peerPrefix $ take 12 $ (Random.randomRs ('0', '9') g)

parseData :: IO (M.Map [Char] [Char])
parseData = do
            (BDict dict) <- getBValue
            let announce = BStr (pack "announce")
                info = BStr (pack "info")
                len = BStr (pack "length")
                name = BStr (pack "name")
                p_len = BStr (pack "piece length")
                p = BStr (pack "pieces")
                (BStr announceUrl) = dict M.! announce 
                (BDict infoDict) = dict M.! info
                (BInt _length) = infoDict M.! len
                (BStr _name) = infoDict M.! name
                (BInt pieceLength) = infoDict M.! p_len
                (BStr pieces) = infoDict M.! p
            return $ M.fromList [("announce", unpack announceUrl), 
                                ("piece length", show pieceLength), 
                                ("name", unpack _name), ("length", show _length), 
                                ("pieces", unpack pieces)]

parseFromFile :: Prim.Parsec B.ByteString () a -> String -> IO (Either PError.ParseError a)
parseFromFile p fname
    = do input <- B.readFile fname
         return (Prim.runP p () fname input)
