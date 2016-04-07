module Parser (
    produceInterestPackets
    , produceInterests
    , produceContents
    --, produceContentPackets
    --, producePairs
) where

import System.IO
import System.Random
import Control.Monad
import Data.Text
import Data.Bits
import Data.Word
import Data.ByteString
import Data.ByteString.Char8

import Generator

--import Crypto.Hash.SHA56
import Codec.Crypto.RSA (sign, RSAError)
import Crypto.PubKey.OpenSsh (decodePrivate, OpenSshPrivateKey)
import Crypto.PubKey.OpenSsh( OpenSshPrivateKey( OpenSshPrivateKeyRsa ) )
import Crypto.Types.PubKey.RSA (PrivateKey)
{-
    Crypto: http://stackoverflow.com/questions/20318751/rsa-sign-using-a-privatekey-from-a-file
-}

class Serializer t where
    serialize :: t -> ByteString

class Encoder t where
    toTLV :: t -> TLV
    encodingSize :: t -> Int

data TwoByte = TType Word8 Word8 | Length Word8 Word8 deriving(Show)

-- TODO: reduce this into a single function... this is silly
-- TODO: make TType and Length fully-fledged types
intToTType :: Int -> TwoByte
intToTType x = (TType (fromIntegral (x `shiftR` 8)) (fromIntegral x))
intToLength :: Int -> TwoByte
intToLength x = (Length (fromIntegral (x `shiftR` 8)) (fromIntegral x))
intTo2Bytes :: Int -> [Word8]
intTo2Bytes x = [(fromIntegral (x `shiftR` 8)), (fromIntegral x)]

data TLV = RawTLV {
                tlv_type :: TwoByte,
                tlv_length :: TwoByte,
                tlv_raw_value :: Data.ByteString.ByteString }
            | NestedTLV {
                tlv_type :: TwoByte,
                tlv_length :: TwoByte,
                tlv_nested_value :: [TLV] }
            deriving (Show)

instance Serializer TLV where
    serialize (RawTLV (TType t1 t2) (Length l1 l2) v) =
        let bytelist = [(Data.ByteString.singleton t1),
                        (Data.ByteString.singleton t2),
                        (Data.ByteString.singleton l1),
                        (Data.ByteString.singleton l2),
                        v]
        in
        (Data.ByteString.concat bytelist)
    serialize (NestedTLV (TType t1 t2) (Length l1 l2) v) =
        let bytelist = [(Data.ByteString.singleton t1),
                        (Data.ByteString.singleton t2),
                        (Data.ByteString.singleton l1),
                        (Data.ByteString.singleton l2)]
                        ++ (Prelude.map serialize v) -- v :: [TLV], so we need to serialize each and then append it to the list
        in
        (Data.ByteString.concat bytelist)

data NameComponent = NameComponent {value :: String}
                     | PayloadId {value :: String} deriving (Show)
instance Encoder NameComponent where
    toTLV (NameComponent value) = RawTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_raw_value = bvalue }
        where
            bvalue = (Data.ByteString.Char8.pack value)
            blength = (Data.ByteString.length bvalue)
    toTLV (PayloadId value) = RawTLV {tlv_type = (intToTType 2), tlv_length = (intToLength blength), tlv_raw_value = bvalue }
        where
            bvalue = (Data.ByteString.Char8.pack value)
            blength = (Data.ByteString.length bvalue)

    encodingSize (NameComponent value) = 4 + (Data.ByteString.length bvalue)
        where
            bvalue = (Data.ByteString.Char8.pack value)
    encodingSize (PayloadId value) = 4 + (Data.ByteString.length bvalue)
        where
            bvalue = (Data.ByteString.Char8.pack value)

data Name = Name {components :: [NameComponent]} deriving (Show)
instance Encoder Name where
    toTLV (Name components) = NestedTLV {tlv_type = (intToTType 0), tlv_length = (intToLength csize), tlv_nested_value = nvalue }
        where
            nvalue = (Prelude.map toTLV components)
            csize = (sum (Prelude.map encodingSize components))
    encodingSize (Name components) = 4 + (sum (Prelude.map encodingSize components))

-- TODO: name should create a random name from a data source
-- TODO: implement name function to read from file
innerGenName :: [NameComponent] -> [String] -> Maybe Name
innerGenName nc (s:xs) = 
    let 
        component = NameComponent s
    in
        Just (Name (nc ++ [component]))
innerGenName nc [] = Just (Name nc)

name :: [String] -> Maybe Name
name nc = Just (Name [ NameComponent s | s <- nc ])

-- PACKET FORMAT
{-
                       1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +---------------+---------------+---------------+---------------+
   |    Version    |  PacketType   |         PacketLength          |
   +---------------+---------------+---------------+---------------+
   |           PacketType specific fields          | HeaderLength  |
   +---------------+---------------+---------------+---------------+
   / Optional Hop-by-hop header TLVs                               /
   +---------------+---------------+---------------+---------------+
   / PacketPayload TLVs                                            /
   +---------------+---------------+---------------+---------------+

                    1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +---------------+---------------+---------------+---------------+
   | CCNx NamedPayload TLV                                              /
   +---------------+---------------+---------------+---------------+
   / Optional CCNx ValidationAlgorithm TLV                         /
   +---------------+---------------+---------------+---------------+
   / Optional CCNx ValidationPayload TLV (ValidationAlg required)  /
   +---------------+---------------+---------------+---------------+
-}

type Version = Word8
type PacketType = Word8
type PacketLength = Word16
type HeaderLength = Word8
type ValidationTType = Word16

type KeyId = [Word8]
type Cert = [Word8]
type PubKey = [Word8]
type KeyName = Name

data Payload = Payload { bytes :: [Word8] } deriving(Show)
instance Encoder Payload where
    toTLV (Payload bytes) = RawTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_raw_value = bvalue }
        where
            bvalue = (Data.ByteString.pack bytes)
            blength = (Data.ByteString.length bvalue)

    encodingSize (Payload bytes) = 4 + (Data.ByteString.length (Data.ByteString.pack bytes))

gen_payload :: Int -> Int -> Payload
gen_payload n seed = Payload (randomBytes n seed)

class Packet t where
    preparePacket :: Maybe t -> Maybe Data.ByteString.ByteString

type NamedPayload = (Name, Payload)

data ValidationDependentData = ValidationDependentData KeyId PubKey Cert KeyName deriving(Show)
data ValidationPayload = ValidationPayload [Word8] deriving(Show)
data ValidationAlg = ValidationAlg ValidationTType ValidationDependentData deriving(Show)
data Validation = Validation ValidationAlg ValidationPayload deriving(Show)

data Interest = Interest Name | InterestWithPayload NamedPayload | SignedInterest NamedPayload Validation deriving(Show)
instance Encoder Interest where
    -- TL type is 1
    toTLV (Interest name) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name)]
            blength = (encodingSize name)
    -- TL type is 1
    toTLV (InterestWithPayload (name, payload)) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV payload)]
            blength = (sum [(encodingSize name), (encodingSize payload)])

    encodingSize (Interest name) = 4 + (encodingSize name)
    encodingSize (InterestWithPayload (name, payload)) = 4 + (sum [(encodingSize name), (encodingSize payload)])

instance Packet Interest where
    preparePacket (Just (Interest name)) =
        Just (prependFixedHeader 1 0 (serialize (toTLV (Interest name))))
    preparePacket (Just (InterestWithPayload (name, payload))) =
        Just (prependFixedHeader 1 0 (serialize (toTLV (InterestWithPayload (name, payload)))))
    preparePacket Nothing = Nothing

data Content = NamelessContent Payload 
               | Content NamedPayload 
               | SignedContent NamedPayload Validation 
               deriving (Show)

instance Encoder Content where
    -- TL type is 2
    toTLV (Content (name, payload)) = NestedTLV { tlv_type = (intToTType 2), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV payload)]
            blength = (sum [(encodingSize name), (encodingSize payload)])
    -- TL type is 2
    toTLV (NamelessContent payload) = NestedTLV { tlv_type = (intToTType 2), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV payload)]
            blength = (encodingSize payload)

    encodingSize (Content (name, payload)) = 4 + (sum [(encodingSize name), (encodingSize payload)])
    encodingSize (NamelessContent payload) = 4 + (encodingSize payload)

instance Packet Content where
    preparePacket (Just (Content (name, payload))) =
        Just (prependFixedHeader 1 1 (serialize (toTLV (Content (name, payload)))))
    preparePacket Nothing = Nothing

-- TODO: implement the manifest encoding
-- TODO: implement the body of the manifest
--data Manifest = Manifest NamedPayload | SignedManifest NamedPayload Validation deriving(Show)

interest :: [String] -> Maybe Interest
interest s =
    case (name s) of
        Nothing -> Nothing
        Just (Name nc) -> Just (Interest (Name nc))

content :: Payload -> [String] -> Maybe Content
content p s =
    case (name s) of
        Nothing -> Nothing
        Just (Name nc) -> Just (Content ((Name nc), p))

data FixedHeader = FixedHeader Version PacketType PacketLength deriving(Show)

prependFixedHeader :: Version -> PacketType -> Data.ByteString.ByteString -> Data.ByteString.ByteString
prependFixedHeader pv pt body =
    let bytes = [(Data.ByteString.singleton pv),
                 (Data.ByteString.singleton pt),
                 (Data.ByteString.pack (intTo2Bytes ((Data.ByteString.length body) + 8))), -- header length is 8 bytes
                 (Data.ByteString.pack [255,0,0,8]), -- the header length is 8 byets -- no optional headers, yet. 64 is hop limit
                 body]
    in
        (Data.ByteString.concat bytes)

produceInterests :: [[String]] -> [Maybe Interest]
produceInterests (s:xs) = 
    case (interest s) of
        Nothing -> []
        Just (Interest msg) -> [Just (Interest msg)] ++ (produceInterests xs)
produceInterests [] = []

produceInterestPackets :: [[String]] -> [Maybe ByteString]
produceInterestPackets s = preparePacket <$> produceInterests s

produceContents :: [[String]] -> [[Word8]] -> [Maybe Content]
produceContents (n:ns) (p:ps) =
    case (content (Payload p) n) of 
        Nothing -> []
        Just (Content msg) -> [Just (Content msg)] ++ (produceContents ns ps)
produceContents [] _ = []
produceContents _ [] = []

produceContentPackets :: [[String]] -> [[Word8]] -> [Maybe ByteString]
produceContentPackets n s = preparePacket <$> produceContents n s



--producePairs :: (RandomGen t) => [(Int, Int)] -> t -> [(ByteString, ByteString)]
--producePairs ((n,p):xs) g =
--    let (_, g') = (next g) in
--        [ ((preparePacket (interest n g)), (preparePacket (content n p g))) ] ++ (producePairs xs g')
--producePairs _ _ = []

--  producePairs (Prelude.zip (randomInts 1 2 3) (randomInts 1 100 400)) (mkStdGen 42)

--loadKeyFromFile :: String -> PrivateKey
--loadKeyFromFile fname = do
--    content <- Data.ByteString.readFile fname
--    case (decodePrivate content) of
--        Right (OpenSshPrivateKeyRsa key) -> key
--        Right _ -> error "Wrong key type"
--        Left err -> error err

throwLeft :: Either String OpenSshPrivateKey -> PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = k
throwLeft (Right _) = error "Wrong key type"
throwLeft (Left s)  = error $ "Error reading keys: " ++ s

loadKey :: FilePath -> IO PrivateKey
loadKey p = (throwLeft . decodePrivate) `fmap` Data.ByteString.readFile p

-- call sign key msg here...
