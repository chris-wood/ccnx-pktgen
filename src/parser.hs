module Parser (
    produceInterests
    , produceContents
    , producePairs
) where

import System.IO
import System.Random
import Control.Monad
import Data.Text
import Data.Bits
import Data.Word
import Data.ByteString
import Data.ByteString.Char8

--import Crypto.Hash.SHA56
import Codec.Crypto.RSA (sign, RSAError)
import Crypto.PubKey.OpenSsh (decodePrivate, OpenSshPrivateKey)
import Crypto.PubKey.OpenSsh( OpenSshPrivateKey( OpenSshPrivateKeyRsa ) )
import Crypto.Types.PubKey.RSA (PrivateKey)
-- import Data.ByteString (ByteString)

-- TODO: need to create a NameGenerator class
-- -> use this in the interest and content and pair production functions

-- TODO: signing example (get it working!)
-- TODO: file loading example
-- TODO: Throughput calculation

{-
    Networking example: http://book.realworldhaskell.org/read/sockets-and-syslog.html#sockets.udp.client
    Crypto: http://stackoverflow.com/questions/20318751/rsa-sign-using-a-privatekey-from-a-file
-}

-- TODO: implement functions to produce streams of packets -- what does that API look like?
-- produceInterest (stream parameters where one calls 'take') and produces stream of interests
-- producerInterest = preparePacket 1 0 (gen_interest (take name_length))
-- -> list of natural numbers: num1 = 1 : map (+1) num1, call `take n num1`
-- TODO: still requires random name creation... what's the best way?

class Serializer t where
    serialize :: t -> ByteString

class Encoder t where
    toTLV :: t -> TLV
    encodingSize :: t -> Int

randomIntStream :: (Random a) => Int -> [a]
randomIntStream seed = randoms (mkStdGen seed)

randomBytes :: Int -> [Word8]
randomBytes n = Prelude.take n (randomIntStream 42 :: [Word8])

_modSwap a b = mod b a
randomInts :: Int -> Int -> Int -> [Int]
randomInts n low high = Prelude.take n (Prelude.map (+ low) (Prelude.map (_modSwap (high - low)) (randomIntStream 42)))

randomString :: (RandomGen t) => t -> Int -> Int -> String
randomString g low high = let (len, g') = randomR (low, high) g -- randomR returns a new value for the std generator
                  in Data.Text.unpack (Data.Text.unfoldrN len rand_text (len, g'))
 where rand_text (0,_) = Nothing
       rand_text (k,g) = let (c, g') = randomR ('a','z') g
                         in Just (c, ((k-1), g'))

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

-- TODO: gen_name_component should create a random name component from a file
gen_name_component :: Int -> Int -> NameComponent
gen_name_component low high = (NameComponent (randomString (mkStdGen 42) low high))
--gen_single_name_component = (NameComponent (randomString (mkStdGen 42) 1 10))
gen_single_name_component prng = (NameComponent (randomString prng 1 10))

-- TODO: gen_name should create a random name from a data source
-- TODO: implement gen_name function to read from file
inner_gen_name :: (RandomGen t) => [NameComponent] -> Int -> t -> Name
inner_gen_name nc n prng =
    let (_, prng') = (next prng) in
        if n <= 1 then
            Name (nc ++ [gen_single_name_component prng'])
        else
            inner_gen_name (nc ++ [gen_single_name_component prng']) (n - 1) prng'

gen_name :: (RandomGen t) => Int -> t -> Name
gen_name n g = inner_gen_name [] n g

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
   | CCNx Message TLV                                              /
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

gen_payload :: Int -> Payload
gen_payload n = Payload (randomBytes n)

class Packet t where
    preparePacket :: t -> Data.ByteString.ByteString

type Message = (Name, Payload)

data ValidationDependentData = ValidationDependentData KeyId PubKey Cert KeyName deriving(Show)
data ValidationPayload = ValidationPayload [Word8] deriving(Show)
data ValidationAlg = ValidationAlg ValidationTType ValidationDependentData deriving(Show)
data Validation = Validation ValidationAlg ValidationPayload deriving(Show)

data Interest = Interest Name | InterestWithPayload Message | SignedInterest Message Validation deriving(Show)
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
    preparePacket (Interest name) =
        prependFixedHeader 1 0 (serialize (toTLV (Interest name)))
    preparePacket (InterestWithPayload (name, payload)) =
        prependFixedHeader 1 0 (serialize (toTLV (InterestWithPayload (name, payload))))

data Content = NamelessContent Payload | Content Message | SignedContent Message Validation deriving(Show)
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
    preparePacket (Content (name, payload)) =
        prependFixedHeader 1 1 (serialize (toTLV (Content (name, payload))))

-- TODO: implement the manifest encoding
-- TODO: implement the body of the manifest
--data Manifest = Manifest Message | SignedManifest Message Validation deriving(Show)

gen_interest :: (RandomGen t) => Int -> t ->Interest
gen_interest nl g = Interest (gen_name nl g)

gen_content :: (RandomGen t) => Int -> Int -> t -> Content
gen_content nl pl g = Content ((gen_name nl g), (gen_payload pl))

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


produceInterests :: (RandomGen t) => [Int] -> t -> [ByteString]
produceInterests (n:ns) g =
    let (_, g') = (next g) in
        [ (preparePacket (gen_interest n g)) ] ++ (produceInterests ns g')
produceInterests [] _ = []
-- produceInterests (randomInts 100 0 10)

produceContents :: (RandomGen t) => [(Int, Int)] -> t -> [ByteString]
produceContents ((n,p):xs) g =
    let (_, g') = (next g) in
        [ (preparePacket (gen_content n p g)) ] ++ (produceContents xs g')
produceContents _ _ = []
-- e.g., produceContents [(1,2),(1,2)]

producePairs :: (RandomGen t) => [(Int, Int)] -> t -> [(ByteString, ByteString)]
producePairs ((n,p):xs) g =
    let (_, g') = (next g) in
        [ ((preparePacket (gen_interest n g)), (preparePacket (gen_content n p g))) ] ++ (producePairs xs g')
producePairs _ _ = []

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
