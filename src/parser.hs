import System.IO  
import System.Random
import Control.Monad
import Data.Bits
import Data.Word
import Data.ByteString
import Data.ByteString.Char8

class Serializer t where
    serialize :: t -> ByteString

class Encoder t where
    toTLV :: t -> TLV
    encodingSize :: t -> Int

randomByteStream :: (Random a) => Int -> [a]
randomByteStream seed = randoms (mkStdGen seed)

randomBytes :: Int -> [Word8]
randomBytes n = Prelude.take n (randomByteStream 42 :: [Word8])

data TwoByte = Type Word8 Word8 | Length Word8 Word8 deriving(Show) 

intToType :: Int -> TwoByte
intToType x = (Type (fromIntegral (x `shiftR` 8)) (fromIntegral x))
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
    serialize (RawTLV (Type t1 t2) (Length l1 l2) v) = 
        let bytelist = [(Data.ByteString.singleton t1), 
                        (Data.ByteString.singleton t2), 
                        (Data.ByteString.singleton l1), 
                        (Data.ByteString.singleton l2), 
                        v]
        in
        (Data.ByteString.concat bytelist)
    serialize (NestedTLV (Type t1 t2) (Length l1 l2) v) = 
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
    toTLV (NameComponent value) = RawTLV { tlv_type = (intToType 1), tlv_length = (intToLength blength), tlv_raw_value = bvalue } 
        where 
            bvalue = (Data.ByteString.Char8.pack value)
            blength = (Data.ByteString.length bvalue)
    toTLV (PayloadId value) = RawTLV {tlv_type = (intToType 2), tlv_length = (intToLength blength), tlv_raw_value = bvalue }
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
    toTLV (Name components) = NestedTLV {tlv_type = (intToType 0), tlv_length = (intToLength csize), tlv_nested_value = nvalue }
        where 
            -- goal: convert each component to a TLV, serialize the TLVs, then append the TLVs
            nvalue = (Prelude.map toTLV components)
            csize = (sum (Prelude.map encodingSize components))
    encodingSize (Name components) = 4 + (sum (Prelude.map encodingSize components))

-- TODO: gen_name_component should create a random name component from a file
gen_name_component :: NameComponent
gen_name_component = (NameComponent "random_component")

-- TODO: gen_name should create a random name from a data source
inner_gen_name :: [NameComponent] -> Int -> Name
inner_gen_name nc n = 
    if n <= 1 then 
        Name (nc ++ [gen_name_component])
    else 
        inner_gen_name (nc ++ [gen_name_component]) (n - 1)

gen_name :: Int -> Name
gen_name n = inner_gen_name [] n


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
type ValidationType = Word16

type KeyId = [Word8]
type Cert = [Word8]
type PublicKey = [Word8]
type KeyName = Name

data Payload = Payload { bytes :: [Word8] } deriving(Show)
instance Encoder Payload where
    toTLV (Payload bytes) = RawTLV { tlv_type = (intToType 0), tlv_length = (intToLength blength), tlv_raw_value = bvalue }
        where
            bvalue = (Data.ByteString.pack bytes)
            blength = (Data.ByteString.length bvalue)

    encodingSize (Payload bytes) = 4 + (Data.ByteString.length (Data.ByteString.pack bytes))

gen_payload :: Int -> Payload
gen_payload n = Payload (randomBytes n)

class Packet t where
    preparePacket :: t -> Data.ByteString.ByteString

type Message = (Name, Payload)

data ValidationDependentData = ValidationDependentData KeyId PublicKey Cert KeyName deriving(Show)
data ValidationPayload = ValidationPayload [Word8] deriving(Show)
data ValidationAlg = ValidationAlg ValidationType ValidationDependentData deriving(Show)
data Validation = Validation ValidationAlg ValidationPayload deriving(Show)

data Interest = Interest Name | InterestWithPayload Message | SignedInterest Message Validation deriving(Show)
instance Encoder Interest where
    toTLV (Interest name) = NestedTLV { tlv_type = (intToType 0), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name)]
            blength = (encodingSize name)
    toTLV (InterestWithPayload (name, payload)) = NestedTLV { tlv_type = (intToType 0), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV payload)]
            blength = (sum [(encodingSize name), (encodingSize payload)])

    encodingSize (Interest name) = 4 + (encodingSize name)
    encodingSize (InterestWithPayload (name, payload)) = 4 + (sum [(encodingSize name), (encodingSize payload)])

-- TODO: preparePacket == prependFixedHeader (serialize (toTLV interest))
instance Packet Interest where
    preparePacket (Interest name) = Data.ByteString.pack [0 :: Word8]
    preparePacket (InterestWithPayload (name, payload)) = Data.ByteString.pack [0 :: Word8]

data Content = Content Message | SignedContent Message Validation deriving(Show)
instance Encoder Content where
    toTLV (Content (name, payload)) = NestedTLV { tlv_type = (intToType 0), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV payload)]
            blength = (sum [(encodingSize name), (encodingSize payload)])
    encodingSize (Content (name, payload)) = 4 + (sum [(encodingSize name), (encodingSize payload)])

-- TODO: implement the manifest encoding 
-- TODO: implement the body of the manifest
--data Manifest = Manifest Message | SignedManifest Message Validation deriving(Show)

gen_interest :: Int -> Interest
gen_interest nl = Interest (gen_name nl)


data FixedHeader = FixedHeader Version PacketType PacketLength deriving(Show)

prependFixedHeader :: Version -> PacketType -> Data.ByteString.ByteString -> Data.ByteString.ByteString
prependFixedHeader pv pt body = 
    let bytes = [(Data.ByteString.singleton pv),
                 (Data.ByteString.singleton pt),
                 (Data.ByteString.pack (intTo2Bytes ((Data.ByteString.length body) + 8))), -- header length is 8 bytes
                 (Data.ByteString.pack [64,0,0,8]), -- the header length is 8 byets -- no optional headers, yet. 64 is hop limit
                 body] 
    in
        (Data.ByteString.concat bytes) 

-- shorthand IO functions
bshow :: Data.ByteString.ByteString -> IO ()
bshow s = (Data.ByteString.Char8.putStrLn s)

bwrite :: Data.ByteString.ByteString -> String -> IO ()
bwrite s f = (Data.ByteString.writeFile f s)

