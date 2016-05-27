module Parser (
    produceInterestPackets
    , produceInterests
    , produceContents
    , produceContentPackets
    , producePacketPairs
) where

import System.IO
import System.Random
import Control.Monad
import Data.Text
import Data.Bits
import Data.Word
import Data.Maybe
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.ByteString
import qualified Data.Digest.Pure.SHA as SHA

import Generator

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

name :: [String] -> Maybe Name
name nc = Just (Name [ NameComponent s | s <- nc ])

type Version = Word8
type PacketType = Word8
type PacketLength = Word16
type HeaderLength = Word8
type VaidationType = Word16

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

class Packet t where
    preparePacket :: Maybe t -> Maybe Data.ByteString.ByteString

type NamedPayload = (Name, Payload)

data ValidationDependentData = ValidationDependentData KeyId PubKey Cert KeyName deriving(Show)
data ValidationPayload = ValidationPayload [Word8] deriving(Show)
data ValidationAlg = ValidationAlg VaidationType ValidationDependentData deriving(Show)
data Validation = Validation ValidationAlg ValidationPayload deriving(Show)

data Interest = SimpleInterest Name
                | InterestWithPayload NamedPayload
                | SignedInterest NamedPayload Validation
                deriving(Show)

instance Encoder Interest where
    -- TL type is 1
    toTLV (SimpleInterest name) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name)]
            blength = (encodingSize name)
    -- TL type is 1
    toTLV (InterestWithPayload (name, payload)) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV payload)]
            blength = (sum [(encodingSize name), (encodingSize payload)])

    encodingSize (SimpleInterest name) = 4 + (encodingSize name)
    encodingSize (InterestWithPayload (name, payload)) = 4 + (sum [(encodingSize name), (encodingSize payload)])

instance Packet Interest where
    preparePacket (Just (SimpleInterest name)) =
        Just (prependFixedHeader 1 0 (serialize (toTLV (SimpleInterest name))))
    preparePacket (Just (InterestWithPayload (name, payload))) =
        Just (prependFixedHeader 1 0 (serialize (toTLV (InterestWithPayload (name, payload)))))
    preparePacket Nothing = Nothing

data Content = NamelessContent Payload
               | SimpleContent NamedPayload
               | SignedContent NamedPayload Validation
               deriving (Show)

instance Encoder Content where
    -- TL type is 2
    toTLV (SimpleContent (name, payload)) = NestedTLV { tlv_type = (intToTType 2), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV payload)]
            blength = (sum [(encodingSize name), (encodingSize payload)])
    -- TL type is 2
    toTLV (NamelessContent payload) = NestedTLV { tlv_type = (intToTType 2), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV payload)]
            blength = (encodingSize payload)

    encodingSize (SimpleContent (name, payload)) = 4 + (sum [(encodingSize name), (encodingSize payload)])
    encodingSize (NamelessContent payload) = 4 + (encodingSize payload)

instance Packet Content where
    preparePacket (Just (SimpleContent (name, payload))) =
        Just (prependFixedHeader 1 1 (serialize (toTLV (SimpleContent (name, payload)))))
    preparePacket Nothing = Nothing

data HashGroupPointer = DataPointer ByteString
                       | ManifestPointer ByteString
                       deriving (Show)

instance Encoder HashGroupPointer where
    toTLV (DataPointer bytes) = RawTLV { tlv_type = (intToTType 1), tlv_length = (intToLength (Data.ByteString.length bytes)), tlv_raw_value = bytes }
        -- where
        --     bvalue = (Data.ByteString.pack bytes)
        --     blength = (Data.ByteString.length bvalue)
    toTLV (ManifestPointer bytes) = RawTLV { tlv_type = (intToTType 1), tlv_length = (intToLength (Data.ByteString.length bytes)), tlv_raw_value = bytes }
        -- where
        --     bvalue = (Data.ByteString.pack bytes)
        --     blength = (Data.ByteString.length bvalue)

    encodingSize (DataPointer bytes) = 4 + (Data.ByteString.length bytes)
    encodingSize (ManifestPointer bytes) = 4 + (Data.ByteString.length bytes)

data ManifestHashGroup = ManifestHashGroup [HashGroupPointer] deriving (Show)

instance Encoder ManifestHashGroup where
    toTLV (ManifestHashGroup pointers) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = Prelude.map toTLV pointers
            blength = sum (Prelude.map encodingSize pointers)
    encodingSize (ManifestHashGroup pointers) = 4 + (sum (Prelude.map encodingSize pointers))

type ManifestBody = [ManifestHashGroup]

data Manifest = SimpleManifest Name ManifestBody
                | NamelessManifest ManifestBody
                | SignedManifest Name ManifestBody Validation
                deriving (Show)



instance Encoder Manifest where
    toTLV (SimpleManifest name body) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name)] ++ (Prelude.map toTLV body)
            blength = sum ([(encodingSize name)] ++ (Prelude.map encodingSize body))
    toTLV (NamelessManifest body) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = Prelude.map toTLV body
            blength = sum (Prelude.map encodingSize body)
-- TODO: implement the validation encoder
--    toTLV (SignedManifest name body validation)

    encodingSize (SimpleManifest name body) = 4 + (sum ([(encodingSize name)] ++ (Prelude.map encodingSize body)))
    encodingSize (NamelessManifest body) = 4 + (sum (Prelude.map encodingSize body))

instance Packet Manifest where
    preparePacket (Just (SimpleManifest name body)) =
        Just (prependFixedHeader 1 1 (serialize (toTLV (SimpleManifest name body))))
    preparePacket (Just (NamelessManifest body)) =
        Just (prependFixedHeader 1 1 (serialize (toTLV (NamelessManifest body))))
    preparePacket Nothing = Nothing

-- We have three types of messages: Interest, Content Object, and (FLIC) Manifest
data Message = IMessage Interest | CMessage Content | MMessage Manifest deriving (Show)
instance Packet Message where
    preparePacket (Just (IMessage interest)) = preparePacket (Just interest)
    preparePacket (Just (CMessage content)) = preparePacket (Just content)
    preparePacket (Just (MMessage manifest)) = preparePacket (Just manifest)
    preparePacket Nothing = Nothing
--type Message = Interest | Content | Manifest

interest :: [String] -> Maybe Interest
interest s =
    case (name s) of
        Nothing -> Nothing
        Just (Name nc) -> Just (SimpleInterest (Name nc))

content :: Payload -> [String] -> Maybe Content
content p s =
    case (name s) of
        Nothing -> Nothing
        Just (Name nc) -> Just (SimpleContent ((Name nc), p))

messagesToHashDigests :: [Message] -> [ByteString]
messagesToHashDigests messages = do
    let rawPackets = Prelude.map Data.Maybe.fromJust (Prelude.map preparePacket (Prelude.map (\x -> Just x) messages))
    let hashChunks = Prelude.reverse (Prelude.map SHA.sha256 (Lazy.fromStrict <$> rawPackets))
        in
            Prelude.map Lazy.toStrict (Prelude.map SHA.bytestringDigest hashChunks)

manifestFromPointers :: [HashGroupPointer] -> Manifest
manifestFromPointers pointers = do
    let hashGroup = ManifestHashGroup pointers
        in NamelessManifest [hashGroup]

namedManifestFromPointers :: Name -> [HashGroupPointer] -> Manifest
namedManifestFromPointers n pointers = do
    let hashGroup = ManifestHashGroup pointers 
        in SimpleManifest n [hashGroup]

balancedManifestTreeFromPointers :: [Message] -> Name -> Int -> [HashGroupPointer] -> [Message]
balancedManifestTreeFromPointers messageList n numPointers pointers
    -- Base case: when we don't need to extend the tree to add more pointers
    | Prelude.length pointers < numPointers = do
        let manifestNode = MMessage (namedManifestFromPointers n pointers)
            in
                messageList ++ [manifestNode]

    -- Recurse and add more pointers to the tree
    | otherwise = do
        let pointerChunks = splitIntoChunks numPointers pointers

        -- Note: this will build a tree of manifests
        let manifestNodes = Prelude.map manifestFromPointers pointerChunks
        let rawPackets = Prelude.map Data.Maybe.fromJust (Prelude.map preparePacket (Prelude.map (\x -> Just x) manifestNodes))
        let hashChunks = Prelude.reverse (Prelude.map SHA.sha256 (Lazy.fromStrict <$> rawPackets))
        let manifestPointers = Prelude.map ManifestPointer (Prelude.map Lazy.toStrict (Prelude.map SHA.bytestringDigest hashChunks))
            in
                balancedManifestTreeFromPointers (messageList ++ (Prelude.map MMessage manifestNodes)) n numPointers manifestPointers

-- TODO: extract the data pointer generation into a separate function
balancedManifestTree :: [String] -> Int -> [Content] -> [Message]
balancedManifestTree s numPointers datas =
    case (name s) of
        Nothing -> []
        Just (Name nc) -> do
            let rawPackets = Prelude.map Data.Maybe.fromJust (Prelude.map preparePacket (Prelude.map (\x -> Just x) datas))
            let hashChunks = Prelude.reverse (Prelude.map SHA.sha256 (Lazy.fromStrict <$> rawPackets))
            let dataPointers = Prelude.map DataPointer (Prelude.map Lazy.toStrict (Prelude.map SHA.bytestringDigest hashChunks))
                in
                    balancedManifestTreeFromPointers [] (Name nc) numPointers dataPointers

-- TODO: write a function that takes a name and chunks and creates a single manifest
-- use it with foldl to generate the single manifest

data FixedHeader = FixedHeader Version PacketType PacketLength deriving(Show)

prependFixedHeader :: Version -> PacketType -> Data.ByteString.ByteString -> Data.ByteString.ByteString
prependFixedHeader pv pt body =
    let bytes = [(Data.ByteString.singleton pv),
                 (Data.ByteString.singleton pt),
                 (Data.ByteString.pack (intTo2Bytes ((Data.ByteString.length body) + 8))), -- header length is 8 bytes
                 (Data.ByteString.pack [255,0,0,8]), -- the header length is 8 bytes; no optional headers, yet. 255 is hop limit.
                 body]
    in
        (Data.ByteString.concat bytes)

produceInterests :: [[String]] -> [Maybe Interest]
produceInterests (s:xs) =
    case (interest s) of
        Nothing -> []
        Just (SimpleInterest msg) -> [Just (SimpleInterest msg)] ++ (produceInterests xs)
produceInterests [] = []

produceInterestPackets :: [[String]] -> [Maybe ByteString]
produceInterestPackets s = preparePacket <$> produceInterests s

produceContents :: [[String]] -> [[Word8]] -> [Maybe Content]
produceContents (n:ns) (p:ps) =
    case (content (Payload p) n) of
        Nothing -> []
        Just (SimpleContent msg) -> [Just (SimpleContent msg)] ++ (produceContents ns ps)
produceContents [] _ = []
produceContents _ [] = []

produceContentPackets :: [[String]] -> [[Word8]] -> [Maybe ByteString]
produceContentPackets names payloads = preparePacket <$> produceContents names payloads

producePacketPairs :: [[String]] -> [[Word8]] -> [(Maybe ByteString, Maybe ByteString)]
producePacketPairs n p = do
    let interests = produceInterestPackets n
    let contents = produceContentPackets n p
        in
            Prelude.zip interests contents

splitIntoChunks _ [] = []
splitIntoChunks n s
    | n > 0 = (Prelude.take n s) : (splitIntoChunks n (Prelude.drop n s))
    | otherwise = error "Error: the splitIntoChunks parameter must be positive"

produceManifests :: [[String]] -> [ByteString] -> [Maybe Manifest]
produceManifests names datas = do
    let hashChunks = splitIntoChunks 2 (Prelude.reverse (Prelude.map SHA.sha256 (Prelude.map Lazy.fromStrict datas)))
        in
            -- TODO: use foldl to create a manifest, and insert the new value into the list (hashed), and continue with the creation
            --Prelude.foldl
            []
    -- case (content (Payload p) n) of
    --     Nothing -> []
    --     Just (Content msg) -> [Just (Content msg)] ++ (produceManifests ns ps)

produceManifestPackets :: [[String]] -> [ByteString] -> [Maybe ByteString]
produceManifestPackets names datas = preparePacket <$> produceManifests names datas
