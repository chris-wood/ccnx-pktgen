module CCNxPacketGenerator (
    preparePacket
    , produceInterestPackets
    , produceInterests
    , produceContents
    , produceNamelessContents
    , produceContentPackets
    , producePacketPairs
    , balancedManifestTree
    , createInterest
    , createContent
    , createPayload
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
import Debug.Trace

import Numeric (readHex)

import Generator

class Serializer t where
    serialize :: t -> ByteString

class Encoder t where
    toTLV :: t -> TLV
    encodingSize :: t -> Int

data TwoByte = TType Word8 Word8 | Length Word8 Word8 deriving(Show)

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

type Cert = [Word8]
type PubKey = [Word8]
type KeyName = Name

data Payload = Payload [Word8] deriving(Show)
instance Encoder Payload where
    toTLV (Payload bytes) = RawTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_raw_value = bvalue }
        where
            bvalue = (Data.ByteString.pack bytes)
            blength = (Data.ByteString.length bvalue)

    encodingSize (Payload bytes) = 4 + (Data.ByteString.length (Data.ByteString.pack bytes))

data KeyId = KeyId [Word8] deriving(Show)
instance Encoder KeyId where
    toTLV (KeyId bytes) = RawTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_raw_value = bvalue }
        where
            bvalue = (Data.ByteString.pack bytes)
            blength = (Data.ByteString.length bvalue)

    encodingSize (KeyId bytes) = 4 + (Data.ByteString.length (Data.ByteString.pack bytes))

data ContentId = ContentId [Word8] deriving(Show)
instance Encoder ContentId where
    toTLV (ContentId bytes) = RawTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_raw_value = bvalue }
        where
            bvalue = (Data.ByteString.pack bytes)
            blength = (Data.ByteString.length bvalue)

    encodingSize (ContentId bytes) = 4 + (Data.ByteString.length (Data.ByteString.pack bytes))

createKeyId :: [Char] -> Maybe KeyId
createKeyId string 
    | Prelude.length string > 0 = Just (KeyId (stringToBytes string))
    | otherwise = Nothing

createContentId :: [Char] -> Maybe ContentId
createContentId string 
    | Prelude.length string > 0 = Just (ContentId (stringToBytes string))
    | otherwise = Nothing

class Packet t where
    preparePacket :: Maybe t -> Maybe Data.ByteString.ByteString

type NamedPayload = (Name, Payload)

data ValidationDependentData = ValidationDependentData KeyId PubKey Cert KeyName deriving(Show)
data ValidationPayload = ValidationPayload [Word8] deriving(Show)
data ValidationAlg = ValidationAlg VaidationType ValidationDependentData deriving(Show)
data Validation = Validation ValidationAlg ValidationPayload deriving(Show)

data Interest = SimpleInterest Name
                | RestrictedInterest Name (Maybe KeyId) (Maybe ContentId)
                | InterestWithPayload NamedPayload
                | SignedInterest NamedPayload Validation
                deriving(Show)

instance Encoder Interest where
    toTLV (SimpleInterest name) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name)]
            blength = (encodingSize name)
    toTLV (InterestWithPayload (name, payload)) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV payload)]
            blength = (sum [(encodingSize name), (encodingSize payload)])

    toTLV (RestrictedInterest name Nothing Nothing) = toTLV (SimpleInterest name)
    toTLV (RestrictedInterest name (Just keyId) Nothing) =  NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV keyId)]
            blength = (sum [(encodingSize name), (encodingSize keyId)])
    toTLV (RestrictedInterest name Nothing (Just contentId)) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV contentId)]
            blength = (sum [(encodingSize name), (encodingSize contentId)])
    toTLV (RestrictedInterest name (Just keyId) (Just contentId)) = NestedTLV { tlv_type = (intToTType 1), tlv_length = (intToLength blength), tlv_nested_value = bvalue }
        where
            bvalue = [(toTLV name), (toTLV keyId), (toTLV contentId)]
            blength = (sum [(encodingSize name), (encodingSize keyId), (encodingSize contentId)])
            
    encodingSize (SimpleInterest name) = 4 + (encodingSize name)
    encodingSize (InterestWithPayload (name, payload)) = 4 + (sum [(encodingSize name), (encodingSize payload)])
    encodingSize (RestrictedInterest name Nothing Nothing) = 4 + (encodingSize name)
    encodingSize (RestrictedInterest name (Just keyId) Nothing) = 4 + (sum [(encodingSize name), (encodingSize keyId)])
    encodingSize (RestrictedInterest name Nothing (Just contentId)) = 4 + (sum [(encodingSize name), (encodingSize contentId)])
    encodingSize (RestrictedInterest name (Just keyId) (Just contentId)) = 4 + (sum [(encodingSize name), (encodingSize keyId), (encodingSize contentId)])

instance Packet Interest where
    preparePacket (Just (SimpleInterest name)) =
        Just (prependFixedHeader 1 0 (serialize (toTLV (SimpleInterest name))))
    preparePacket (Just (RestrictedInterest name keyId contentId)) =
        Just (prependFixedHeader 1 0 (serialize (toTLV (RestrictedInterest name keyId contentId))))
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
    preparePacket (Just (NamelessContent payload)) =
        Just (prependFixedHeader 1 1 (serialize (toTLV (NamelessContent payload))))
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

createSimpleInterest :: [String] -> Maybe Interest
createSimpleInterest s =
    case (name s) of
        Nothing -> Nothing
        Just (Name nc) -> Just (SimpleInterest (Name nc))

-- Helper 
stringToByte :: [Char] -> Word8
stringToByte string = fromIntegral (fst ((readHex string) !! 0))

stringToBytes :: [Char] -> [Word8]
stringToBytes string 
    | Prelude.length string > 2 = [(stringToByte (Prelude.take 2 string))] ++ (stringToBytes (Prelude.drop 2 string))
    | otherwise = [(stringToByte (Prelude.take 2 string))]

createInterest :: [String] -> String -> String -> Maybe Interest
createInterest nameString keyId contentId = 
    case (name nameString) of
        Nothing -> Nothing
        Just (Name nc) -> Just (RestrictedInterest (Name nc) (createKeyId  keyId) (createContentId contentId))
            
createContent :: Payload -> [String] -> Maybe Content
createContent p s =
    case (name s) of
        Nothing -> Nothing
        Just (Name nc) -> Just (SimpleContent ((Name nc), p))

createNamelessContent :: Payload -> Maybe Content
createNamelessContent p = Just (NamelessContent p)

createPayload :: [Word8] -> Payload
createPayload d = Payload d

messagesToHashDigests :: [Message] -> [ByteString]
messagesToHashDigests messages = do
    let rawPackets = Prelude.map Data.Maybe.fromJust (Prelude.map preparePacket (Prelude.map (\x -> Just x) messages))
    let hashChunks = Prelude.reverse (Prelude.map SHA.sha256 (fmap Lazy.fromStrict rawPackets))
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

splitIntoChunks _ [] = []
splitIntoChunks n s
    | n > 0 = (Prelude.take n s) : (splitIntoChunks n (Prelude.drop n s))
    | otherwise = error "Error: the splitIntoChunks parameter must be positive"

balancedManifestTreeFromPointers :: [Message] -> Name -> Int -> [HashGroupPointer] -> [Message]
balancedManifestTreeFromPointers messageList rootName numPointers pointers
    -- Base case: when we don't need to extend the tree to add more pointers
    | (Prelude.length pointers) < numPointers = do
        -- System.IO.putStrLn "balancedManifestTreeFromPointers leaf"
        let manifestNode = MMessage (namedManifestFromPointers rootName pointers)
            in
                messageList ++ [manifestNode]

    -- Recurse and add more pointers to the tree
    | otherwise = do
        -- System.IO.putStrLn "balancedManifestTreeFromPointers inner node"
        []

        let pointerChunks = splitIntoChunks numPointers pointers

        -- Note: this will build a tree of manifests
        let manifestNodes = Prelude.map manifestFromPointers pointerChunks
        let rawPackets = Prelude.map Data.Maybe.fromJust (Prelude.map preparePacket (Prelude.map (\x -> Just x) manifestNodes))
        let hashChunks = Prelude.reverse (Prelude.map SHA.sha256 (fmap Lazy.fromStrict rawPackets))
        let manifestPointers = Prelude.map ManifestPointer (Prelude.map Lazy.toStrict (Prelude.map SHA.bytestringDigest hashChunks))
            in
                balancedManifestTreeFromPointers (messageList ++ (Prelude.map MMessage manifestNodes)) rootName numPointers manifestPointers

balancedManifestTree :: [String] -> Int -> [Content] -> [Message]
balancedManifestTree s numPointers datas =
    case (name s) of
        Nothing -> []
        Just (Name nc) -> do
            let rawContents = Prelude.map CMessage datas
            let rawPackets = Prelude.map Data.Maybe.fromJust (Prelude.map preparePacket (Prelude.map (\x -> Just x) datas))
            let lazyHashChunks = Prelude.reverse (Prelude.map SHA.bytestringDigest (Prelude.map SHA.sha256 (fmap Lazy.fromStrict rawPackets)))
            let strictHashChunks = Prelude.map Lazy.toStrict lazyHashChunks
            let dataPointers = Prelude.map DataPointer strictHashChunks
            balancedManifestTreeFromPointers rawContents (Name nc) numPointers dataPointers

-- stupid test function
testManifesTree :: [Message]
testManifesTree = do
    let seed = 100
    let number = 1
    let nmin = 3
    let nmax = 5
    let ncmin = 2
    let ncmax = 5
    let pmin = 16
    let pmax = 32

    let payloadStream = Prelude.take number (randomListOfByteArrays (pmin, pmax) number (mkStdGen seed))
    let contents = produceNamelessContents payloadStream

    balancedManifestTree ["abc","def"] 4096 contents

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
    case (createSimpleInterest s) of
        Nothing -> []
        Just (SimpleInterest msg) -> [Just (SimpleInterest msg)] ++ (produceInterests xs)
produceInterests [] = []

produceInterestPackets :: [[String]] -> [Maybe ByteString]
produceInterestPackets s = fmap preparePacket (produceInterests s)

produceContents :: [[String]] -> [[Word8]] -> [Maybe Content]
produceContents (n:ns) (p:ps) =
    case (createContent (Payload p) n) of
        Nothing -> []
        Just (SimpleContent msg) -> [Just (SimpleContent msg)] ++ (produceContents ns ps)
produceContents [] _ = []
produceContents _ [] = []

produceNamelessContents :: [[Word8]] -> [Content]
produceNamelessContents (p:ps) = [NamelessContent (Payload p)] ++ (produceNamelessContents ps)
produceNamelessContents [] = []

produceContentPackets :: [[String]] -> [[Word8]] -> [Maybe ByteString]
produceContentPackets names payloads = fmap preparePacket (produceContents names payloads)

producePacketPairs :: [[String]] -> [[Word8]] -> [(Maybe ByteString, Maybe ByteString)]
producePacketPairs n p = do
    let interests = produceInterestPackets n
    let contents = produceContentPackets n p
        in
            Prelude.zip interests contents





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
produceManifestPackets names datas = fmap preparePacket (produceManifests names datas)
