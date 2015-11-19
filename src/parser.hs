import System.IO  
import Control.Monad
import Data.Bits
import Data.Word
import Data.ByteString
import Data.ByteString.Char8

-- import qualified Data.ByteString as B
-- TODO: clean up code and implement the second serialize function

class Serializer t where
    serialize :: t -> ByteString

class Encoder t where
    toTLV :: t -> TLV
    encodingSize :: t -> Int

data TwoByte = Type Word8 Word8 | Length Word8 Word8 deriving(Show) 

intToType :: Int -> TwoByte
intToType x = (Type (fromIntegral (x `shiftR` 8)) (fromIntegral x))
--intToType x = (Type (fromInteger ((shiftR x 8) :: Int) :: Word8) (fromInteger (x :: Int) :: Word8))
intToLength :: Int -> TwoByte
intToLength x = (Length (fromIntegral (x `shiftR` 8)) (fromIntegral x))
--intToLength x = (Length (fromInteger ((shiftR x 8) :: Int) :: Word8) (fromInteger (x :: Int) :: Word8))

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
        let bytelist = [(Data.ByteString.singleton t1), (Data.ByteString.singleton t2), (Data.ByteString.singleton l1), (Data.ByteString.singleton l2), v]
        in
        (Data.ByteString.concat bytelist)
    serialize (NestedTLV (Type t1 t2) (Length l1 l2) v) = Data.ByteString.Char8.pack " "

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

gen_name_component :: NameComponent
gen_name_component = (NameComponent "random_component")

gen_name :: [NameComponent] -> Int -> Name
gen_name nc n = 
    if n <= 1 then 
        Name (nc ++ [gen_name_component])
    else 
        gen_name (nc ++ [gen_name_component]) (n - 1)

name :: Int -> Name
name n = gen_name [] n
