import System.IO  
import Control.Monad

class ByteArray t where
    encode :: t -> [Int]

class Encoder t where
    toTLV :: t -> TLV

data TLV = TLV { tlv_type :: Int,
                 tlv_length :: Int,
                 tlv_value :: [Int] } deriving (Show) 

-- TODO: want to generalize the toTLV encoding function

data NameComponent = NameComponent {value :: String} deriving (Show)
instance Encoder NameComponent where
    toTLV (NameComponent value) = TLV {tlv_type=1, tlv_length=1,tlv_value=[1]}
--toTLV (NameComponent nc) = TLV {tlv_type = 1, tlv_length=1, tlv_value=(value nc)}

data Name = Name {components :: [NameComponent]} deriving (Show)
instance Encoder Name where
    toTLV (Name components) = TLV {tlv_type=1, tlv_length=1,tlv_value=[1]}
--toTLV (Name n) = TLV {tlv_type=1, tlv_length=1, tlv_value=[1]}

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
