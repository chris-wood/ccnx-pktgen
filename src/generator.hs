module Generator (
    randomInt
    , randomInts
    , randomBytes
    , randomString
    , randomStrings
) where

import System.Environment
import System.Random
import System.IO.Unsafe
import Data.Word

randomIntStream :: (RandomGen g, Random a) => g -> [a]
randomIntStream gen = randoms gen

randomBytes :: Int -> Int -> [Word8]
randomBytes n seed = take n (randomIntStream seed :: [Word8])




randomInt :: (RandomGen g) => Int -> Int -> g -> (Int, g)
randomInt min max gen = randomR (min, max) gen
    --let (n, seedprime) = randomR (min, max) (mkStdGen seed)
    --in n

--randomInts :: Int -> Int -> [Int]
--randomInts n seed = take n (randomIntStream seed :: [Int])

_modSwap a b = mod b a
randomInts :: (RandomGen g) => (Int, Int) -> g -> [Int]
randomInts (low, high) gen = (Prelude.map (+ low) (Prelude.map (_modSwap (high - low)) (randoms gen)))





-- $ is function application
randomString :: (RandomGen g) => Int -> g -> String
randomString len gen = take len $ randomRs ('a','z') gen

_randomStringSwap :: (RandomGen g) => g -> Int
_randomStringSwap gen val = randomString val gen
randomStrings :: (RandomGen g) => (Int, Int) -> g -> [String]
randomStrings (low, high) gen = Prelude.map (_randomStringSwap gen) (randomInts (low, high) gen)

_randomStringsCall x y z n = take n (randomStrings x y z)
randomListOfStrings :: Int -> Int -> Int -> Int -> Int -> [[String]]
randomListOfStrings low high elow ehigh seed = Prelude.map (_randomStringsCall elow ehigh seed) (randomInts (low, high) seed)

--randomStrings :: Int -> Int -> Int -> Int -> Int -> [String]
--randomStrings :: [Int] -> Int -> [String]
--randomStrings sizes seed = [(randomString i seed) | i <- sizes]

--randomString :: Int -> Int -> String
--randomString n seed = take n (randomCharStream seed :: [Char])
-- take 10 <$> (randoms <$> newStdGen :: Random a => IO [a])
