module Generator (
    randomInt
    , randomIntStreamFromGenerator
    , randomIntListFromGenerator
    , randomIntStream
    , randomIntList
    , randomBytes
    , randomStringFromGenerator
    , randomStringStreamFromGenerator
    , randomStringStream
    , randomStreamOfStringStreams
    , randomListOfStringStreams
) where

import System.Environment
import System.Random
import System.IO.Unsafe
import Data.Word

generateRandomIntStream :: (RandomGen g, Random a) => g -> [a]
--generateRandomIntStream gen = randoms gen
generateRandomIntStream gen = do
    let (a, g) = random gen
        in [a] ++ (generateRandomIntStream g)

randomBytes :: Int -> Int -> [Word8]
randomBytes n seed = take n (generateRandomIntStream (mkStdGen seed) :: [Word8])

randomInt :: (RandomGen g) => Int -> Int -> g -> (Int, g)
randomInt min max gen = randomR (min, max) gen

_modSwap a b = mod b a
randomIntStreamFromGenerator :: (RandomGen g) => (Int, Int) -> g -> [Int]
randomIntStreamFromGenerator (low, high) gen = (Prelude.map (+ low) (Prelude.map (_modSwap (high - low)) (randoms gen)))

randomIntListFromGenerator :: (RandomGen g) => (Int, Int) -> Int -> g -> [Int]
randomIntListFromGenerator (low, high) n gen = take n (randomIntStreamFromGenerator (low, high) gen)

randomIntStream (low, high) = randomIntStreamFromGenerator (low, high)
randomIntList (low, high) n = randomIntListFromGenerator (low, high) n

-- note: $ is function application
randomStringFromGenerator :: (RandomGen g) => Int -> g -> String
randomStringFromGenerator len gen = take len $ randomRs ('a','z') gen

_randomStringSwap :: (RandomGen g) => g -> Int -> String
_randomStringSwap gen val = randomStringFromGenerator val gen
randomStringStreamFromGenerator :: (RandomGen g) => (Int, Int) -> g -> [String]
randomStringStreamFromGenerator (low, high) gen = Prelude.map (_randomStringSwap gen) (randomIntStreamFromGenerator (low, high) gen)

randomStringListFromGenerator :: (RandomGen g) => (Int, Int) -> Int -> g -> [String]
randomStringListFromGenerator (low, high) n gen = take n (randomStringStreamFromGenerator (low, high) gen)

randomStringStream (low, high) = randomStringStreamFromGenerator (low, high)
randomStringList (low, high) n = randomStringListFromGenerator (low, high) n

_randomListOfStringsMap :: (RandomGen g) => (Int, Int) -> g -> Int -> [String]
_randomListOfStringsMap (low, high) g n = randomStringList (low, high) n g

randomStreamOfStringStreams :: (RandomGen g) => (Int, Int) -> (Int, Int) -> g -> [[String]]
randomStreamOfStringStreams (low, high) (elow, ehigh) g = Prelude.map (_randomListOfStringsMap (elow, ehigh) g) (randomIntStream (low, high) g)

randomListOfStringStreams :: (RandomGen g) => (Int, Int) -> (Int, Int) -> Int -> g -> [[String]]
randomListOfStringStreams (low, high) (elow, ehigh) n g = take n (randomStreamOfStringStreams (low, high) (elow, ehigh) g)
