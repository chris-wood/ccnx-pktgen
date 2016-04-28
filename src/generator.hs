module Generator (
    randomInt
    , randomIntStreamFromGenerator
    , randomIntListFromGenerator
    , randomIntStream
    , randomIntList
    , randomStringFromGenerator
    , randomStringStreamFromGenerator
    , randomStringStream
    , randomStreamOfStringStreams
    , randomListOfStringStreams
    , randomBytes
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

randomBytes :: (RandomGen g) => Int -> g -> [Word8]
randomBytes n gen = take n (generateRandomIntStream gen :: [Word8])

randomInt :: (RandomGen g) => (Int, Int) -> g -> (Int, g)
randomInt (min, max) gen = randomR (min, max) gen

randomIntStreamFromGenerator :: (RandomGen g) => (Int, Int) -> g -> [Int]
randomIntStreamFromGenerator (low, high) gen =
    let (n, nextGen) = randomInt (low, high) gen
        in [n] ++ (randomIntStreamFromGenerator (low, high) nextGen)

--_modSwap a b = mod b a
--(Prelude.map (+ low) (Prelude.map (_modSwap (high - low)) (randoms gen)))

randomIntListFromGenerator :: (RandomGen g) => (Int, Int) -> Int -> g -> [Int]
randomIntListFromGenerator (low, high) n gen
    | n == 0 = []
    | otherwise = do
        let (toss, nextGen) = randomInt (low, high) gen
        let nextString = take 1 (randomIntStreamFromGenerator (low, high) gen)
            in nextString ++ (randomIntListFromGenerator (low, high) (n - 1) nextGen)

randomIntStream (low, high) = randomIntStreamFromGenerator (low, high)
randomIntList (low, high) n = randomIntListFromGenerator (low, high) n

randomCharStreamFromGenerator :: (RandomGen g) => g -> [Char]
randomCharStreamFromGenerator gen =
    let (c, g) = randomR ('a', 'z') gen
        in [c] ++ (randomCharStreamFromGenerator g)

randomStringFromGenerator :: (RandomGen g) => Int -> g -> String
randomStringFromGenerator len gen = take len (randomCharStreamFromGenerator gen)

_randomStringSwap :: (RandomGen g) => g -> Int -> String
_randomStringSwap gen val = randomStringFromGenerator val gen
randomStringStreamFromGenerator :: (RandomGen g) => (Int, Int) -> g -> [String]
randomStringStreamFromGenerator (low, high) gen = Prelude.map (_randomStringSwap gen) (randomIntStreamFromGenerator (low, high) gen)

randomStringListFromGenerator :: (RandomGen g) => (Int, Int) -> Int -> g -> [String]
randomStringListFromGenerator (low, high) n gen
    | n == 0 = []
    | otherwise = do
        let (toss, nextGen) = randomInt (low, high) gen
        let nextString = take 1 (randomStringStreamFromGenerator (low, high) gen)
            in nextString ++ (randomStringListFromGenerator (low, high) (n - 1) nextGen)

randomStringStream (low, high) = randomStringStreamFromGenerator (low, high)
randomStringList (low, high) n = randomStringListFromGenerator (low, high) n

_randomListOfStringsMap :: (RandomGen g) => (Int, Int) -> g -> Int -> [String]
_randomListOfStringsMap (low, high) g n = randomStringList (low, high) n g

randomStreamOfStringStreams :: (RandomGen g) => (Int, Int) -> (Int, Int) -> g -> [[String]]
randomStreamOfStringStreams (low, high) (elow, ehigh) g = do
    Prelude.map (_randomListOfStringsMap (elow, ehigh) g) (randomIntStream (low, high) g)

randomListOfStringStreams :: (RandomGen g) => (Int, Int) -> (Int, Int) -> Int -> g -> [[String]]
randomListOfStringStreams (low, high) (elow, ehigh) n g
    | n == 0 = []
    | otherwise = do
        let (toss, nextGen) = randomInt (low, high) g
        let nextStringList = take 1 (randomStreamOfStringStreams (low, high) (elow, ehigh) g)
            in nextStringList ++ (randomListOfStringStreams (low, high) (elow, ehigh) (n - 1) nextGen)
