module Generator (
    randomInts
    , randomBytes
    , randomString
    , randomStrings
) where

import Parser
import System.Environment
import System.Random
import System.IO.Unsafe
import Data.Word

randomIntStream :: (Random a) => Int -> [a]
randomIntStream seed = randoms (mkStdGen seed)

randomInts :: Int -> Int -> [Int]
randomInts n seed = take n (randomIntStream seed :: [Int])

randomBytes :: Int -> Int -> [Word8]
randomBytes n seed = take n (randomIntStream seed :: [Word8])

-- $ is function application
randomString :: Int -> Int -> String
randomString n seed = take n $ randomRs ('a','z') $ mkStdGen seed

randomStrings :: [Int] -> Int -> [String]
randomStrings sizes seed = [(randomString i seed) | i <- sizes]

--randomString :: Int -> Int -> String
--randomString n seed = take n (randomCharStream seed :: [Char])
-- take 10 <$> (randoms <$> newStdGen :: Random a => IO [a])
