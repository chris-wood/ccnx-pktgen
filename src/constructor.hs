import Parser
import System.Environment
import System.Random
import Data.Bits
import Data.Word
import Data.ByteString
import Data.ByteString.Char8

randomIntStream :: (Random a) => Int -> [a]
randomIntStream seed = randoms (mkStdGen seed)

_modSwap a b = mod b a
randomInts :: Int -> Int -> Int -> [Int]
randomInts n low high = Prelude.take n (Prelude.map (+ low) (Prelude.map (_modSwap (high - low)) (randomIntStream 42)))

bshow :: Data.ByteString.ByteString -> IO ()
bshow s = (Data.ByteString.Char8.putStrLn s)

bwrite :: String -> ByteString -> IO ()
bwrite f s = (Data.ByteString.writeFile f s)

bshowList :: String -> String -> Int -> [(ByteString,ByteString)] -> IO ()
bshowList f1 f2 n ((b1,b2):bs) = do
--    Data.ByteString.Char8.writeFile (f ++ (show n)) b
    Prelude.putStrLn ((showString "Writing number: " . shows (n)) [])
    Data.ByteString.Char8.appendFile f1 b1
    Data.ByteString.Char8.appendFile f2 b2
    bshowList f1 f2 (n + 1) bs
bshowList f1 f2 n [] = return ()

-- runhaskell generator.hs data 42 100 2 10 1 10 256 4096
-- runhaskell generator.hs data 42 1 2 3 1 10 10 20

usage :: IO ()
usage = do
    Prelude.putStrLn ""
    Prelude.putStrLn "usage: runhaskell generator.hs prefix:seed:number:nmin:nmax:ncmin:ncmax:pmin:pmax"
    Prelude.putStrLn ""
    Prelude.putStrLn "    prefix   = file prefix for output pair files"
    Prelude.putStrLn "    seed     = PRNG seed"
    Prelude.putStrLn "    number   = number of packet pairs to generate"
    Prelude.putStrLn "    nmin     = minimum name length (in segments)"
    Prelude.putStrLn "    nmax     = maximum name length (in segments)"
    Prelude.putStrLn "    ncmin    = minimum name segment length (in octets)"
    Prelude.putStrLn "    ncmax    = maximum name segment length (in octets)"
    Prelude.putStrLn "    pmin     = minimum payload size"
    Prelude.putStrLn "    pmax     = maximum payload size"
    Prelude.putStrLn ""

main :: IO ()
main = do
        args <- getArgs
        case args of
            (prefix:seed:number:nmin:nmax:ncmin:ncmax:pmin:pmax:_) -> do
                let interestFile = prefix ++ "_int"
                let contentFile = prefix ++ "_data"

                let nameLengthStream = (randomInts (read number) (read nmin) (read nmax))
                let payloadLengthStream = (randomInts (read number) (read pmin) (read pmax))
                let
                    pairs = producePairs (Prelude.zip nameLengthStream payloadLengthStream) (mkStdGen (read seed))
                    in
                        bshowList interestFile contentFile 0 pairs
            ("repl":_) -> do
                Prelude.putStrLn "TODO"
            _ ->
                usage
