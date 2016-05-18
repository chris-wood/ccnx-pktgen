import Parser
import Generator
import Writer
import System.Environment
import System.Random
import Data.Bits
import Data.Word
import Data.ByteString
import Data.ByteString.Char8

-- runhaskell builder.hs output 100 10 3 5 2 5 256 4096

usage :: IO ()
usage = do
    Prelude.putStrLn ""
    Prelude.putStrLn "usage: runhaskell builder.hs (content | manifest) <prefix> <seed> <number> <nmin> <nmax> <ncmin> <ncmax> <pmin> <pmax>"
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
        ("content":prefix:seed_s:number_s:nmin_s:nmax_s:ncmin_s:ncmax_s:pmin_s:pmax_s:_) -> do
            let interestFile = prefix ++ "_interests.bin"
            let contentFile = prefix ++ "_contents.bin"

            -- Convert the string arguments to integers
            let seed = read seed_s
            let number = read number_s
            let nmin = read nmin_s
            let nmax = read nmax_s
            let ncmin = read ncmin_s
            let ncmax = read ncmax_s
            let pmin = read pmin_s
            let pmax = read pmax_s

            let nameStream = randomListOfStringStreams (nmin, nmax) (ncmin, ncmax) number (mkStdGen seed)
            let payloadStream = randomListOfByteArrays (pmin, pmax) number (mkStdGen seed)

            let pairs = producePacketPairs nameStream payloadStream
                in writeByteStringPairs interestFile contentFile pairs

        ("manifests":prefix:seed_s:number_s:nmin_s:nmax_s:ncmin_s:ncmax_s:pmin_s:pmax_s:_) -> do
            return ()
        _ ->
            usage
