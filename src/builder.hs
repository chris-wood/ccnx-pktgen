import Parser
import Generator
import Writer
import System.Environment
import System.Random
import Data.Bits
import Data.Word
import Data.ByteString
import Data.ByteString.Char8

main :: IO ()
main = do
    args <- getArgs
    case args of
        (prefix:seed_s:number_s:nmin_s:nmax_s:ncmin_s:ncmax_s:pmin_s:pmax_s:_) -> do
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

            -- TODO: make a function to create this list of payloads
            --let payloadStream = Prelude.take number (randomBytes (randomInt (pmin, pmax) (mkStdGen seed)) (mkStdGen seed))

            let pairs = producePacketPairs nameStream payloadStream
                in writeByteStringPairs interestFile contentFile pairs
