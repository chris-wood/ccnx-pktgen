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

            -- TODO: this is broken.
            let nameStream = take number (take (randomInt nmin nmax seed) (randomStrings ncmin ncmax seed))
            let payloadStream = take number (randomBytes (randomInt pmin pmax seed) seed)

            let pairs = producePacketPairs nameStream payloadStream
            in
                writeByteStringPairs interestFile contentFile pairs
