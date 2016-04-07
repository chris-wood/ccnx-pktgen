import Parser
import Generator
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
        (prefix:seed:number:nmin:nmax:ncmin:ncmax:pmin:pmax:_) -> do
            let interestFile = prefix ++ "_int"
            let contentFile = prefix ++ "_data"

            let nameLengthStream = XXX
            let payloadStream = XXX
            let pairs = producePairs nameLengthStream payloadStream
            in
                writeByteStringPairs interestFile contentFile pairs
            
