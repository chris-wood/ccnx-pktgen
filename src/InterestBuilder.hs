import CCNxPacketGenerator
import Generator
import Writer
import System.Environment
import System.Random
import Data.Bits
import Data.Word
import Data.ByteString
import Data.ByteString.Char8

-- runhaskell builder.hs content output 100 1 3 5 2 5 256 4096
-- runhaskell builder.hs manifest output 100 1 3 5 2 5 256 4096

usage :: IO ()
usage = do
    Prelude.putStrLn ""
    Prelude.putStrLn "usage: runhaskell InterestBuilder.hs <name> <keyid> <hash>"
    Prelude.putStrLn ""
    Prelude.putStrLn "    name     = data name"
    Prelude.putStrLn "    keyid    = data key ID"
    Prelude.putStrLn "    hash     = data hash"
    Prelude.putStrLn ""

main :: IO ()
main = do
    args <- getArgs
    case args of
        nameString:hashString:keyIdString -> do
            -- XXX: need to split nameString up by '/' character
            let interest = createInterest nameString keyIdString hashString
            let packet = preparePacket interest
                in
                    print packet
        _ ->
            usage
