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
    Prelude.putStrLn "usage: runhaskell ContentBuilder.hs <name> <hash> <keyid>"
    Prelude.putStrLn ""
    Prelude.putStrLn "    name     = data name"
    Prelude.putStrLn "    hash     = data hash"
    Prelude.putStrLn "    keyid    = data key ID"
    Prelude.putStrLn ""

main :: IO ()
main = do
    args <- getArgs
    case args of
        nameString:hashString:keyidString -> do
            --- TODO           

        -- case for "nil" in place of keyid
        
        _ ->
            usage
