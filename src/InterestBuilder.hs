import CCNxPacketGenerator
import Generator
import Writer
import System.Environment
import System.Random
import Data.Bits
import Data.Word
import Data.ByteString
import Data.ByteString.Char8
import Data.List.Split

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
        nameString:keyIdString:hashString:rest -> do
            let interest = createInterest (Prelude.drop 1 (splitOn "/" nameString)) keyIdString hashString
                in
                    case preparePacket interest of
                        Nothing -> Prelude.putStrLn ""
                        Just wireFormat -> Data.ByteString.putStrLn wireFormat
        _ ->
            usage
