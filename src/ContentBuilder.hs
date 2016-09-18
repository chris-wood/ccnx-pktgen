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
    Prelude.putStrLn "usage: runhaskell ContentBuilder.hs <name> <data size>"
    Prelude.putStrLn ""
    Prelude.putStrLn "    name      = data name, e.g., /hello/world"
    Prelude.putStrLn "    data size = integer size of the data to be put in the packet"
    Prelude.putStrLn ""

main :: IO ()
main = do
    args <- getArgs
    case args of
        nameString:dataSize:rest -> do
            let payload = createPayload (randomByteArray (read dataSize) (mkStdGen 42))
            let content = createContent payload (Prelude.drop 1 (splitOn "/" nameString)) 
                in
                    case preparePacket content of
                        Nothing -> Prelude.putStrLn ""
                        Just wireFormat -> Data.ByteString.Char8.putStrLn wireFormat
        _ ->
            usage
