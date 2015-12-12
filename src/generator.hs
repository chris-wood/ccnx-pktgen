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
    Data.ByteString.Char8.appendFile f1 b1
    Data.ByteString.Char8.appendFile f1 (Data.ByteString.Char8.pack "\n")
    Data.ByteString.Char8.appendFile f2 b2
    Data.ByteString.Char8.appendFile f2 (Data.ByteString.Char8.pack "\n")
    bshowList f1 f2 (n + 1) bs
bshowList f1 f2 n [] = return ()

-- runhaskell generator.hs data 100 0 10

main :: IO ()
main = do
    (prefix:number:min:max:_) <- getArgs
    let interestFile = prefix ++ "_int"
    let contentFile = prefix ++ "_data"
    let
        pairs = producePairs (Prelude.zip (randomInts (read number) (read min) (read max)) (randomInts (read number) (read min) (read max)))
        in
            bshowList interestFile contentFile 0 pairs
