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

bshowList :: String -> Int -> [Data.ByteString.ByteString] -> IO ()
bshowList f n (b:bs) = do
--    Data.ByteString.Char8.writeFile (f ++ (show n)) b
    Data.ByteString.Char8.appendFile f b
    Data.ByteString.Char8.appendFile f (Data.ByteString.Char8.pack "\n")
    bshowList f (n + 1) bs
bshowList f n [] = return ()

-- runhaskell generator.hs interest_ 100 0 10 
main :: IO ()
main = do
    (prefix:num:min:max:_) <- getArgs
    let interestStream = produceInterests (randomInts (read num) (read min) (read max)) in
        bshowList prefix 0 interestStream
    
    

