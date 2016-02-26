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
    Prelude.putStrLn ((showString "Writing number: " . shows (n)) [])
    Data.ByteString.Char8.appendFile f1 b1
    Data.ByteString.Char8.appendFile f2 b2
    bshowList f1 f2 (n + 1) bs
bshowList f1 f2 n [] = return ()

-- TODO: need to write the functions that generate name lengths and payload lengths based on some procedure
-- 

main :: IO ()
main = do
    let interests = produceInterests [2] [["foo", "bar"], ["hello", "world"]] in
        print interests
