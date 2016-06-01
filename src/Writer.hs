module Writer (
    printByteString
    , writeByteString
    , writeByteStringPairs
) where

import Data.ByteString
import Data.ByteString.Char8

printByteString :: Maybe ByteString -> IO ()
printByteString (Just s) = (Data.ByteString.Char8.putStrLn s)
printByteString Nothing = error "Can't display nothing"

writeByteString :: String -> Maybe ByteString -> IO ()
writeByteString f (Just s) = Data.ByteString.writeFile f s
writeByteString f Nothing =  error "Can't write nothing"

writeByteStringPairs :: String -> String -> [(Maybe ByteString, Maybe ByteString)] -> IO ()
writeByteStringPairs f1 f2 (((Just b1), (Just b2)):bs) = do
    Data.ByteString.Char8.appendFile f1 b1
    Data.ByteString.Char8.appendFile f2 b2
    writeByteStringPairs f1 f2 bs
writeByteStringPairs f1 f2 ((_, _):bs) = writeByteStringPairs f1 f2 bs
writeByteStringPairs f1 f2 [] = return ()
