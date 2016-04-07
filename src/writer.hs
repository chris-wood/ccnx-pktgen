module Writer (
    printByteString
    , writeByteString
    , writeByteStringPairs
) where

printByteString :: Maybe ByteString -> IO ()
printByteString (Just s) = (Data.ByteString.Char8.putStrLn s)
printByteString Nothing = error "Can't display nothing"

writeByteString :: String -> Maybe ByteString -> IO ()
writeByteString f (Just s) = Data.ByteString.writeFile f s
writeByteString f Nothing =  error "Can't write nothing"

writeByteStringPairs :: String -> String -> Int -> [(ByteString,ByteString)] -> IO ()
writeByteStringPairs f1 f2 ((b1,b2):bs) = do
    Data.ByteString.Char8.appendFile f1 b1
    Data.ByteString.Char8.appendFile f2 b2
    writeByteStringPairs f1 f2 bs
writeByteStringPairs f1 f2 [] = return ()
