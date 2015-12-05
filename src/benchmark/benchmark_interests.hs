import Parser
import System.Environment
import System.Random

-- Criterion benchmarking library
import Criterion.Main

randomIntStream :: (Random a) => Int -> [a]
randomIntStream seed = randoms (mkStdGen seed)

_modSwap a b = mod b a
randomInts :: Int -> Int -> Int -> [Int]
randomInts n low high = Prelude.take n (Prelude.map (+ low) (Prelude.map (_modSwap (high - low)) (randomIntStream 42)))

main = do
    (min:max:_) <- getArgs

    withArgs [] $ defaultMain $ [
            bgroup "interests" [
                bench "10" $ nf produceInterests (randomInts 10 (read min) (read max))
                , bench "100" $ nf produceInterests (randomInts 100 (read min) (read max))
                , bench "1000" $ nf produceInterests (randomInts 1000 (read min) (read max))
            ]
        ]
