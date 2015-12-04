import Parser
import System.Environment
import System.Random

-- Criterion benchmarking library
import Criterion.Main

-- criterion tutorial: http://www.serpentine.com/criterion/tutorial.html

randomIntStream :: (Random a) => Int -> [a]
randomIntStream seed = randoms (mkStdGen seed)

_modSwap a b = mod b a
randomInts :: Int -> Int -> Int -> [Int]
randomInts n low high = Prelude.take n (Prelude.map (+ low) (Prelude.map (_modSwap (high - low)) (randomIntStream 42)))

main = do
--    (low:high:args) <- getArgs
    

    -- TODO: command line input:
    -- step: step for benchmark
    -- max: max number to use for benchmark

    defaultMain $ [
        bgroup "interests" [
                bench "1"  $ whnf produceInterests (randomInts 1 1 10)
                , bench "50"  $ whnf produceInterests (randomInts 50 1 10)
                , bench "100" $ whnf produceInterests (randomInts 100 1 10)
            ]
        ]
