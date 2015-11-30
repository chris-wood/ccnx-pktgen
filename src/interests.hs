import Parser
import System.Environment

-- Criterion benchmarking library
import Criterion.Main

-- criterion tutorial: http://www.serpentine.com/criterion/tutorial.html

main = do
    args <- getArgs
    putStrLn $ (read $ head args :: Int)

    -- TODO: command line input:
    -- step: step for benchmark
    -- max: max number to use for benchmark

    defaultMain $ [
        bgroup "interests" [
                bench "1"  $ whnf produceInterests [1]
                , bench "5"  $ whnf produceInterests [1,2,3,4,5]
            ]
        ]
