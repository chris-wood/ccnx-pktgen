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
    (nmin:nmax:dmin:dmax:_) <- getArgs

    withArgs [] $ defaultMain $ [
            bgroup "contents" [
                bench "10" $ nf produceContents (zip
                        (randomInts 10 (read nmin) (read nmax))
                        (randomInts 10 (read dmin) (read dmax))
                    )
                , bench "100" $ nf produceContents (zip
                        (randomInts 100 (read nmin) (read nmax))
                        (randomInts 100 (read dmin) (read dmax))
                    )
                , bench "1000" $ nf produceContents (zip
                        (randomInts 1000 (read nmin) (read nmax))
                        (randomInts 1000 (read dmin) (read dmax))
                    )
            ]
        ]
