module Main (main) where

import System.Random
import Constants
import Boid
import Output

main :: IO ()
main = do
    randGen <- newStdGen
    let sim = doSim randGen
    writeOut sim

-- Initialize and perform the simulation, returning the iterations as a [[Boid]]
-- Time taken is proportional to the square of the number of Boids, and to the number of steps.
doSim :: StdGen -> [[Boid]]
doSim randGen = let (initBoids, nextRandGen) = initializeBoids randGen numBoids
                    rngs = makeRngs nextRandGen numBoids
                    rands = pregenRands rngs numIterations
                    frames = [0..(length rands)]
                in scanl3 (\b r f -> zipWith (update f b) b r) initBoids rands frames

-- A left-scan taking 3 arguments instead of 2.
scanl3 :: (a -> b -> c -> a) -> a -> [b] -> [c] -> [a]
scanl3 f q xs ys = q : (case xs of
                            [] -> []
                            x:xs -> (case ys of
                                        [] -> []
                                        y:ys -> scanl3 f (f q x y) xs ys))

-- Create a list of randomly-distributed Boids with random initial flashing states.
-- Return the random number generator when done for further use.
initializeBoids :: StdGen -> Integer -> ([Boid], StdGen)
initializeBoids gen 0 = ([], gen)
initializeBoids gen i = let (pos, posGen) = randomPos gen
                            (rand, randGen) = random gen :: (Double, StdGen)
                            (rest, restGen) = initializeBoids randGen (i - 1)
                        in ((initBoid pos (0.0, 0.0, 0.0) rand) : rest, restGen)

-- Generate a random position within the cube whose sides are from [-10, 10] in each dimension.
-- Return the random number generator when done for further use.
randomPos :: StdGen -> ((Double, Double, Double), StdGen)
randomPos gen = let range = (-10.0, 10.0) :: (Double, Double)
                    (x, xGen) = randomR range gen
                    (y, yGen) = randomR range xGen
                    (z, zGen) = randomR range yGen
                in ((x, y, z), zGen)

-- Split off i random number generators from prev, return as a list of length i.
makeRngs :: StdGen -> Integer -> [StdGen]
makeRngs _ 0 = []
makeRngs prev i = let next = (fst . split) prev
                  in next : makeRngs next (i - 1)

-- Pre-generate a list of lists of random doubles in [0.0, 1.0], of length i.
-- The inner lists will be of the same length as gens.
-- No need to return the generator, as it's not used again.
pregenRands :: [StdGen] -> Integer -> [[Double]]
pregenRands _ 0 = [[]]
pregenRands gens i = let (rands, nextGens) = unzip $ map random gens
                     in rands : pregenRands nextGens (i - 1)