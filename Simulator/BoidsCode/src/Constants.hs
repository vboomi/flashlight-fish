module Constants where

-- FILE CONSTANTS

fileHeader = 0xABCD -- First 32 bits of the file structure.
numBoids = 100 -- Second, indicates the number of Boids to simulate.
numIterations = 500 -- Third, indicates the number of iterations to simulate.
padding = 0 -- Fourth 32 bits of the file structure.

-- GENERAL SIMULATION CONSTANTS

maxSpeed = 1.25 :: Double -- The maximum speed a Boid can have, in units per iteration.
minSpeed = 0.75 :: Double -- The minimum speed a Boid can have, in units per iteration.
maxAcceleration = 0.03 :: Double -- The maximum unweighted acceleration a Boid can have from a single Rule, in units per iteration^2.
deadConeRadius = 0.5 :: Double -- The radius, in units, of the cone behind each Boid in which it cannot see.
flashProbability = 0.5 :: Double -- The probability that any Boid will be flashing on a certain frame.

-- DISTANCES

cohereDistance = 2000.0 :: Double -- The distance over which Boids can be seen (while flashing) for purposes of coherence.
                                  -- Mediated visually
alignDistance = 10.0 :: Double -- The distance over which Boids can be seen (while flashing) for purposes of alignment.
                               -- Believed to be mediated in large part by the lateral line system.
separateDistance = 7.5 :: Double  -- The distance over which Boids can be seen (while flashing) for purposes of separation.
                                  -- Likely mediated by the lateral line system, vision, and turbulence.
separateDistanceInvisible = 5.0 :: Double  -- The distance over which Boids can be seen (while not flashing) for purposes of coherence.
                                           -- As above, but without vision

-- WEIGHTS
-- Weights are functions that take the current frame number, so they can change over time if need be.

cohereWeight :: Int -> Double
cohereWeight _ = 7.0

alignWeight :: Int -> Double
alignWeight frame
    | frame > 250 = 6.0
    | otherwise   = 0.0

separateWeight :: Int -> Double
separateWeight _ = 9.0

frictionWeight :: Int -> Double
frictionWeight _ = 1.05

motivateWeight :: Int -> Double
motivateWeight frame
    | frame > 250 = 0.0
    | otherwise   = 0.0