-- Rules, definitions, etc. for the Boid type.
module Boid(Boid(..), update, initBoid) where

import Data.List (partition)
import Data.Vector.Class
import Data.Vector.V3
import Constants

-- EXPORTS

-- The Boid data type, which is operated on.
data Boid = Boid { position :: Vector3, velocity :: Vector3, flash :: Bool }

-- Update a given Boid (this) based on the previous state of the simulation and a random double in [0.0, 1.0].
update :: Int -> [Boid] -> Boid -> Double -> Boid
update frame allBoids this random = apply acceleration random
    where mappedRules = map (\r -> r frame allBoids this) rules
          mappedWeights = map (\w -> w frame) weights
          weightedRules = mappedRules `scale` mappedWeights
          scale = zipWith vmult
          acceleration = sum weightedRules
          apply acceleration random = Boid { position = (position this) + (enforceSpeedBounds $ velocity this) + acceleration,
                                             velocity = (enforceSpeedBounds $ velocity this) + acceleration,
                                             flash = random < flashProbability }

-- Initialize a Boid, so we don't have to export stuff.
initBoid :: (Double, Double, Double) -> (Double, Double, Double) -> Double -> Boid
initBoid (posx, posy, posz) (velx, vely, velz) random = Boid { position = Vector3 posx posy posz,
                                                               velocity = Vector3 velx vely velz,
                                                               flash = random < flashProbability }

-- RULES & WEIGHTS

-- A rule takes the previous state of the simulation and the Boid it is currently being applied to 
-- and returns an unweighted acceleration to be applied to that Boid.
type Rule = Int -> [Boid] -> Boid -> Vector3

-- The rules that apply to the Boids.
rules :: [Rule]
rules = [cohere, align, separate, friction, motivate]

-- The relative weight function for each rule. Defined in Constants.hs
weights :: [Int -> Double]
weights = [cohereWeight, alignWeight, separateWeight, frictionWeight, motivateWeight]

-- Boids tend to move towards the center of mass of nearby Boids.
cohere :: Rule
cohere _ boids this
    | null seenBoids = zeroVec
    | otherwise = seek this $ vavg $ map position seenBoids
    where seenBoids = filter (this `canSee`) $ filter (\b -> (distance (position this) (position b)) < cohereDistance) boids

-- Boids tend to align themselves to the average direction of movement of nearby Boids.
align :: Rule
align _ boids this
    | null seenBoids = zeroVec
    | otherwise = steer (vavg $ map velocity seenBoids) $ velocity this
    where seenBoids = filter (this `canSee`) $ filter (\b -> (distance (position this) (position b)) < alignDistance) boids

-- Boids tend to keep a distance from very nearby Boids.
separate :: Rule
separate _ boids this
    | null seenBoids = zeroVec
    | otherwise = steer (vavg $ map sepSteer seenBoids) $ velocity this
    where seenPair = Data.List.partition (this `canSee`) boids
          seenBoids = (filter (\b -> (distance (position this) (position b)) < separateDistance) $ fst seenPair) ++
                      (filter (\b -> (distance (position this) (position b)) < separateDistanceInvisible) $ snd seenPair)
          sepSteer :: Boid -> Vector3
          sepSteer boid = (vnormalise $ difference (position this) (position boid)) `vdiv` (distance (position this) (position boid))

-- Boids are affected by friction, modelled as a simple resistive force.
friction :: Rule
friction _ boids this = vflip (limitAcceleration $ velocity this)

-- Boids may be motivated to go in some direction.
motivate :: Rule
motivate frame boids this
    | frame > 400 = (Vector3 (-1.0) 0.0 0.0) `vmult` maxAcceleration
    | frame > 300 = (Vector3    0.0 0.0 1.0) `vmult` maxAcceleration
    | frame > 200 = (Vector3    0.0 1.0 0.0) `vmult` maxAcceleration
    | frame > 100 = (Vector3    1.0 0.0 0.0) `vmult` maxAcceleration
    | otherwise   = (Vector3    0.0 0.0 0.0) `vmult` maxAcceleration

-- HELPER FUNCTIONS

-- A 3-Vector representing a speed of 0.
zeroVec :: Vector3
zeroVec = Vector3 0.0 0.0 0.0

-- Takes a boid and returns the acceleration it would have towards the given target position.
seek :: Boid -> Vector3 -> Vector3
seek boid target = let desired = (vnormalise $ target `vsub` (position boid)) `vmult` maxSpeed
                   in limitAcceleration $ desired `vsub` (velocity boid)

-- Takes two velocities and finds the acceleration that would be used to bring the first in line with the second.
steer :: Vector3 -> Vector3 -> Vector3
steer v vel = limitAcceleration (((vnormalise v) `vmult` maxSpeed) `vsub` vel)

-- Limits the given acceleration vector to maxAcceleration.
limitAcceleration :: Vector3 -> Vector3
limitAcceleration acceleration
    | vmag acceleration < maxAcceleration = acceleration
    | otherwise = (vnormalise acceleration) `vmult` maxAcceleration

-- Converts a given velocity so that it is between minSpeed and maxSpeed.
enforceSpeedBounds :: Vector3 -> Vector3
enforceSpeedBounds velocity
    | mag < minSpeed = (vnormalise velocity) `vmult` minSpeed
    | mag > maxSpeed = (vnormalise velocity) `vmult` minSpeed
    | otherwise      = velocity
    where mag = vmag velocity

-- Takes two Boids and determines whether the first can see the second.
canSee :: Boid -> Boid -> Bool
canSee this other = let proj = difference (position other) (position this)
                        back = vnormalise . vflip $ velocity this
                        bdist = vdot proj back
                        crad = bdist * deadConeRadius
                        orth = vmag $ proj + (vflip $ back `vmult` bdist)
                    in (flash other) && (orth > crad)

vflip :: Vector3 -> Vector3
vflip vec = (|*) vec (-1.0)

vmult :: Vector3 -> Double -> Vector3
vmult vec scale = (|*) vec scale

vdiv :: Vector3 -> Double -> Vector3
vdiv vec scale
    | scale == 0 = Vector3 0.0 0.0 0.0
    | otherwise = (|/) vec scale

vsub :: Vector3 -> Vector3 -> Vector3
vsub v1 v2 = v1 + (vflip v2)

vavg :: [Vector3] -> Vector3
vavg [] = error "Empty list"
vavg vecs = (sum vecs) `vdiv` (fromIntegral $ length vecs)

difference :: Vector3 -> Vector3 -> Vector3
difference v1 v2 = v1 `vsub` v2

distance :: Vector3 -> Vector3 -> Double
distance v1 v2 = vmag $ difference v1 v2
