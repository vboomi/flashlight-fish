-- Output: methods to output simulation data to a file.
module Output (writeOut) where

import Data.Binary.Put
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as Bin
import System.IO
import Data.Vector.Class
import Data.Vector.V3
import System.FilePath
import Data.Csv
import Boid
import Constants

-- Write out the result of the simulation to a user-provided file.
writeOut :: [[Boid]] -> IO ()
writeOut sim = do
    outFile <- getOutFile
    putStrLn "Working..."
    hBoidOut <- openFile outFile WriteMode
    Bin.hPut hBoidOut . runPut . putWord32le $ fromIntegral fileHeader
    Bin.hPut hBoidOut . runPut . putWord32le $ fromIntegral numBoids
    Bin.hPut hBoidOut . runPut . putWord32le $ fromIntegral numIterations
    Bin.hPut hBoidOut . runPut . putWord32le $ fromIntegral padding
    takePut sim (fromIntegral numIterations) hBoidOut
    hClose hBoidOut
    Bin.writeFile ((dropExtension outFile) <.> "csv") $ encode $ zip (map mSync sim) (map averageSpeed sim)
    putStrLn "Done."

-- Calculates the mSync at a particular frame given the state at that frame.
mSync :: [Boid] -> Double
mSync boids = (averageVelocity boids) / (averageSpeed boids)

-- Calculates the average speed at a particular frame given the state at that frame.
averageSpeed :: [Boid] -> Double
averageSpeed boids = (sum $ map vmag $ map velocity boids) / (fromIntegral $ length boids)

-- Calculates the average velocity at a particular frame given the state at that frame.
averageVelocity :: [Boid] -> Double
averageVelocity boids = let vecs = map velocity boids
                        in vmag $ (|/) (sum vecs) (fromIntegral $ length vecs)

-- Puts the simulation into the file in the format expected by the parser.
takePut :: [[Boid]] -> Int -> Handle -> IO ()
takePut sim i hOut = do
    mapM (Bin.hPut hOut) $ map runPut $ concat . map putBoid $ head sim
    if i > 0 then takePut (tail sim) (i - 1) hOut else return ()
    where putBoid boid = (map putDoublele $ vunpack $ position boid) ++
                         (map putDoublele $ vunpack $ velocity boid) ++
                         ((putWord64le $ if flash boid then 1 else 0) : [])

-- Prompts the user for the name of the output file and returns it.
getOutFile :: IO String
getOutFile = do
    putStr "Enter the output file: "
    hFlush stdout
    getLine