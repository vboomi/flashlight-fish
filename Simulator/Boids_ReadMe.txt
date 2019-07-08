
Rory O'Brien <rfo231@nyu.edu>

Here is a folder (BoidsCode) containing the source code (in Haskell) of the flashlight fish simulator program, as well as an executable for the viewer program. To rebuild the simulator, you will need to install Cabal, run "cabal update" on any command line, then run build.bat. The resulting executable will reside in the bin folder, alongside the viewer program and its dependencies.
Main.hs contains the main function, as well as stuff required to initialize and run the simulation. You should not need to worry about this file.

Output.hs contains the functions needed to output the simulation data in a format readable by the viewer. You should likewise not need to worry about this file.

Boid.hs contains the meat of the program: the definitions of the various rules that decide how flashlight fish interact. Rules take a single Boid, a frame number, and a list containing all Boids at the previous time-step. Thus, they may consider the state of the simulation as well as how many frames have passed, and can therefore change over time if you want to test something.

Constants.hs contains all the "constants" that determine the way the simulation evolves. The Weights, at the bottom of the file, are not actually necessarily constant - they are functions that take a frame number and return a double. Thus, they may or may not change over time.
