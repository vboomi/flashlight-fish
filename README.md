# Flashlight-fish

Code set analyzing flashlight fish video.

## Dataset folders

data_67500 - used to generate Fig. S2 of paper <br />
data_97200 - used to generate Fig. 3 of paper

*/example_frame.tif - An example frame from the dataset <br />
*/imgMean.mat - mean of all frames to get noise statistics <br />
*/ptCoords.mat - extracted coordinates of all fish in all the frames <br />
*/tracks.mat - tracking data of all fish in all the frames <br />

All frames of data_67500 is available in - DOI

## MATLAB Codes

The main files to plot mSync are: <br />
main_mSync_67500.m - Generates plot used in Fig. S2 <br />
main_mSync_97200.m - Generates plot, subset of which is used in Fig. 3

Above files use already extracted tracking data - data_*/tracks.mat

Example processing files: <br />
extractCoords.m - Extracts the coordinates of fish from an example frame <br />
trackFish.m - Tracks the fish IDs from the extracted coordinates - data_*/ptCoords.mat

Support file:
particleTrack.m - more info on http://www.physics.emory.edu/faculty/weeks//idl/ <br />
Reference: "Methods of Digital Video Microscopy for Colloidal Studies", John C. Crocker and David G. Grier, J. Colloid Interface Sci. 179, 298 (1996)

## Simulator

ReadMe at Simulator/Boids_ReadMe.txt

## Paper title

Bioluminescent Flashes Drive Nighttime Schooling Behavior and Synchronized Swimming Dynamics in Flashlight Fish

## Authors

David F. Gruber, Brennan T. Phillips, Rory O’Brien, Vivek Boominathan, Ashok Veeraraghavan, Ganesh Vasan, Peter O’Brien, Vincent A. Pieribone and John S. Sparks 

David F. Gruber - Baruch College, City University of New York, Department of Natural Sciences, New York, NY 10010, USA 

David F. Gruber - City University of New York The Graduate Center Program in Biology, New York, NY 10016, USA 

Brennan T. Phillips - University of Rhode Island, Department of Ocean Engineering, Narragansett, RI 02882, USA

Rory O’Brien, Ganesh Vasan, Peter O’Brien, Vincent A. Pieribone - The John B. Pierce Laboratory Cellular and Molecular Physiology Yale University School of Medicine, New Haven, CT 06519, USA 

Vivek Boominathan, Ashok Veeraraghavan - Rice University, Department of Electrical and Computer Engineering, Houston, Texas, USA

John S. Sparks - American Museum of Natural History, Division of Vertebrate Zoology, Department of Ichthyology, New York, NY 10024, USA 

Correspondence: David.Gruber@Baruch.cuny.edu
