%%
load('data_67500/ptCoords.mat')

%%
ptsMat = cell2mat(ptsCell);
ptsMat = ptsMat(:,[1,2,5]); % extract [x, y, frameNum]

%%
maxDisp = 20;
tracks = particleTrack(ptsMat,maxDisp); %[x, y, frameNum, ptId]