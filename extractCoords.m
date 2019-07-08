%%
load('data_67500/imgMean.mat');
fname = 'data_67500/example_frame.tif';

%%
warning('off', 'Images:initSize:adjustingMag');

img = imread(fname);
img = uint16(single(img)-imgMean);
    
%%
bw = img>12;
%% Extract the fish location and the fish stats
ii = 1;
numFishPerFrame = 100;

arThresh = 15;

bw = bwmorph(bw,'clean');
stats = regionprops(bw,'Centroid','Area','PixelIdxList','BoundingBox');
ars = [stats.Area];
idx = find(ars>=arThresh,numFishPerFrame);
pts = reshape([stats(idx).Centroid],2,[])';


npts = length(idx);
pts3D = cat(2,pts,zeros(npts,3));
for pp = 1:npts
    pts3D(pp,3) = mean(img(stats(idx(pp)).PixelIdxList));
    pts3D(pp,4) = stats(idx(pp)).Area;
    pts3D(pp,5) = ii;
end

ptsCell{ii} = pts3D; %[x, y, meanIntensity, area, frameNum]

stats = stats(idx);
statsCell{ii} = stats;

%% Create image with only fish (no noise)
pts3D = ptsCell{ii};
npts = size(pts3D,1);
stats = statsCell{ii};

mask = zeros(2048,2048);
for pp = 1:npts
    mask(stats(pp).PixelIdxList) = pts3D(pp,3);
end

figure, imshow(mask,[]);