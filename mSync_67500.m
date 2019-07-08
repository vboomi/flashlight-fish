%%
load('data_67500/tracks.mat')

%% Vels

nId = max(tracks(:,4));
nFrms = max(tracks(:,3));
nPts = size(tracks,1);

velTracks = zeros(nPts,2); %[vel amp, vel dir (degrees)]
velBrownian = zeros(nPts,2);
bDir = (rand(1,nId)-0.5)*2*180;

for id=1:nId
    indx = find(tracks(:,4)==id);
    trId = tracks(indx,1:2);
    
    dtrId = diff(trId,1,1);
    if isempty(dtrId)
        dtrId = [0,0];
    else
        dtrId = [dtrId; dtrId(end,:)];
    end
    
    vAmp = sqrt(dtrId(:,1).^2 + dtrId(:,2).^2);
    vDir = atan2d(dtrId(:,2), dtrId(:,1));
    
    velTracks(indx,:) = [vAmp, vDir]; 
    
    velBrownian(indx,1) = vAmp;
%     bDir = (rand(1)-0.5)*2*180;
    velBrownian(indx,2) = bDir(id);
end
clearvars indx trId dtrId vAmp vDir bDir

%%
avgVelVec = zeros(nFrms,3); %[avg Am, avg Ang, numFish]
avgSpeed = zeros(nFrms,3);

avgVelB = zeros(nFrms,3);

for fm = 1:nFrms
    indx = find(tracks(:,3)==fm);
    lindx = length(indx);
    velSub = velTracks(indx,:);
    
    velVec = velSub(:,1).*exp(1i*deg2rad(velSub(:,2)));
    velVec = sum(velVec);
    
    avgVelVec(fm,1) = abs(velVec)/lindx;
    avgVelVec(fm,2) = rad2deg(angle(velVec));
    avgVelVec(fm,3) = lindx;
    
    avgSpeed(fm,1:2) = mean(velSub,1);
    avgSpeed(fm,3) = lindx;
    
    velB = velBrownian(indx,:);
    velB = velB(:,1).*exp(1i*deg2rad(velB(:,2)));
    velB = sum(velB);
    avgVelB(fm,1) = abs(velB)/lindx;
    avgVelB(fm,2) = rad2deg(angle(velB));
    avgVelB(fm,3) = lindx;
end
clearvars indx lindx velSub velVec velB

%% mSync
mSync = avgVelVec(:,1)./avgSpeed(:,1);
mSyncB = avgVelB(:,1)./avgSpeed(:,1);

%% Plot
rnge = 184:515; % 1:length(avgSpeed)
figure, subplot(3,1,1), plot(avgSpeed(rnge,1)); title 'Average Speed';
subplot(3,1,2), plot(mSync(rnge)), ylim([0,1]); title 'mSync for observed data';
subplot(3,1,3), plot(mSyncB(rnge)), ylim([0,1]);title 'mSync for random motion'
