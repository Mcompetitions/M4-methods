% This code can be used only to reproduce the forecasts submitted to the M4 competition for
% the BLE method and quarterly and hourly time series
% Author: Grzegorz Dudek (GMD) - Czestochowa University of Technology, Poland
% Method Description: Bunch linear extrapolation


%% Select the data file

%T = readtable('Quarterly-train1.csv'); h = 8; m=4; file=('Quart_wyn.mat');
T = readtable('Hourly-train1.csv'); h = 48; m=24; file=('Hourl_wyn.mat');

% Comment: Data files are the same as original ones but without quotation marks
% around numerical values

%% Model creation

lsz = size(T,1);
N=10;

ypro = nan(lsz,h);
y025 = ypro;
y975 = ypro;

label = T{:,1};
for k=1:lsz
    
    data = T{k,2:end};
    q = ~isnan(data);
    y = data(q);
    
    n=length(y);
    if n > N*h
        n=N*h;
        y=y(1:n);
    end
    
    x=1:n;
    x1 = n+1:n+h;
    
    for j=1:m
        yj = y(end+j-m:-m:1);
        yj = yj(end:-1:1);
        nj=length(yj);
        xj=1:nj;
    
        xq = [nj+1 nj+2];
        yq = nan(nj-1,2);  
        yp = nan(nj-1,nj); 
        for i=1:nj-1
            x2=[xj(i) nj];
            y2=[yj(i) yj(end)]; 
            p=polyfit(x2,y2,1);
            yq(i,:)=polyval(p,xq);
            yp(i,:)=polyval(p,xj);
        end
    
        yq1([j,j+m]) = median(yq);
        yp1 = median(yp);
        
        %PI
        alpha=0.05;
        crit = tinv(1-alpha/2,nj-2);
        SSE = sum((yp1-yj).^2);
        MSE = SSE/(nj-2);
        s = MSE^0.5;
        xm = mean(xj);
        xs = sum((xj-xm).^2);
        dy([j,j+m]) = crit*s*(1+1/nj+(xq-xm).^2/xs).^0.5;
    end

    ypro(k,:) = yq1;  
    y025(k,:) = yq1-dy;
    y975(k,:) = yq1+dy;
end
%Saving time series label, forecasts (ypro), lower (y025) and upper (y975) bounds of prediction intervals
save(file,'label','ypro','y025','y975');