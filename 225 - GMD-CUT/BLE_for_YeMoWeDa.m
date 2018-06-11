% This code can be used only to reproduce the forecasts submitted to the M4 competition 
% for the BLE method and yearly, monthly, weekly and daily time series
% Author: Grzegorz Dudek (GMD) - Czestochowa University of Technology, Poland
% Method Description: Bunch linear extrapolation


%% Select the data file:

%T = readtable('Yearly-train1.csv'); h = 6; m=1; file=('Yerly_wyn.mat');
%T = readtable('Monthly-train1.csv'); h = 18; m=1; file=('Month_wyn.mat');
%T = readtable('Weekly-train1.csv'); h = 13; m=1; file=('Week_wyn.mat');
T = readtable('Daily-train1.csv'); h = 14; m=1; file=('Dail_wyn.mat');

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
    
    xq = n+1:n+h;
    yq = nan(n-1,h); 
    yp = nan(n-1,n); 
    for i=1:n-1
        x2=[x(i) n];
        y2=[y(i) y(end)];
        p=polyfit(x2,y2,1);
        yq(i,:)=polyval(p,xq);
        yp(i,:)=polyval(p,x);
    end

    yq1 = median(yq);
    yp1 = median(yp);
    
    %PI
    alpha=0.05;
    crit = tinv(1-alpha/2,n-2);
    SSE = sum((yp1-y).^2);
    MSE = SSE/(n-2);
    s = MSE^0.5;
    xm = mean(x);
    xs = sum((x-xm).^2);
    dy = crit*s*(1+1/n+(xq-xm).^2/xs).^0.5;

    ypro(k,:) = yq1;  
    y025(k,:) = yq1-dy;
    y975(k,:) = yq1+dy;
    
end
%Saving time series label, forecasts (ypro), lower (y025) and upper (y975) bounds of prediction intervals  
save(file,'label','ypro','y025','y975');



