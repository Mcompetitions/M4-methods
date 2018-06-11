% for aa=1:20
r = randi([1 100000],1,100);
% r = randi([1 100000],1,3);

%   ww = randi([415 4641],1,100);
% j1=415+1210;
% j2=415+2112;
%     ww = randi([415+1282 415+2084],1,100);

%   ww = randi([415+1210 415+2112],1,100);


%  ww = randi([77001 78500],1,100);

% ww2 = randi([77001 100000],1,100);
% r=[ww ww2];
% ww=414+2070:414+2188;
% ww=414+1181:414+1210;
% ww=414+1210;
% ww2 = randi([77001:200:100000],1,100);
 %ww=[[1182 1185 1210 2084 2085]+415 ww] ;

%   r=ww;


rs=sort(r);

prog=nan(100,48);
% load hourly
defi=3;

T = readtable('Hourly-train1.csv'); h = 48;
     t=table2array(T(:,2:end));
     
     Hourly_train1 = readtable('Hourly-train1.csv'); 
     Daily_train1 = readtable('Daily-train1.csv'); 
     Weekly_train1 = readtable('Weekly-train1.csv'); 
     Monthly_train1 = readtable('Monthly-train1.csv'); 
     Quarterly_train1 = readtable('Quarterly-train1.csv'); 
     Yearly_train1 = readtable('Yearly-train1.csv'); 

     %testy
%      Yearly_train1 = readtable('Yearly-train1.csv'); 
%       t=table2array(T(:,2:end));
%      QQloo=fun_M4n(t(572,:), h,3);

%       t=table2array(Yearly_train1(:,2:end));
%       QQloo=fun_M4n(t(881,:), h,3);
     %
%      t=table2array(Daily_train1(2070,2:end));
%           t=table2array(Daily_train1(1182,2:end));



% t=table2array(Yearly_train1(1148,2:end));
% 
%      QQloo=fun_M4n(t, h,3);

%szukanie krotszych od 20 w yearly
% t=table2array(Yearly_train1(:,2:end));
% krotsze=isnan(t(:,19));
% krot_inx = find(krotsze(:,1));
% % rs=krot_inx(1:100:end)'+77001;
% rs=krot_inx(179:279)'+77000;

%   T = readtable('Hourly-train1.csv','Range','C2:E6');  
%comma2point_overwrite('bigdata.csv')

% folder='C:<path>Desktop';
% fid = fopen('Hourly-train1.csv');
% out = textscan(fid,'%s%f$f','delimiter',',');
% fclose(fid);

% data = csvread('Hourly-train1.csv',251,0,'B252..E352');

 %data = csvread('Hourly-train1.csv',0,0,'B2..B2');

%  data = dlmread('Hourly-train1.csv', ',', 251, 0, 'B252..D352');
% data = xlsread('file.csv', 'B252:D5352');

% F=nan(1,48);
F=nan(length(rs),48);
%  W=t(1,1:48);
% Wyn(1,1:h+1)=[T(1,1) array2table(F)];
Wyn(1:length(rs),1:h+1)=[T(1:length(rs),1) array2table(F)];


% load yearly
 %load yearly
 licznik=0;
 for ix=rs
     defi=3;
 %H1-414;D415-4641;W4642-W5000;M5001:M53000;Q53001-Q77000;Y77001-Y100000
 %ix=30;
 if ix<415
     % load hourly
     ix2=ix;
     nam='H';

%      T = readtable('Hourly-train1.csv');
     h = 48;
          T = Hourly_train1; 

     t=table2array(T(:,2:end));
     %mnale 1182-2084
 elseif ix>=415&&ix<4643
     %  load daily
     ix2=ix-415+1;
     nam='D';
     
%      T = readtable('Daily-train1.csv');
     h = 14;
               T = Daily_train1; 

     t=table2array(T(:,2:end));
    % if ix2>1181
%     if ix2>1210%
     if ix2>2084%||(ix2<2070&&ix2>2084)
     defi=1;
     end
     
 elseif ix>=4642&&ix<5001
     %    load weekly
     ix2=ix-4642+1;
     nam='W';
     
%      T = readtable('Weekly-train1.csv');
     h = 14;
               T = Weekly_train1; 

     t=table2array(T(:,2:end));
 elseif ix>=5001&&ix<53001
     %  load monthly
     ix2=ix-5001+1;
     nam='M';
     
%      T = readtable('Monthly-train1.csv');
     h = 18;
               T = Monthly_train1; 

     t=table2array(T(:,2:end));
 elseif ix>=53001&&ix<77001
     %      load quarterly
     ix2=ix-53001+1;
     nam='Q';
     
%      T = readtable('Quarterly-train1.csv');
     h = 8;
               T = Quarterly_train1; 

     t=table2array(T(:,2:end));
 elseif ix>=77001&&ix<100001
%      load yearly
     ix2=ix-77001+1;
     nam='Y';
     
%      T = readtable('Yearly-train1.csv'); 
     h = 6;
                    T = Yearly_train1; 

     t=table2array(T(:,2:end));
     %mnale do +/-846 zle licza
      if ix2>9011
     defi=1;
      end
     
     
     
 end
 
 QQloo=fun_M4nREPRO2(t(ix2,:), h,defi,ix);
 
 if ix>=77001&&ix<77001+1248
     %funkcja do reproduckci krotszych szeregow z Yearly z poczatku danych x>Yearly1247
%bo to bylo wykonane 2.05.2018, a wtedy bylo w tym przypadku(krotszych szeregow length(x<20)) (ls=10)
     QQloo=fun_M4nREPRO2(t(ix2,:), h,defi,ix);
 end
 
 a=0;
if sum(isnan(QQloox(1).Yp(dd,:).xpp))>0
    a=1;
end
if all(isnan(QQloox(1).Yp(dd,:).xpp),2)>0
    QQloo=fun_M4nREPRO2(t(ix2,:), h,1,ix);
end
licznik=licznik+1;
prog(licznik,1:h)= QQloox(1).Yp(dd,:).xpp;
Wyn(licznik,1:h+1)=[T(ix2,1) array2table(prog(licznik,1:h))];
 
 end
 

writetable(Wyn,'repro.csv')

save('results','Wyn')

Repro=readtable('repro.csv');
Results=readtable('results.csv');

%      Tx = readtable('Yearly-train1.csv','Range','A2:A4'); h = 6;


[tf, rowidx] = ismember(Repro.Var1, Results.id);
Res_repro=Results(rowidx,:);

a=table2array(Res_repro(:,2:end))-table2array(Repro(:,2:end));
aa=abs(((table2array(Res_repro(:,2:end))-table2array(Repro(:,2:end)))./table2array(Res_repro(:,2:end))))*100;
ar=[Res_repro(:,1) array2table(a)];
Rrp=table2array(Res_repro(:,2:end));
Rrp2=table2array(Repro(:,2:end));
inne = find(a(:,1));

figure
plot(Rrp(inne,:))
hold on 
plot(Rrp2(inne,:))

% end

% b=Results(Repro(:,1),:);
% b=Results({Repro(:,1)},:);
% b=Results(Repro.Var1,:);
% b=Results(Repro.Var1==Results.id,:);
% b=Results({Repro.Var1'},:);
% b=Results(intersect(A, B),:);
% Results(Repro.Var1 == 'DAL1448',:)
% A = table2array({Repro.Var1});
% B = table2array({Results.id});
% b=Results(intersect(A, B),:);
% [tf, rowidx] = ismember(A, B);

% Res_repro=table2cell(Res_repro);
% Res_repro(cellfun(@(elem) elem == 'NA', Res_repro)) = {'na'};



% Res_repro2=zeros(size(Res_repro,1),size(Res_repro,2));
% Res_repro2=str2double(Res_repro);
% asd=table2array(Res_repro(:,2:end));

%Results=readtable('results.csv');
%%Results=csvread('results.csv');
%a=Results(Repro(1,1));



% mcc -m execution_REPRO2.m%tworzenie exe
% mcc -m wywolanie_REPRO.m%tworzenie exe



 % ax=  find(all(isnan(monthly_prog),2));

% brak_h=  find(all(isnan(hourly_prog),2));
% brak_d=  find(all(isnan(daily_prog),2));
% brak_w=  find(all(isnan(weekly_prog),2));
% brak_m=  find(all(isnan(monthly_prog),2));
% brak_q=  find(all(isnan(quarterly_prog),2));
% brak_y=  find(all(isnan(yearly_prog),2));

% % Fx=table2array(results(:,2:end));
% % brak_all=  find(all(isnan(Fx),2));


% load daily

% for SZER=brak_d'
    
% QQloo=fun_M4(t(SZER,:), h)

% acx=['Daily_'   'sz' num2str(SZER)];
% save (acx, 'QQloox');

% load daily_prog

% %yearly_prog=[yearly_prog; QQloox(1).Yp(dd,:)];
% daily_prog(SZER,:)= QQloox(1).Yp(dd,:).xpp;

% save ('daily_prog', 'daily_prog');
% clear QQloo
% clear QQloox

% end

% load weekly

% for SZER=brak_w'
    
% QQloo=fun_M4(t(SZER,:), h)

% acx=['Weekly_'   'sz' num2str(SZER)];
% save (acx, 'QQloox');

% load weekly_prog

% %yearly_prog=[yearly_prog; QQloox(1).Yp(dd,:)];
% weekly_prog(SZER,:)= QQloox(1).Yp(dd,:).xpp;

% save ('weekly_prog', 'weekly_prog');
% clear QQloo
% clear QQloox

% end

% load monthly

% for SZER=brak_m'
    
% QQloo=fun_M4(t(SZER,:), h)

% acx=['Monthly_'   'sz' num2str(SZER)];
% save (acx, 'QQloox');

% load monthly_prog

% %yearly_prog=[yearly_prog; QQloox(1).Yp(dd,:)];
% monthly_prog(SZER,:)= QQloox(1).Yp(dd,:).xpp;

% save ('monthly_prog', 'monthly_prog');
% clear QQloo
% clear QQloox

% end

% load quarterly

% for SZER=brak_q'
    
% QQloo=fun_M4(t(SZER,:), h)

% acx=['Quarterly_'   'sz' num2str(SZER)];
% save (acx, 'QQloox');

% load quarterly_prog

% %yearly_prog=[yearly_prog; QQloox(1).Yp(dd,:)];
% quarterly_prog(SZER,:)= QQloox(1).Yp(dd,:).xpp;

% save ('quarterly_prog', 'quarterly_prog');
% clear QQloo
% clear QQloox

% end

% load yearly

% for SZER=brak_y'
    
% QQloo=fun_M4(t(SZER,:), h)

% acx=['Yearly_'   'sz' num2str(SZER)];
% save (acx, 'QQloox');

% load yearly_prog

% %yearly_prog=[yearly_prog; QQloox(1).Yp(dd,:)];
% yearly_prog(SZER,:)= QQloox(1).Yp(dd,:).xpp;

% save ('yearly_prog', 'yearly_prog');
% clear QQloo
% clear QQloox

% end