clear all
%used this script to calculate the annual and seasonal average flows for
%all rivers
[data,parms]=xlsread('C:\Users\nshall\OneDrive - University of North Carolina at Chapel Hill\Documents\Proposals\NCFisheriesCollaboratory\RiverLoading\DailyNeuseSFBTPloading.csv');


data(:,1)=[];
data(:,1)=data(:,1)-data(1,1)+datenum('1-Jan-1974');

dates=datevec(data(:,1));
mon=dates(:,2);
yrs=dates(:,1);
yr=unique(yrs);

ann=NaN*ones(length(yr),1);;
win=NaN*ones(length(yr),1);;
spr=NaN*ones(length(yr),1);;
sum=NaN*ones(length(yr),1);;
fal=NaN*ones(length(yr),1);;

for i=1:length(yr)
    ann(i)=nanmean(data(yrs==yr(i),4));
    win(i)=nanmean(data(yrs==yr(i) & mon<4,4));
    spr(i)=nanmean(data(yrs==yr(i) & mon>3 & mon<7,4));
    sum(i)=nanmean(data(yrs==yr(i) & mon>6 & mon<10,4));
    fal(i)=nanmean(data(yrs==yr(i) & mon>9,4));
    
end;

flowdata=[yr ann win spr sum fal];

success=xlswrite('C:\Users\nshall\OneDrive - University of North Carolina at Chapel Hill\Documents\Proposals\NCFisheriesCollaboratory\RiverLoading\P1974Neuse_TPload_nh.xlsx',flowdata);%exported the data as an excel file, added column headers manually and resaved as csv

