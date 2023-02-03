clear all
%used this file to do the annual and seasonal averages of nutrient loads and flow normalized nutrient loads calculated using weighted regressions on discharge season and time (WRTDS). See metadata for references 
[data,parms]=xlsread('C:\Users\nshall\OneDrive - University of North Carolina at Chapel Hill\Documents\Proposals\NCFisheriesCollaboratory\RiverLoading\DailyNeuseSFBTPloading.csv');%input nutrient loading data calculated using WRTDS in R, see script "NeuseNutrientLoads.R"


data(:,1)=[];
data(:,1)=data(:,1)-data(1,1)+datenum('1-Jan-1974');

dates=datevec(data(:,1));
mon=dates(:,2);
yrs=dates(:,1);
yr=unique(yrs);

ann=NaN*ones(length(yr),1);
win=NaN*ones(length(yr),1);
spr=NaN*ones(length(yr),1);
sum=NaN*ones(length(yr),1);
fal=NaN*ones(length(yr),1);
fnann=NaN*ones(length(yr),1);
fnwin=NaN*ones(length(yr),1);
fnspr=NaN*ones(length(yr),1);
fnsum=NaN*ones(length(yr),1);
fnfal=NaN*ones(length(yr),1);

for i=1:length(yr)
    ann(i)=nanmean(data(yrs==yr(i),2));
    win(i)=nanmean(data(yrs==yr(i) & mon<4,2));
    spr(i)=nanmean(data(yrs==yr(i) & mon>3 & mon<7,2));
    sum(i)=nanmean(data(yrs==yr(i) & mon>6 & mon<10,2));
    fal(i)=nanmean(data(yrs==yr(i) & mon>9,2));
    fnann(i)=nanmean(data(yrs==yr(i),5));
    fnwin(i)=nanmean(data(yrs==yr(i) & mon<4,5));
    fnspr(i)=nanmean(data(yrs==yr(i) & mon>3 & mon<7,5));
    fnsum(i)=nanmean(data(yrs==yr(i) & mon>6 & mon<10,5));
    fnfal(i)=nanmean(data(yrs==yr(i) & mon>9,5));
end;

loaddata=[yr ann win spr sum fal fnann fnwin fnspr fnsum fnfal];
success=xlswrite('C:\Users\nshall\OneDrive - University of North Carolina at Chapel Hill\Documents\Proposals\NCFisheriesCollaboratory\RiverLoading\P1974Neuse_TPload_nh.xlsx',loaddata);%exported the data as an excel file, added column headers manually and resaved as csv
