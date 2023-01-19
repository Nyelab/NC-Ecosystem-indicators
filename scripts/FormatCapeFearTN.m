clear all

data=xlsread('C:/Users/nshall/OneDrive - University of North Carolina at Chapel Hill/Documents/Proposals/NCFisheriesCollaboratory/RiverLoading/B835TNunformatted');
dates=unique(floor(data(:,1)));
data(data(:,2)>3,:)=[];
data(data(:,2)<2,:)=[];

dta=NaN*ones(length(dates),5);
dta(:,1)=dates;
for i=1:length(dates)
    dta(i,3)=nanmean(data(floor(data(:,1))==dates(i)& data(:,2)==2,4));
    
    dta(i,5)=nanmean(data(floor(data(:,1))==dates(i)&data(:,2)==3,4));
    dta(i,2)=nanmean(data(floor(data(:,1))==dates(i)& data(:,2)==2,3));
    dta(i,4)=nanmean(data(floor(data(:,1))==dates(i)& data(:,2)==3,3));
end;

