
library(EGRET)

filePath<-"/Users/nshall/OneDrive - University of North Carolina at Chapel Hill/Documents/Proposals/NCFisheriesCollaboratory/RiverLoading/"

#input river flow data
fileName<-"FortBarnwellFlow1974_2022.csv"
Daily <-readUserDaily(filePath,fileName)
INFO <- readNWISInfo("", "")

#input properly formatted nutrient concentration data (see Hirsch, R.M and L.A. DiCicco. 2015. User Guide #to Exploration and Graphics for RivEr Trends (EGRET) and dataRetrieval: R Packages for Hydrologic # #Data Chapter 10 of Section A, Statistical Analysis Book 4, Hydrologic Analysis and Interpretation. USGS #2015)
filePath<-"/Users/nshall/OneDrive - University of North Carolina at Chapel Hill/Documents/Proposals/NCFisheriesCollaboratory/RiverLoading/"
fileName<-"StreetsFerryTP_1974_2022.csv"
Sample<-readUserSample(filePath,fileName)

Daily <- subset(Daily, Date > "1974-01-01" & Date<"2020-12-31")#add the date range for the analyses, note #that data for the Roanoke River only went back to 1982. All other rivers had data from 1974 to 2020
Sample <- subset(Sample, Date > "1974-01-01" & Date<"2020-12-31")

eList <- mergeReport(INFO,Daily,Sample)
eList<-setPA(eList,paStart=1,paLong=12)
eList<-modelEstimation(eList,windowY=7,minNumObs=50,minNumUnc=50)


dt<-eList$Daily$Julian
fx<-eList$Daily$FluxDay
cn<-eList$Daily$ConcDay
qs<-eList$Daily$Q
fnfx<-eList$Daily$FNFlux
fncn<-eList$Daily$FNConc

myData<-cbind(dt,fx,cn,qs,fnfx,fncn)
setwd("C://Users/nshall/OneDrive - University of North Carolina at Chapel Hill/Documents/Proposals/NCFisheriesCollaboratory/RiverLoading/")
write.csv(myData, file = "DailyNeuseSFBTPloading.csv")





































plotFluxHist(eList,fluxMax=0.08)


