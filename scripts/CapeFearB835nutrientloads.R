
library(EGRET)

filePath<-"/Users/nshall/OneDrive - University of North Carolina at Chapel Hill/Documents/Proposals/NCFisheriesCollaboratory/RiverLoading/"
fileName<-"CapeFearKellyFlow1969_2023.csv"
Daily <-readUserDaily(filePath,fileName)
INFO <- readNWISInfo("", "")

filePath<-"/Users/nshall/OneDrive - University of North Carolina at Chapel Hill/Documents/Proposals/NCFisheriesCollaboratory/RiverLoading/"
fileName<-"B835TN.csv"
#fileName<-"B835TP.csv"
Sample<-readUserSample(filePath,fileName)

Daily <- subset(Daily, Date > "1974-01-01" & Date<"2020-12-31")
Sample <- subset(Sample, Date > "1974-01-01" & Date<"2020-12-31")

eList <- mergeReport(INFO,Daily,Sample)
eList<-setPA(eList,paStart=1,paLong=12)
eList<-modelEstimation(eList,windowY=7,minNumObs=50,minNumUnc=50)



dt<-eList$Daily$Julian
fx<-eList$Daily$FluxDay
cn<-eList$Daily$ConcDay
qs<-eList$Daily$Q

myData<-cbind(dt,fx,cn,qs)
setwd("C://Users/nshall/OneDrive - University of North Carolina at Chapel Hill/Documents/Proposals/NCFisheriesCollaboratory/RiverLoading/")
write.csv(myData, file = "DailyB835TN.csv")
#write.csv(myData, file = "DailyB835TP.csv")

AnnualResults<-setupYears(eList$Daily,paLong=12,paStart=1)
yr<-AnnualResults$DecYear
flw<-AnnualResults$Q
cnc<-AnnualResults$Conc
flx<-AnnualResults$Flux
fncn<-AnnualResults$FNConc
fnflx<-AnnualResults$FNFlux

myData<-cbind(yr,flw,cnc,flx,fncn,fnflx)
setwd("C://Users/nshall/OneDrive - University of North Carolina at Chapel Hill/Documents/Proposals/NCFisheriesCollaboratory/RiverLoading/")
write.csv(myData, file = "AnnualB835TN.csv")
#write.csv(myData, file = "AnnualB835TP.csv")




































plotFluxHist(eList,fluxMax=0.08)


