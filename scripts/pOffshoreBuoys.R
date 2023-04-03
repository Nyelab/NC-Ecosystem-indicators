library(tidyverse)
library(data.table)
library(stringr)
library(sf)
library(sp)
library(spatialEco)
library(rnaturalearth)
library(scales)
library(ggpmisc)
library(RColorBrewer)
library(padr)

beauf <- read.csv("G:/My Drive/NCTempProject/bestdatasets/beaufdaily")
beauf$id <- "beauf"
capefear <- read.csv("G:/My Drive/NCTempProject/bestdatasets/capefeardaily")
capefear$id <- "capefear"
duck <- read.csv("G:/My Drive/NCTempProject/bestdatasets/duckdaily")
duck$id <- "duck"
hatteras <- read.csv("G:/My Drive/NCTempProject/bestdatasets/hatterasdaily")
hatteras$id <- "hatteras"
oregon <- read.csv("G:/My Drive/NCTempProject/bestdatasets/oregondaily")
oregon$id <- "oregon"
wrightsville <- read.csv("G:/My Drive/NCTempProject/bestdatasets/wrightsvilledaily")
wrightsville$id <- "wrightsville"
buoylist <- list(beauf, capefear, duck, hatteras, oregon, wrightsville)
buoylabels <- c("beauf", "capefear", "duck", "hatteras", "oregon", "wrightsville")
buoylist <- list(beauf, capefear, duck, hatteras, oregon, wrightsville)

buoydf <- do.call(rbind.data.frame, buoylist)
buoydf$date <- as.Date(buoydf$date)
buoydf$month <- as.numeric(format(buoydf$date, "%m"))
buoydf$year <- as.numeric(format(buoydf$date, "%Y"))
# year <- rep(1996:2021, each = 12)
# year <- as.data.frame(year)
# year$month <- rep(1:12, times = 26)
year <- rep(1996:2021, each = 4)
year <- as.data.frame(year)
year$season <- rep(c("winter", "spring", "summer", "fall"), times = 26)

fillin <- merge(buoydf, year)
season <- c("winter", "winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "fall", "fall", "fall")
season <- as.data.frame(season)
season$month <- 1:12

buoydf <- merge(buoydf, season, by = "month", all.x = TRUE)
buoydf <- buoydf %>% thicken('season') %>% group_by(season)
buoydf <- pad(buoydf)
buoyseason <- buoydf %>% group_by(id, year, season) %>% summarise(mean = mean(temp), sd = sd(temp), latitude = mean(latitude), longitude = mean(longitude), sample = n())
write.csv(buoyseason, "~/NC-Ecosystem-indicators/data/p1995OffshoreWaterCB.csv")
