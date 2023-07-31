#!/c/Program Files/R/R-4.2.2/bin/Rscript.exe

library(dplyr)
library(tidyverse)
library(ggplot2)
library(chron)

require(sf)
shape <- sf::read_sf(file.path("~/NC-Ecosystem-indicators/data/shapefiles/rivers.shp"))
shape <- st_transform(shape, crs = 4326)
setwd("~/NC-Ecosystem-indicators/data")

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
library(ggmap)
library(rstatix)
library(gridExtra)
library(gdata)

#load data
stations <- read.csv("G:/My Drive/NCTempProject/NC DEQ Ambient Monitoring System (estuarine stations only)/station.csv")
stations <- dplyr::select(stations, OrganizationFormalName, MonitoringLocationIdentifier, MonitoringLocationName, LatitudeMeasure, LongitudeMeasure)
names(stations) <- c("org", "LocationID", "name", "lat", "long")
tempsold <- read.csv("G:/My Drive/NCTempProject/NC DEQ Ambient Monitoring System (estuarine stations only)/narrowresult.csv")
tempsold <- temps %>% filter(name %in% c("Temperature, water", "pH", "Dissolved oxygen (DO)",  "Salinity"))
tempsold  <- dplyr::select(temps,  MonitoringLocationIdentifier, ActivityStartDate, ActivityStartTime.Time, ActivityStartTime.TimeZoneCode, ActivityIdentifier, CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode)
temps <- read.csv("G:/My Drive/NCTempProject/NC DEQ Ambient Monitoring System (estuarine stations only)/deqcontinuousstations.csv")
temps <- dplyr::select(temps, LocationID, lat, long, date, month, year, name.x, measure, unit)
names(temps) <- c("LocationID", "lat", "long", "date", "month", "year", "name", "measure", "unit")
# temps$datetime <- paste(temps$date, temps$time, sep = " ")
# df <- left_join(temps, stations, by = "LocationID")
# depth <- read.csv("G:/My Drive/NCTempProject/NC DEQ Ambient Monitoring System (estuarine stations only)/activityall.csv")
# depth <- dplyr::select(depth, ActivityStartDate, ActivityStartTime.Time, ActivityStartTime.TimeZoneCode, ActivityDepthHeightMeasure.MeasureValue, ActivityDepthHeightMeasure.MeasureUnitCode, ActivityIdentifier, MonitoringLocationIdentifier, ActivityCommentText, ActivityLocation.LatitudeMeasure, ActivityLocation.LongitudeMeasure)
# names(depth) <- c("date", "time", "timezone", "depth", "unit", "activityID", "locationID", "notes", "latitude", "longitude")
season <- c("winter", "winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "fall", "fall", "fall")
season <- as.data.frame(season)
season$month <- 1:12

df <- temps




#try depth
# depth$date <- as.POSIXct(depth$date, format ="%Y-%m-%d")
# depth$time <- times(depth$time)
# depth$datetime <- as.POSIXct(paste(depth$date, depth$time), format = "%Y-%m-%d %H:%M:%S")
# trydepth <- dplyr::select(depth, activityID, depth, datetime, timezone, time, latitude, longitude)
# colnames(trydepth) <- paste(colnames(trydepth),"depth",sep="_")
# trydepth$datetime_depth <- as.character(trydepth$datetime_depth)
# trydepth$depth_depth <- as.numeric(trydepth$depth_depth)
# colnames(trydepth)[1] <- c("activityID")
# colnames(trydepth)[3] <- c("datetime")
# colnames(trydepth)[6:7] <- c("lat", "long")
# names(trydepth)

#pH 
tryph <- filter(df, name %in% "pH")
colnames(tryph) <- paste(colnames(tryph),"pH",sep="_")
colnames(tryph)[1] <- c("LocationID")
colnames(tryph)[4] <- c("date")
colnames(tryph)[2:3] <- c("lat", "long")
tryph$measure_pH <- as.numeric(tryph$measure_pH)


#Dissolved Oxygen
trydo <- filter(df, name %in% "Dissolved oxygen (DO)")
colnames(trydo) <- paste(colnames(trydo),"do",sep="_")
colnames(trydo)[1] <- c("LocationID")
colnames(trydo)[4] <- c("date")
colnames(trydo)[2:3] <- c("lat", "long")
trydo$measure_do <- as.numeric(trydo$measure_do)


#Salinity
trysal <- filter(df, name %in% "Salinity")
colnames(trysal) <- paste(colnames(trysal),"sal",sep="_")
colnames(trysal)[1] <- c("LocationID")
colnames(trysal)[4] <- c("date")
colnames(trysal)[2:3] <- c("lat", "long")
trysal$measure_sal <- as.numeric(trysal$measure_sal)
newsal <- trysal %>% filter(unit_sal == "g/l") 
trysal <- anti_join(trysal, newsal, by="unit_sal")
# trysal$measure_sal <- apply(trysal$measure_sal, 1, function(x) {ifelse(any(x == 0), NA, length(unique(x)))})

#Temperature
trytemp <- filter(df, name %in% "Temperature, water")
colnames(trytemp) <- paste(colnames(trytemp),"temp",sep="_")
colnames(trytemp)[1] <- c("LocationID")
colnames(trytemp)[4] <- c("date")
colnames(trytemp)[2:3] <- c("lat", "long")
trytemp$measure_temp <- as.numeric(trytemp$measure_temp)

#put all data frames into list
trydf_list <- list(trysal, trytemp, trydo, tryph)
label <- c("salinity", "temperature", "dissolvedoxygen", "pH")

#sampling frequency
df <- trydf_list[[j]]
colnames(df)[8] <- c("measure")
df <- df %>% group_by(LocationID, lat, long, date) %>% mutate(mean = mean(measure))
label <- labels[i]
df$date <- as.Date(df$date)
df$year <- format(df$date,"%Y")
df$month <- format(df$date,"%m")
# df <- transform(df, Cluster_ID = as.numeric(interaction(latitude, longitude, drop=TRUE)))
# df <- df %>% dplyr::select(year, month, Cluster_ID)
# df <- table(df)
# df <- as.data.frame(df)
# df <- df %>% mutate(presence = ifelse(Freq > 0, 1, 0))
df$month <- as.numeric(as.character(df$month))
df$year <- as.numeric(as.character(df$year))
# df$Cluster_ID <- as.numeric(df$Cluster_ID)
df <- unique(df)
ggplot(df) + geom_point(aes(x = month, y = LocationID, color = measure)) + facet_wrap(~year) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ggtitle("DEQ Salinity Sampling Frequency")


##subsetting data spatially
#get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

#checkit
ggplot(data = world) + geom_sf() +geom_sf(data = shape, fill = "blue") +
  coord_sf(xlim=c(-80, -70), ylim=c(30,40), expand = TRUE) + theme(panel.background = element_rect(fill = "white", colour = "black"))

for(j in 1:length(trydf_list)) {
  df <- trydf_list[[j]]
  colnames(df)[8] <- c("measure")
  df <- df %>% group_by(LocationID, lat, long, date) %>% mutate(mean = mean(measure))
  
  #no depth
  df$date <- as.Date(df$date)
  df$year <- format(df$date,"%Y")
  df$year <- as.numeric(as.character(df$year))
  df$month <- format(df$date,"%m")
  df$month <- as.numeric(as.character(df$month))
  df <- df %>% dplyr::select(date, month, year, lat, long, mean, LocationID)
  names(df) <- c("date", "month", "year", "latitude", "longitude", "mean", "LocationID")
  
  #intersect spatially
  coordinates(df) <- ~ longitude + latitude
  df <- st_as_sf(df)
  
  df <- st_set_crs(df, 4326)
  grid <- shape
  grid <- st_set_crs(grid, 4326)
  
  #spatial intersections? 
  df_extract <- st_intersection(df, grid)
  df_extract_2 <- as.data.frame(df_extract)
  
  #pretty picture
  df_extract_point <- df_extract_2$geometry
  plot(st_geometry(df_extract_point))
  
  
  df_extract_2$gridID <- df_extract_2$Class
  df_extract_2$coords <- do.call(rbind, st_geometry(df_extract_point)) %>% 
    as_tibble() %>% setNames(c("longitude","latitude"))
  df_extract_2 <- unnest(df_extract_2)
  df_extract_2_2 <- df_extract_2[!duplicated(df_extract_2[c("date", "longitude","latitude")]),] 
  
  
  ggplot(data = world) + geom_sf() + geom_sf(data = shape)  + geom_point(data = df_extract_2_2, aes(x = longitude, y = latitude, color = gridID), size = 0.8) +
    coord_sf(xlim=c(-80, -74), ylim=c(32,37), expand = TRUE) + theme(panel.background = element_rect(fill = "white", colour = "black")) + ggtitle(paste( "Subsetted Points")) + ggtitle(paste("Indicators Subset")) + xlab("Longitude") + ylab("Latitude") 
  
  
  
  locations <- unique(df_extract_2_2$gridID)
  
  allvariables <- list()
  p <- list()
  for (i in 1:length(locations)) {
    #select by region
    df <- df_extract_2_2 %>% dplyr::select(date, month, year, mean, gridID, geometry, longitude, latitude) %>% filter(gridID == locations[i])
    #plot map
    ggplot(data = world) + geom_sf() + geom_sf(data = shape) + geom_point(data = df, aes(x = longitude, y = latitude), color = "blue") +
      coord_sf(xlim=c(-78, -75.5), ylim=c(33.6,36.5), expand = TRUE) + theme(panel.background = element_rect(fill = "white", colour = "black")) + ggtitle(paste( "Subsetted Points")) + ggtitle(paste(locations[i], "Subset")) + xlab("Longitude") + ylab("Latitude")
    #merge by season
    df <- merge(df, season, by = "month", all.x = TRUE)
    hist(df$mean)
    df_outliers <- df %>% identify_outliers(mean) 
    df <- df %>% anti_join(df_outliers, by = c("date", "latitude", "longitude"))
    
    #summarize by year and season
    finaldf <- df %>% group_by(year, season) %>% summarise(mean = mean(mean, na.rm = TRUE), samplesize = n()) %>% ungroup()
    finaldf$id <- locations[i]
    names(finaldf)[names(finaldf) == 'mean'] <- paste("mean",label[j],sep="_")
    names(finaldf)[names(finaldf) == 'samplesize'] <- paste("samplesize",label[j],sep="_")
    allvariables[[i]] <- finaldf
    
    #plot time series
    colnames(finaldf)[3] <- c("mean")
    ts_df <- finaldf %>% dplyr::select(mean)
    ts_df <- ts(ts_df, frequency = 4, start = min(finaldf$year))
    # png(paste0("G:/My Drive/NCTempProject/NC DEQ Ambient Monitoring System (estuarine stations only)/", locations[i], label[j],  "timeseriesDEQ.png"))
    plot.ts(ts_df) + title(main = paste(locations[i], label[j]), adj = 1)
    # dev.off()
    p[[i]] <- plot.ts(ts_df) + title(main = paste(locations[i], label[j]), adj = 1)
  }
  # do.call(grid.arrange, p)
  final <- do.call("rbind", allvariables)
  assign(label[j], final)
}
print("hi")
allvariables <- list(salinity, temperature, dissolvedoxygen, pH)
finalallvariables <- Reduce(function(x, y) merge(x, y, by = c("year", "season", "id"), all = TRUE), allvariables)


#yay
annualallvariables <- finalallvariables %>% group_by(year, id) %>% summarize(mean_temperature = mean(mean_temperature, na.rm = TRUE), mean_salinity = mean(mean_salinity, na.rm = TRUE), mean_dissolvedoxygen = mean(mean_dissolvedoxygen, na.rm = TRUE),  mean_pH = mean(mean_pH, na.rm = TRUE))


for (i in 1:length(locations)) {
  df1 <- annualallvariables %>% dplyr::filter(id == locations[i]) %>% dplyr::select(year, mean_salinity)
  names(df1) <- c("year", paste0(locations[i], "dissolvedoxygen"))
  
  write.csv(df1, paste0("~/NC-Ecosystem-indicators/data/finalized/p", min(df1$year), locations[i], "salinityCB_finalized.csv"), row.names = FALSE)
}

for (i in 1:length(locations)) {
    df1 <- finalallvariables %>% dplyr::filter(id == locations[i] & season == "fall") %>% dplyr::select(year, mean_dissolvedoxygen)
    names(df1) <- c("year", paste0(locations[i], "dissolvedoxygen"))
    
    write.csv(df1, paste0("~/NC-Ecosystem-indicators/data/finalized/p", min(df1$year), locations[i], "DOCB_fall_finalized.csv"), row.names = FALSE)
}
