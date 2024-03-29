#subset by SAV regions
#set your working directory from github (should be NC-Ecosystem-indicators)
require(sf)
shape <- sf::read_sf(file.path("data/shapefiles/SAVRegions.shp"))
shape <- st_transform(shape, crs = 4326)


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
standard_error <- function(x) sd(x) / sqrt(length(x))

#get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

#checkit
ggplot(data = world) + geom_sf() +geom_sf(data = shape, fill = "blue") +
  coord_sf(xlim=c(-80, -70), ylim=c(30,40), expand = TRUE) + theme(panel.background = element_rect(fill = "white", colour = "black"))


#load temp data
df <- read.csv("G:/My Drive/NCTempProject/P195/P195_1214.csv")
names(df) <- tolower(names(df))
df$date <- as.Date(df$date, format = "%m-%d-%Y")
df <- df %>% dplyr::select(date, tempsurface, salinitysurface, sdo, latitudestart, longitudestart)
names(df) <- c("date", "temp", "surfacesalinity", "surfaceDO", "latitude", "longitude")
df <- df[!is.na(df$latitude),]
df <- df[!is.na(df$longitude),]
#idk what this does
coordinates(df) <- ~ longitude + latitude
df <- st_as_sf(df)

df <- st_set_crs(df, 4326)
grid <- shape

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
ggplot(data = world) + geom_sf() + geom_sf(data = shape)  + geom_point(data = df_extract_2_2, aes(x = longitude, y = latitude, color = gridID)) +
  coord_sf(xlim=c(-80, -74), ylim=c(32,37), expand = TRUE) + theme(panel.background = element_rect(fill = "white", colour = "black")) + ggtitle(paste( "Subsetted Points")) + ggtitle(paste("Grid Subset")) + xlab("Longitude") + ylab("Latitude") 

#all
df <- df_extract_2_2
df$date <- as.Date(df$date)
df$year <- as.numeric(format(df$date,"%Y"))
df$month <- as.numeric(format(df$date,"%m"))
#all
df <- df %>% dplyr::select(date, temp, surfacesalinity, surfaceDO, latitude, longitude, Class, month, year)
df$month <- as.numeric(df$month)
df <- merge(df, season, by = "month", all.x = TRUE)
df <- df %>% group_by(year, season) %>% summarise(meantemp = mean(temp, na.rm = TRUE), sdtemp = sd(temp), meansal = mean(surfacesalinity, na.rm = TRUE), sdsal = sd(surfacesalinity), meanDO = mean(surfaceDO,  na.rm = TRUE), sdDO = sd(surfaceDO), samplesize = n()) %>% ungroup()
#Take out obvious outliers in 2016/2017
library(rstatix)
outliers <- df %>% identify_outliers(meanDO) %>% dplyr::filter(is.extreme == TRUE)
finaldf <- df %>% anti_join(outliers)
ggplot(finaldf, mapping = aes(x = as.numeric(year), y = meanDO)) + geom_point() + geom_smooth(method = "lm")
write.csv(finaldf, "data/p1987AllPamlicoWaterCB.csv")

#just north
df <- df_extract_2_2
df$date <- as.Date(df$date)
df$year <- as.numeric(format(df$date,"%Y"))
df$month <- as.numeric(format(df$date,"%m"))
df <- df %>% dplyr::filter(Class == "NORTH") %>% dplyr::select(date, temp, surfacesalinity, surfaceDO, latitude, longitude, Class, month, year)

df$month <- as.numeric(as.character(df$month))
df$year <- as.numeric(as.character(df$year))
df$region <- df$Class
season <- c("winter", "winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "fall", "fall", "fall")
season <- as.data.frame(season)
season$month <- 1:12
df <- merge(df, season, by = "month", all.x = TRUE)
northdf <- df
northfinaldf <- df %>% group_by(year, season) %>% summarise(meantemp = mean(temp), sdtemp = sd(temp), meansal = mean(surfacesalinity), sdsal = sd(surfacesalinity), meanDO = mean(surfaceDO), sdDO = sd(surfaceDO), samplesize = n()) %>% ungroup()

outliers <- northfinaldf %>% identify_outliers(meanDO) %>% dplyr::filter(is.extreme == TRUE)
northfinaldf <- northfinaldf %>% anti_join(outliers)

ts_df <- northfinaldf %>% filter(season != "fall") %>% dplyr::select(meantemp, meansal, meanDO) 
ts_df <- ts(ts_df, frequency = 2, start = 1987)
png(paste0("~/NC-Ecosystem-indicators/figures/pNorthPamlicoWatertimeseriesCB.png"))
plot.ts(ts_df) + title(main = "Central Region", adj = 1)
dev.off()

write.csv(northfinaldf, "data/p1987NorthPamlicoWaterCB.csv")

#subset central
df <- df_extract_2_2
df$date <- as.Date(df$date)
df$year <- format(df$date,"%Y")
df$month <- format(df$date,"%m")
df <- df %>% dplyr::filter(Class == "CENTRAL") %>% dplyr::select(date, temp, surfacesalinity, surfaceDO, latitude, longitude, Class, month, year)

df$month <- as.numeric(as.character(df$month))
df$year <- as.numeric(as.character(df$year))
df$region <- df$gridID
season <- c("winter", "winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "fall", "fall", "fall")
season <- as.data.frame(season)
season$month <- 1:12
df <- merge(df, season, by = "month", all.x = TRUE)
centraldf <- df
centralfinaldf <- df %>% group_by(year, season) %>% summarise(meantemp = mean(temp), sdtemp = sd(temp), meansal = mean(surfacesalinity), sdsal = sd(surfacesalinity), meanDO = mean(surfaceDO), sdDO = sd(surfaceDO), samplesize = n()) %>% ungroup()

outliers <- centralfinaldf %>% identify_outliers(meanDO) %>% dplyr::filter(is.extreme == TRUE)
centralfinaldf <- centralfinaldf %>% anti_join(outliers)

ts_df <- centralfinaldf %>% filter(season != "fall") %>% dplyr::select(meantemp, meansal, meanDO)
ts_df <- ts(ts_df, frequency = 2, start = 1996)
png(paste0("~/NC-Ecosystem-indicators/figures/pCentralPamlicoWatertimeseriesCB.png"))
plot.ts(ts_df) + title(main = "North Region", adj = 1)
dev.off()


write.csv(centralfinaldf, "data/p1987CentralPamlicoWaterCB.csv")

#try bottom temp
#load temp data
df <- read.csv("G:/My Drive/NCTempProject/P195/P195_1214.csv")
names(df) <- tolower(names(df))
df$date <- as.Date(df$date, format = "%m-%d-%Y")
df <- df %>% dplyr::select(date, tempsurface, tempbottom, salinitysurface, sdo, latitudestart, longitudestart)
names(df) <- c("date", "surfacetemp", "bottomtemp", "surfacesalinity", "surfaceDO", "latitude", "longitude")

df$date <- as.Date(df$date)
df$year <- format(df$date,"%Y")
df$month <- format(df$date,"%m")
df$month <- as.numeric(as.character(df$month))
df$year <- as.numeric(as.character(df$year))
df$region <- df$Class
season <- c("winter", "winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "fall", "fall", "fall")
season <- as.data.frame(season)
season$month <- 1:12
df <- merge(df, season, by = "month", all.x = TRUE)
finaldf <- df %>% group_by(year, season) %>% summarise(meansurftemp = mean(surfacetemp, na.rm = TRUE),meanbottomtemp = mean(bottomtemp, na.rm = TRUE), sdtemp = sd(surfacetemp), meansal = mean(surfacesalinity, na.rm = TRUE), sdsal = sd(surfacesalinity), meanDO = mean(surfaceDO, na.rm = TRUE), sdDO = sd(surfaceDO), samplesize = n()) %>% ungroup() %>% filter(season %in% c("summer", "spring"))
