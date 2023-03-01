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
df <- read.csv("G:/My Drive/NCTempProject/P195/p195final.csv")
df$date <- as.Date(df$date, tryFormats = c("%m/%d/%Y"))
df <- df %>% dplyr::select(date, temp, surfacesalinity, surfaceDO, latitude, longitude) %>% na.omit

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

#subset north
df <- df_extract_2_2
df$date <- as.Date(df$date)
df$year <- format(df$date,"%Y")
df$month <- format(df$date,"%m")
df <- df %>% dplyr::filter(Class == "NORTH") %>% dplyr::select(date, temp, surfacesalinity, surfaceDO, latitude, longitude, Class, month, year)

df$month <- as.numeric(as.character(df$month))
df$year <- as.numeric(as.character(df$year))
df$region <- df$gridID
season <- c("winter", "winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "fall", "fall", "fall")
season <- as.data.frame(season)
season$month <- 1:12
df <- merge(df, season, by = "month", all.x = TRUE)
northdf <- df
northfinaldf <- df %>% group_by(year, season) %>% summarise(meantemp = mean(temp), sdtemp = sd(temp), meansal = mean(surfacesalinity), sdsal = sd(surfacesalinity), meanDO = mean(surfaceDO), sdDO = sd(surfaceDO), samplesize = n())
write.csv(northfinaldf, "data/p1996NorthPamlicoWaterCB.csv")

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
centralfinaldf <- df %>% group_by(year, season) %>% summarise(meantemp = mean(temp), sdtemp = sd(temp), meansal = mean(surfacesalinity), sdsal = sd(surfacesalinity), meanDO = mean(surfaceDO), sdDO = sd(surfaceDO), samplesize = n())
write.csv(centralfinaldf, "data/p1996CentralPamlicoWaterCB.csv")

