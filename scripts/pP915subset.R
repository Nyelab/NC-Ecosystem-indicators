require(sf)
shape <- sf::read_sf(file.path("data/shapefiles/rivers.shp"))
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
df <- read.csv("G:/My Drive/NCTempProject/P915/p915final.csv")
df$date <- as.Date(df$date)
df$temp <- df$surfacetemp
df <- df %>% dplyr::select(date, temp, surfacesal, surfacedo, pH, latitude, longitude) %>% drop_na(latitude, longitude)
#took out na omit :?
df %>% ggplot(mapping = aes(x = date, y = surfacedo)) + geom_point()
weird <- df %>% mutate(year = as.numeric(format(date, "%Y")))
weird <- weird %>% filter(year > 2014) %>% filter(year < 2016)
weird  %>% ggplot(mapping = aes(x = date, y = surfacedo)) + geom_point()
ggplot(data = world) + geom_sf() +
  coord_sf(xlim=c(-78, -75), ylim=c(33,37), expand = TRUE) + theme(panel.background = element_rect(fill = "white", colour = "black")) + geom_point(weird, mapping = aes(x = longitude, y = latitude, color = surfacedo))


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

df <- df_extract_2_2
capefear <- filter(df, Class == "CAPEFEAR")
new <- filter(df, Class == "NEW")
neuse <- filter(df, Class == "NEUSE")
pamlico <- filter(df, Class == "PAMLICO")
datalist <- list(capefear, new, neuse, pamlico)
names <- c("capefearriver", "newriver", "neuseriver", "pamlicoriver")

for (i in 1:length(names)) {
df <- datalist[[i]]
label <- names[i]
df$date <- as.Date(df$date)
df$year <- format(df$date,"%Y")
df$month <- format(df$date,"%m")
df <- df %>% dplyr::select(date, temp, pH, surfacesal, surfacedo, latitude, longitude, Class, month, year)
df$month <- as.numeric(as.character(df$month))
df$year <- as.numeric(as.character(df$year))
df$region <- df$Class
season <- c("winter", "winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "fall", "fall", "fall")
season <- as.data.frame(season)
season$month <- 1:12
df <- merge(df, season, by = "month", all.x = TRUE)

finaldf <- df %>% group_by(year, season) %>% summarise(meantemp = mean(temp, na.rm=TRUE), sdtemp = sd(temp), meansal = mean(surfacesal,na.rm=TRUE), sdsal = sd(surfacesal), meanDO = mean(surfacedo, na.rm=TRUE), sdDO = sd(surfacedo), meanpH = mean(pH, na.rm=TRUE), sdpH = sd(pH), samplesize = n()) %>% ungroup()

ts_df <- select(finaldf, meantemp, meansal, meanDO, meanpH)
ts_df <- ts(ts_df, frequency = 4, start = 2008)
png(paste0("~/NC-Ecosystem-indicators/figures/p", min(finaldf$year), names[i], "timeseriesCB.png"))
plot.ts(ts_df) + title(main = paste0(sub = label), adj = 1)
dev.off()
write.csv(finaldf, paste0("~/NC-Ecosystem-indicators/data/p", min(finaldf$year), names[i], "WaterCB.csv"))
}
