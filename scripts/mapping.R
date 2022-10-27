#delete # and run if you don't have packages installed
# install.packages("tidyverse")
# install.packages("ggmap")
library(tidyverse)
library(ggmap)

#change your working directory
# setwd("~/NC-Ecosystem-indicators")

#load data (change to your file name)
df <- read.csv("./data/MyFile.csv")

#mapping info
myLocation <- c(-78.900147,33.802938,-75.263672,36.672128)
myMap <- get_map(location=myLocation, source="osm", color="bw")

#plot, make sure lat and long are numeric, change title
ggmap(myMap) +
  geom_point(data = df, aes(x = longitude, y = latitude), color = "blue", size = 0.1) + ggtitle("MyTitle")

#save, change title
ggsave(plot = last_plot(), filename = paste0("./figures/", "MyTitle", ".png"), width = 6, height = 5, units = "in")