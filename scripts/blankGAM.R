#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

library(devtools)

setwd("~/NC-Ecosystem-indicators")
source_gist("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30")

library(mgcv)
library(ggplot2)
library(stringr)
# library(mgcViz)


#######Load the datasets
setwd("~/NC-Ecosystem-indicators/data")
data_files <- list.files("~/NC-Ecosystem-indicators/data")
data_files <- data_files[grepl("^c", data_files)]

for(i in 1:length(data_files)) {                              
  assign(paste0(str_sub(data_files[i], end=-12)),                                  
         read.csv(paste0("~/NC-Ecosystem-indicators/data/",
                         data_files[i])))
}
data_files <- str_sub(data_files, end=-12)

# Your final time series is (hopefully) a dataframe with a column for the year 
# and a column for whatever the data variable is.  Here I give an example using 
# Hudson river mean flow data where one column is year and the other is flowrate

#######################
for(i in 1:length(data_files)){
BT <- get(data_files[i])
names(BT) <- tolower(names(BT))

BT <- filter(BT, year < 2020)
# BT <- filter(BT, year > 1980)
#average on year
# BT <- BT %>% group_by(year, season) %>% filter(season == "fall") %>% summarise(avtemp = mean(meantemp, na.rm = TRUE))

#other temps
# BT <- BT %>% group_by(year) %>% summarise(avtemp = mean(meantemp, na.rm = TRUE))

#DEQ sal
# BT <- BT %>% group_by(year) %>% summarise(avtemp = mean(mean_dissolvedoxygen, na.rm = TRUE))


#deq temps
# BT <- BT %>% group_by(year) %>% summarise(avtemp = mean(mean_temperature, na.rm = TRUE))

#flow 
# BT <- BT %>% group_by(year) %>% summarise(avtemp = mean(annual.average.flow..m3.s., na.rm = TRUE))

#chl a
# BT <- BT %>% group_by(year) %>% summarise(avtemp = mean(chlorophyll.a, na.rm = TRUE))


# Creat a GAM - adjust k and remember to check model
mod <- gam(avtemp ~ s(year, k=7), data = BT)
summary(mod) #check out model
gam.check(mod)

pdata <- with(BT, data.frame(year = year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = as.numeric(attr(p2_mod,"constant")) # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
n <- as.numeric(length(unique(pdata$year)))
mod.d <- Deriv(mod, n=n) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv, upper = mod.dci[[Term]]$upper, lower = mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(avtemp ~ year, data = BT)
lines(avtemp ~ year, data = BT)
lines(p2_mod+intercept ~ year, data = pdata, type = "n")
lines(p2_mod+intercept ~ year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)

# linearMod<- lm(avtemp ~ year, data=BT)
# summary(linearMod)

name <- str_sub(data_files[i], 6)
name <- gsub("(?!^)(?=[[:upper:]])", " ", name, perl=T)

BTpost1997 <- filter(BT, year > 1997)
linearMod<- lm(avtemp ~ year, data=BTpost1997)
summary(linearMod)

name <- "Neuse River Chlorophyll a"

#non significant linear
ggplot() + 
  geom_line(data = BT, aes(x = year, y = avtemp), color = 'grey53', alpha = 0.6) +
  geom_point(data = BT, aes(x = year, y = avtemp), color = 'gray53', alpha = 0.6) + 
  geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) +  
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) +  
  theme_bw() +
  labs (y = "Chlorophyll a (ug/L)", x = 'Year', title = paste0(name)) + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

ggsave(paste0("~/NC-Ecosystem-indicators/figures/GAMS/", data_files[i], "GAMwithTrendDO.png"))

name <- "Albemarle Sound Chlorophyll a"

#significant linear
ggplot() + 
  geom_line(data = BT, aes(x = year, y = avtemp), color = 'grey53', alpha = 0.6) +
  geom_point(data = BT, aes(x = year, y = avtemp), color = 'gray53', alpha = 0.6) + 
  geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) +   geom_smooth(data = BTpost1997, aes(x = year, y = avtemp), method = lm, se = FALSE, color = 'darkgreen') +
  theme_bw() +
  labs (y = "Chlorophyll a (ug/L)", x = 'Year', title = paste0(name)) + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

temp <- "Temperature (\u00B0C)"

ggsave(paste0("~/NC-Ecosystem-indicators/figures/GAMS/", gsub(" ", "", name) , "GAMwithTrend.png"))
}

#### 
#for biomass
#import
load("~/NC-Ecosystem-indicators/data/B1972landingsLS.RData")
for(i in 1:length(data_files)){
  BT <- trunc_df
  names(BT) <- tolower(names(BT))
  BT <- filter(BT , fish == "Blue Crabs")
  BT <- BT[, -c(1, 2)]
  BT <- BT %>% pivot_longer(cols = everything(), names_to = "year", values_to = "landings")
  BT$year <- as.numeric(BT$year)
  BT <- filter(BT, year < 2020)
  # BT <- filter(BT, year > 1980)
  #average on year
  # BT <- BT %>% group_by(year, season)  %>% summarise(avtemp = mean(mean_temperature, na.rm = TRUE))
  
  # Creat a GAM - adjust k and remember to check model
  mod <- gam(landings ~ s(year, k=7), data = BT)
  summary(mod) #check out model
  gam.check(mod)
  
  pdata <- with(BT, data.frame(year = year))
  p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
  intercept = as.numeric(attr(p2_mod,"constant")) # look at p2_mod and extract the intercept
  pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])
  
  #  Now that we have the model prediction, the next step is to calculate the first derivative
  #  Then determine which increases and decreases are significant
  Term = "year"
  n <- as.numeric(length(unique(pdata$year)))
  mod.d <- Deriv(mod, n=n) # n is the number of years
  mod.dci <- confint(mod.d, term = Term)
  mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv, upper = mod.dci[[Term]]$upper, lower = mod.dci[[Term]]$lower)
  
  # Take a quick look to make sure it appears ok before final plotting
  plot(landings ~ year, data = BT)
  lines(landings ~ year, data = BT)
  lines(p2_mod+intercept ~ year, data = pdata, type = "n")
  lines(p2_mod+intercept ~ year, data = pdata)
  lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
  lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)
  
  
  name <- "Blue Crab Landings"
  
  BTpost1997 <- filter(BT, year > 1997)
  linearMod<- lm(landings ~ year, data=BTpost1997)
  summary(linearMod)
  
  #non significant linear
  ggplot() + 
    geom_line(data = BT, aes(x = year, y = landings), color = 'grey53', alpha = 0.6) +
    geom_point(data = BT, aes(x = year, y = landings), color = 'gray53', alpha = 0.6) + 
    geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
    geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) +  
    geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) +  
    theme_bw() +
    labs (y = "Landings", x = 'Year', title = paste0(name)) + 
    theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
  
  ggsave(paste0("~/NC-Ecosystem-indicators/figures/GAMS/", data_files[i], "GAMwithTrend.png"))
  
  #significant linear
  ggplot() + 
    geom_line(data = BT, aes(x = year, y = landings), color = 'grey53', alpha = 0.6) +
    geom_point(data = BT, aes(x = year, y = landings), color = 'gray53', alpha = 0.6) + 
    geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
    geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
    geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) +   geom_smooth(data = BTpost1997, aes(x = year, y = landings), method = lm, se = FALSE, color = 'darkgreen') +
    theme_bw() +
    labs (y = "Landings", x = 'Year', title = paste0(name)) + 
    theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
  
  temp <- "Temperature (\u00B0C)"
  
  ggsave(paste0("~/NC-Ecosystem-indicators/figures/GAMS/",  gsub(" ", "", name), "GAMwithTrend.png"))
}

#for population
data_files <- list.files("~/NC-Ecosystem-indicators/data")
data_files <- data_files[grepl("^s", data_files)]

for(i in 1:length(data_files)) {  
  i = 1
  assign(paste0(str_sub(data_files[i], end=-4)),                                  
         read.csv(paste0("~/NC-Ecosystem-indicators/data/",
                         data_files[i]), header=FALSE, stringsAsFactors=FALSE,fileEncoding="latin1"))
}
data_files <- str_sub(data_files, end=-4)

# Your final time series is (hopefully) a dataframe with a column for the year 
# and a column for whatever the data variable is.  Here I give an example using 
# Hudson river mean flow data where one column is year and the other is flowrate


#######################
for(i in 1:length(data_files)){
  BT <- get(data_files[i])
  names(BT) <- tolower(names(BT))
  BT <- dplyr::select(BT, county, beaufort)
  names(BT) <- c("year", "population")
  BT <- filter(BT, year < 2020)
  # BT <- filter(BT, year > 1980)
  BT$population <- as.numeric(gsub(",","",BT$population))
  
  # Creat a GAM - adjust k and remember to check model
  mod <- gam(population ~ s(year, k=15),gamma = 1.4, data = BT)
  summary(mod) #check out model
  gam.check(mod)
  
  pdata <- with(BT, data.frame(year = year))
  p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
  intercept = as.numeric(attr(p2_mod,"constant")) # look at p2_mod and extract the intercept
  pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])
  
  #  Now that we have the model prediction, the next step is to calculate the first derivative
  #  Then determine which increases and decreases are significant
  Term = "year"
  n <- as.numeric(length(unique(pdata$year)))
  mod.d <- Deriv(mod, n=n) # n is the number of years
  mod.dci <- confint(mod.d, term = Term)
  mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv, upper = mod.dci[[Term]]$upper, lower = mod.dci[[Term]]$lower)
  
  # Take a quick look to make sure it appears ok before final plotting
  plot(population ~ year, data = BT)
  lines(population ~ year, data = BT)
  lines(p2_mod+intercept ~ year, data = pdata, type = "n")
  lines(p2_mod+intercept ~ year, data = pdata)
  lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
  lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)
  
  BTpost1997 <- filter(BT, year > 1997)
  linearMod<- lm(population ~ year, data=BTpost1997)
  summary(linearMod)
  
  name <- "Beaufort County Population"
  

  
  ggplot() + 
    geom_line(data = BT, aes(x = year, y = population), color = 'grey53', alpha = 0.6) +
    geom_point(data = BT, aes(x = year, y = population), color = 'gray53', alpha = 0.6) + 
    geom_line(data = pdata, mapping = aes(x = year, y = p2_mod + intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
    geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
    geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) +   geom_smooth(data = BTpost1997, aes(x = year, y = population), method = lm, se = FALSE, color = 'darkgreen') +
    theme_bw() +
    labs (y = "Population", x = 'Year', title = paste0(name)) + 
    theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
  
  temp <- "Temperature (\u00B0C)"
  
  ggsave(paste0("~/NC-Ecosystem-indicators/figures/GAMS/", name, "GAM.png"))
}