#this is an example, feel free to delete

#delete # and run if you don't have packages installed
# install.packages("tidyverse")
library(tidyverse)


#change your working directory
# setwd("~/NC-Ecosystem-indicators")

#import data
df <- read.csv("./data/MyFile.csv")
df$date <- as.Date(df$date)

#example linear model
fit <- lm(data = df, formula = temperature ~ date)
coef <- summary(fit)$coefficients  
write.csv(coef, paste0("./output/", "examplelinearmodel", ".csv"))
