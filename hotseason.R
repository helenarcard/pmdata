#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)

install.packages("ggplot2")
install.packages("GGally")
install.packages("psych")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("e1071")
install.packages('caTools')
library(caTools) 
require(e1071) #Contains the SVM
library(tidyverse)
library(Hmisc)
library(corrplot)

#loading data hot season June - August 
dfhot <- read.csv("C:/Users/patron/Desktop/hotseasonpmdata.csv")
head(dfhot)
datahot <- (dfhot[7:13])
#scatterplot matrix hot season 
pairs(datahot, main = "Scatterplot Matrix for Hot Season PM Data")



#MULTIPLE LINEAR REGRESSION FOR HOT SEASON
#MLR for PM 2.5 
pmtwohot.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfhot)
summary(pmtwohot.mlr)

#MLR for PM 10 
pmtenhot.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfhot)
summary(pmtenhot.mlr)

