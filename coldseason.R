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

#loading data cold season November - April
dfcold <- read.csv("C:/Users/patron/Desktop/coldseasonpmdata.csv")
head(dfcold)
datacold <- (dfcold[7:13])
#scatterplot matrix 
pairs(datacold, main = "Scatterplot Matrix for Cold Season PM Data")



#MULTIPLE LINEAR REGRESSION FOR COLD SEASON
#MLR for PM 2.5 
pmtwocold.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfcold)
summary(pmtwocold.mlr)

#MLR for PM 10 
pmtencold.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfcold)
summary(pmtencold.mlr)

