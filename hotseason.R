#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)

install.packages("ggplot2")
install.packages("GGally")
install.packages("psych")
install.packages("Hmisc")
install.packages("corrplot")
library(tidyverse)
library(Hmisc)
library(corrplot)

#loading data
datahot <- read.csv("C:/Users/patron/Desktop/hotseasonpmdata.csv")
head(datahot)
my_data2 <- (datahot[7:15])
#scatterplot matrix 
pairs(my_data2, main = "Scatterplot Matrix for PM Data")

#hot season June - August 
#Multiple linear regression for PM 2.5 
pmtwohot.regression <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WDF5 + WSF2 + WSF5, data = datahot)
summary(pmtwohot.regression)

#Multiple linear regression for PM 10 
pmtenhot.regression <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WDF5 + WSF2 + WSF5, data = datahot)
summary(pmtenhot.regression)
