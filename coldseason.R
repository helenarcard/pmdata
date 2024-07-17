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
datacold <- read.csv("C:/Users/patron/Desktop/coldseasonpmdata.csv")
head(datacold)
my_data3 <- (datacold[7:15])
#scatterplot matrix 
pairs(my_data3, main = "Scatterplot Matrix for PM Data")

#cold season November - April 
#Multiple linear regression for PM 2.5 
pmtwocold.regression <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WDF5 + WSF2 + WSF5, data = datacold)
summary(pmtwocold.regression)

#Multiple linear regression for PM 10 
pmtencold.regression <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WDF5 + WSF2 + WSF5, data = datacold)
summary(pmtencold.regression)

