#if you get "trying to use cran without setting a mirror" error when compiling report uncomment this
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
data1 <- read.csv("C:/Users/patron/Desktop/DataPMyosemitestation.csv")
head(data1)
my_data <- (data1[7:15])
dataWDF <- (data1[12:13])

#scatterplot matrix 
pairs(dataWDF, main = "Scatterplot Matrix for WDF")


#Multiple linear regression for WDF2 pm 2.5
pmtwoWDF2.regression <- lm(PMTWO ~ WDF2, data = my_data)
summary(pmtwoWDF2.regression)
#Multiple linear regression for WDF5 pm 2.5
pmtwoWDF5.regression <- lm(PMTWO ~ WDF5, data = my_data)
summary(pmtwoWDF5.regression)


#Multiple linear regression for WDF2 PM 10 
pmtenWDF2.regression <- lm(PMTEN ~ WDF2, data = my_data)
summary(pmtenWDF2.regression)
#Multiple linear regression for WDF2 PM 10 
pmtenWDF5.regression <- lm(PMTEN ~ WDF5, data = my_data)
summary(pmtenWDF5.regression)
