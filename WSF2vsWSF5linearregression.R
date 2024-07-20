r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

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
dataWSF <- (data1[14:15])

#scatterplot matrix 
pairs(dataWSF, main = "Scatterplot Matrix for WSF")


#Multiple linear regression for WSF2 pm 2.5
pmtwoWSF2.regression <- lm(PMTWO ~ WSF2, data = my_data)
summary(pmtwoWSF2.regression)
#Multiple linear regression for WSF5 pm 2.5
pmtwoWSF5.regression <- lm(PMTWO ~ WSF5, data = my_data)
summary(pmtwoWSF5.regression)


#Multiple linear regression for WSF2 PM 10 
pmtenWSF2.regression <- lm(PMTEN ~ WSF2, data = my_data)
summary(pmtenWSF2.regression)
#Multiple linear regression for WSF5 PM 10 
pmtenWSF5.regression <- lm(PMTEN ~ WSF5, data = my_data)
summary(pmtenWSF5.regression)
