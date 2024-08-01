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

#loading Full dataset hot season June - August 
dfhot <- read.csv("C:/Users/patron/Desktop/hotseasonpmdata.csv")
head(dfhot)
datahot <- (dfhot[7:13])
#preping hotdata for later MLR, SVR, and NN 
#removing na values
#dfHOTcn <- (datahot %>% drop_na())
#sdfHOT <- scale (dfHOTcn)
#check to make sure data looks right and number of rows is not too few 
#head(sdfHOT)
#nrow(sdfHOT)

#loading 2021 dataset hot season testing
dfH21 <- read.csv("C:/Users/patron/Desktop/2021hotseason.csv")
datahot21 <- (dfH21[7:13])
#dfHOT21cn <- (datahot21 %>% drop_na())
#sdfHOT21 <- scale(dfHOT21cn)
#head(sdfHOT21)
#nrow(sdfHOT21)
testdatahot21 = datahot21[,-c(1,2)]
#setting seed so random can be reproduced 
set.seed(6) 



#MULTIPLE LINEAR REGRESSION FOR HOT SEASON
#MLR for PM 2.5 
pmtwohot.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfhot)
summary(pmtwohot.mlr)

#MLR for PM 10 
pmtenhot.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfhot)
summary(pmtenhot.mlr)



#predicting 2021 PM data from HOT dataset using MLR
#actual MLR for PM 2.5 2021 only
pmtwohot21.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfH21)
summary(pmtwohot21.mlr)

#predicting PM 2.5 2021 using object pmtwohot.mlr and datahot21 data 
y_predmlr2hot = predict(pmtwohot.mlr, newdata = testdatahot21)
head(y_predmlr2hot)
head(datahot21$PMTWO)


#plotting predicted y_hat vs actual y from 2021 data PM 2.5 values 
plot(y_predmlr2hot, datahot21$PMTWO, main = "Actual PM 2.5 VS MLR Prediction 2021: HOT SEASON", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue")


#R SQUARED error metric -- Coefficient of Determination
postResample(y_predmlr2hot, datahot21$PMTWO)



#actual MLR for PM 10 2021 only 
pmtenhot21.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfH21)
summary(pmtenhot21.mlr)

#predicting PM 10 2021 using object pmtenhot.mlr and datahot21 data 
y_predmlr10hot = predict(pmtenhot.mlr, newdata = testdatahot21)
head(y_predmlr10hot)
head(datahot21$PMTEN)

#plotting predicted y_hat vs actual y from 2021 data PM 10 values 
plot(y_predmlr10hot, datahot21$PMTEN, main = "Actual PM 10 VS MLR Prediction for 2021: HOT SEASON", xlab = "Predicted", ylab = "Actual PM 10", col = "coral")

#R SQUARED error metric -- Coefficient of Determination
postResample(y_predmlr10hot, datahot21$PMTEN)

