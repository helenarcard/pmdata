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

#loading Full dataset cold season November - April
dfcold <- read.csv("C:/Users/patron/Desktop/coldseasonpmdata.csv")
head(dfcold)
datacold <- (dfcold[7:13])
#preping colddata for later MLR, SVR, and NN 
#removing na values
#dfCOLDcn <- (datacold %>% drop_na())
#sdfCOLD <- scale (dfCOLDcn)
#check to make sure data looks right and number of rows is not too few 
#head(sdfCOLD)
#nrow(sdfCOLD)

#loading 2021 dataset hot season testing
dfC21 <- read.csv("C:/Users/patron/Desktop/2021coldseason.csv")
datacold21 <- (dfC21[7:13])
#dfCOLD21cn <- (datacold21 %>% drop_na())
#sdfCOLD21 <- scale(dfCOLD21cn)
#head(sdfCOLD21)
#nrow(sdfCOLD21)
testdatacold21 = datacold21[,-c(1,2)]

#setting seed so random can be reproduced 
set.seed(7) 



#MULTIPLE LINEAR REGRESSION FOR COLD SEASON
#MLR for PM 2.5 
pmtwocold.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfcold)
summary(pmtwocold.mlr)

#MLR for PM 10 
pmtencold.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfcold)
summary(pmtencold.mlr)


#predicting 2021 PM data from COLD dataset MLR
#actual MLR for PM 2.5 2021 only
pmtwocold21.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfC21)
summary(pmtwocold21.mlr)

#predicting PM 2.5 2021 using object pmtwocold.mlr and datacold21 data 
y_predmlr2cold = predict(pmtwocold.mlr, newdata = testdatacold21)
head(y_predmlr2cold)
head(datacold21$PMTWO)


#plotting predicted y_hat vs actual y from 2021 data PM 2.5 values 
plot(y_predmlr2cold, datacold21$PMTWO, main = "Actual PM 2.5 VS MLR Prediction 2021: COLD SEASON", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue")


#R SQUARED error metric -- Coefficient of Determination
postResample(y_predmlr2cold, datacold21$PMTWO)



#actual MLR for PM 10 2021 only 
pmtencold21.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = dfC21)
summary(pmtencold21.mlr)

#predicting PM 10 2021 using object pmtencold.mlr and datacold21 data 
y_predmlr10cold = predict(pmtencold.mlr, newdata = testdatacold21)
head(y_predmlr10cold)
head(datacold21$PMTEN)

#plotting predicted y_hat vs actual y from 2021 data PM 10 values 
plot(y_predmlr10cold, datacold21$PMTEN, main = "Actual PM 10 VS MLR Prediction for 2021: COLD SEASON", xlab = "Predicted", ylab = "Actual PM 10", col = "coral")

#R SQUARED error metric -- Coefficient of Determination
postResample(y_predmlr10cold, datacold21$PMTEN)

