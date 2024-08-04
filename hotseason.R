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
s.datahot<-as.data.frame(scale(datahot))


#this is not producing any prediction for some reason
#df21hot <- read.csv("C:/Users/patron/Desktop/2021hotseason.csv")
#head(df21hot); df21hot<-df21hot[7:13]
#PMd21hot <- df21hot[complete.cases(df21hot),]
#s.PMd21hot<-as.data.frame(scale(PMd21hot))
#testdatahot21 = s.PMd21hot[,-c(1,2)]


#loading 2021 dataset hot season testing
dfH21 <- read.csv("C:/Users/patron/Desktop/2021hotseason.csv")
datahot21 <- (dfH21[7:13])
#dfHOT21cn <- (datahot21 %>% drop_na())
#sdfHOT21 <- scale(dfHOT21cn)
#head(sdfHOT21)
#nrow(sdfHOT21)
testdatahot21 = datahot21[,-c(1,2)]





#MULTIPLE LINEAR REGRESSION FOR HOT SEASON
#MLR for PM 2.5 
pmtwohot.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = s.datahot)
summary(pmtwohot.mlr)

#MLR for PM 10 
pmtenhot.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = s.datahot)
summary(pmtenhot.mlr)




#predicting PM 2.5 2021 
y_predmlr2hot = predict(pmtwohot.mlr, newdata = testdatahot21)
head(y_predmlr2hot)


#plotting predicted y_hat vs actual y from 2021 data PM 2.5 values 
plot(y_predmlr2hot, datahot21$PMTWO, main = "Actual PM 2.5 VS MLR Prediction 2021: HOT SEASON", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue")
plot(datahot21$PMTWO, y_predmlr2hot, xlab = "Actual", ylab = "Predicted PM 10", xlim=c(0,50), ylim=c(0,50), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-datahot21$PMTWO; p<-y_predmlr2hot
R2<-sum((y-mean(y))*(p-mean(p)))/(273*sd(y)*sd(p))
MSE<-sum((y-p)^2)/273
MAE<-sum(abs(y-p))/273
PA<-sum((p-mean(p))^2)/sum((y-mean(y))^2)
IA<- 1 - sum((p-mean(p))^2)/sum((abs(p-mean(p)) + abs(y-mean(y)))^2)
print(R2)
print(MSE) 
print(MAE) 
print(PA) 
print(IA)



#predicting PM 10 2021
y_predmlr10hot = predict(pmtenhot.mlr, newdata = testdatahot21)
head(y_predmlr10hot)


#plotting predicted y_hat vs actual y from 2021 data PM 10 values 
plot(y_predmlr10hot, datahot21$PMTEN, main = "Actual PM 10 VS MLR Prediction for 2021: HOT SEASON", xlab = "Predicted", ylab = "Actual PM 10", col = "coral")
plot(datahot21$PMTEN, y_predmlr10hot, xlab = "Actual", ylab = "Predicted PM10", xlim=c(0,60), ylim=c(0,60), col = "coral")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-datahot21$PMTEN; p<-y_predmlr10hot
R2<-sum((y-mean(y))*(p-mean(p)))/(273*sd(y)*sd(p))
MSE<-sum((y-p)^2)/273
MAE<-sum(abs(y-p))/273
PA<-sum((p-mean(p))^2)/sum((y-mean(y))^2)
IA<- 1 - sum((p-mean(p))^2)/sum((abs(p-mean(p)) + abs(y-mean(y)))^2)
print(R2)
print(MSE) 
print(MAE) 
print(PA) 
print(IA)


