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
s.datacold<-as.data.frame(scale(datacold))



#loading 2021 dataset cold season testing
df21cold <- read.csv("C:/Users/patron/Desktop/2021coldseason.csv")
head(df21cold); df21cold<-df21cold[7:13]
PMd21cold <- df21cold[complete.cases(df21cold),]
s.PMd21cold<-as.data.frame(scale(PMd21cold))
testdatacold21 = s.PMd21cold[,-c(1,2)]



#MULTIPLE LINEAR REGRESSION FOR COLD SEASON
#MLR for PM 2.5 
pmtwocold.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = s.datacold)
summary(pmtwocold.mlr)

#MLR for PM 10 
pmtencold.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = s.datacold)
summary(pmtencold.mlr)



#predicting PM 2.5 2021 using object pmtwocold.mlr and datacold21 data 
y_predmlr2cold = predict(pmtwocold.mlr, newdata = testdatacold21)
head(y_predmlr2cold)


#plotting predicted y_hat vs actual y from 2021 data PM 2.5 values 
#plot(y_predmlr2cold, s.PMd21cold$PMTWO, main = "Actual PM 2.5 VS MLR Prediction 2021: COLD SEASON", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue")
plot(s.PMd21cold$PMTWO, y_predmlr2cold, xlab = "Actual", ylab = "Predicted PM2.5", xlim=c(-1,1), ylim=c(-1,1), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-s.PMd21cold$PMTWO; p<-y_predmlr2cold
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



#predicting PM 10 2021 using object pmtencold.mlr and datacold21 data 
y_predmlr10cold = predict(pmtencold.mlr, newdata = testdatacold21)
head(y_predmlr10cold)


#plotting predicted y_hat vs actual y from 2021 data PM 10 values 
#plot(y_predmlr10cold, s.PMd21cold$PMTEN, main = "Actual PM 10 VS MLR Prediction for 2021: COLD SEASON", xlab = "Predicted", ylab = "Actual PM 10", col = "coral")
plot(s.PMd21cold$PMTEN, y_predmlr10cold, xlab = "Actual", ylab = "Predicted PM 10", xlim=c(-1,1), ylim=c(-1,1), col = "coral")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-s.PMd21cold$PMTEN; p<-y_predmlr10cold
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

