#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)
install.packages("ggplot2")
install.packages("e1071")
install.packages('caTools')
install.packages("corrplot")
install.packages("caret")
library(ggplot2)
library(caret)
library(caTools) 
require(e1071) #Contains the SVM 
library(tidyverse)
library(corrplot)

#loading COLD dataset for training 
dfcold <- read.csv("C:/Users/patron/Desktop/coldseasonpmdata.csv")
head(dfcold)
datacold <- (dfcold[7:13])
PMdcold <- (datacold %>% drop_na())
sPMdcold <- scale(PMdcold)
head(sPMdcold)
nrow(sPMdcold)

#loading 2021 COLD dataset for testing
df21cold <- read.csv("C:/Users/patron/Desktop/2021coldseason.csv")
PMd21cold <- (df21cold[7:13])
PMd21cleancold <- (PMd21cold %>% drop_na())
sPM21cold <- scale(PMd21cleancold)
head(sPM21cold)
nrow(sPM21cold)

#setting seed so random can be reproduced 
set.seed(7)

#Train data is COLD dataset 2015-2020, test data is 2021 COLD dataset minus pm data 
Train = sPMdcold
Test = sPM21cold[,-c(1,2)] 




#SVM FOR COLD SEASON 
#SVM for PM 2.5 
svm.mod2cold = svm(formula = PMTWO ~ AWND + PRCP + TAVG + WSF2 + WDF2, 
                  data = Train, 
                  type = 'nu-regression', 
                  kernel = 'linear') 
print(svm.mod2cold)


#Predicting the Test set results (y_hat value)
y_predsvm2cold = predict(svm.mod2cold, newdata = Test)

head(y_predsvm2cold)

#plotting predicted y_hat vs actual y from test data PM 2.5 values 
plot(y_predsvm2cold, sPM21cold[,1], main = "PM 2.5 VS SVM Prediction HOT SEASON 2021", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue3")
plot(sPM21cold[,1], y_predsvm2cold, xlab = "Actual", ylab = "Predicted PM 2.5", xlim=c(-1,1), ylim=c(-1,1), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21cold[,1]; p<-y_predsvm2cold
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


#SVM for PM 10
svm.mod10cold = svm(formula = PMTEN ~ AWND + PRCP + TAVG + WSF2 + WDF2, 
                   data = Train, 
                   type = 'nu-regression', 
                   kernel = 'linear') 
print(svm.mod10cold)


#Predicting the Test set results (y_hat value)
y_predsvm10cold = predict(svm.mod10cold, newdata = Test)

head(y_predsvm10cold)

#plotting predicted y_hat vs actual y from test data PM 10 values 
plot(y_predsvm10cold, sPM21cold[,2], main = "PM 10 VS SVM Prediction HOT SEASON 2021", xlab = "Predicted", ylab = "Actual PM 10", col = "coral2")
plot(sPM21cold[,2], y_predsvm10cold, xlab = "Actual", ylab = "Predicted PM 10", xlim=c(-1,1), ylim=c(-1,1), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21cold[,2]; p<-y_predsvm10cold
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

