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

#loading HOT SEASON dataset for training 
dfhot <- read.csv("C:/Users/patron/Desktop/hotseasonpmdata.csv")
head(dfhot)
datahot <- (dfhot[7:13])
PMdhot <- (datahot %>% drop_na())
sPMdhot <- scale(PMdhot)
head(sPMdhot)
nrow(sPMdhot)

#loading 2021 HOT SEASON dataset for testing
df21hot <- read.csv("C:/Users/patron/Desktop/2021hotseason.csv")
PMd21hot <- (df21hot[7:13])
PMd21cleanhot <- (PMd21hot %>% drop_na())
sPM21hot <- scale(PMd21cleanhot)
head(sPM21hot)
nrow(sPM21hot)

#setting seed so random can be reproduced 
set.seed(3) 

#Train data is full dataset, Test data is 2021 dataset
Train = sPMdhot
Test = sPM21hot[,-c(1,2)] 



#SVM FOR HOT SEASON 
#SVM for PM 2.5 
svm.mod2hot = svm(formula = PMTWO ~ AWND + PRCP + TAVG + WSF2 + WDF2, 
               data = Train, 
               type = 'nu-regression', 
               kernel = 'linear') 
print(svm.mod2hot)


#Predicting the Test set results (y_hat value)
y_predsvm2hot = predict(svm.mod2hot, newdata = Test)

print(y_predsvm2hot)

#plotting predicted y_hat vs actual y from test data PM 2.5 values 
plot(y_predsvm2hot, sPM21hot[,1], main = "PM 2.5 VS SVM Prediction HOT SEASON 2021", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue3")
plot(sPM21hot[,1], y_predsvm2hot, xlab = "Actual", ylab = "Predicted PM 2.5", xlim=c(-1,1), ylim=c(-1,1), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21hot[,1]; p<-y_predsvm2hot
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
svm.mod10hot = svm(formula = PMTEN ~ AWND + PRCP + TAVG + WSF2 + WDF2, 
                data = Train, 
                type = 'nu-regression', 
                kernel = 'linear') 
print(svm.mod10hot)


#Predicting the Test set results (y_hat value)
y_predsvm10hot = predict(svm.mod10hot, newdata = Test)

print(y_predsvm10hot)

#plotting predicted y_hat vs actual y from test data PM 10 values 
plot(y_predsvm10hot, sPM21hot[,2], main = "PM 10 VS SVM Prediction HOT SEASON 2021", xlab = "Predicted", ylab = "Actual PM 10", col = "coral2")
plot(sPM21hot[,2], y_predsvm10hot, xlab = "Actual", ylab = "Predicted PM 10", xlim=c(0,50), ylim=c(0,50), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21hot[,2]; p<-y_predsvm10hot
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

