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

#loading full dataset for training 
df <- read.csv("C:/Users/patron/Desktop/DataPMyosemitestation.csv")
head(df)
data <- (df[7:13])
PMd <- (data %>% drop_na())
sPMd <- scale(PMd)
head(sPMd)
nrow(sPMd)

#loading 2021 dataset for testing
df21 <- read.csv("C:/Users/patron/Desktop/2021pmdataexcess.csv")
PMd21 <- (df21[7:13])
PMd21clean <- (PMd21 %>% drop_na())
sPM21 <- scale(PMd21clean)
head(sPM21)
nrow(sPM21)

#setting seed so random can be reproduced 
set.seed(1) 

#Train data is full dataset, Test data is 2021 dataset
Train = sPMd
Test = sPM21[,-c(1,2)] 



#SVM FOR FULL DATASET
#SVM for PM 2.5 
svm.mod2 = svm(formula = PMTWO ~ AWND + PRCP + TAVG + WSF2 + WDF2, 
                 data = Train, 
                 type = 'nu-regression', 
                 kernel = 'linear') 
print(svm.mod2)


#Predicting the Test set results (y_hat value)
y_predsvm2 = predict(svm.mod2, newdata = Test)

head(y_predsvm2)

#plotting predicted y_hat vs actual y from test data PM 2.5 values 
plot(y_predsvm2, sPM21[,1], main = "PM 2.5 VS SVM Prediction 2021", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue3")
plot(sPM21[,1], y_predsvm2, xlab = "Actual", ylab = "Predicted PM 2.5", xlim=c(-1,1), ylim=c(-1,1), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21[,1]; p<-y_predsvm2
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
svm.mod10 = svm(formula = PMTEN ~ AWND + PRCP + TAVG + WSF2 + WDF2, 
               data = Train, 
               type = 'nu-regression', 
               kernel = 'linear') 
print(svm.mod10)


#Predicting the Test set results (y_hat value)
y_predsvm10 = predict(svm.mod10, newdata = Test)

head(y_predsvm10)

#plotting predicted y_hat vs actual y from test data PM 10 values 
plot(y_predsvm10, sPM21[,2], main = "PM 10 VS SVM Prediction 2021", xlab = "Predicted", ylab = "Actual PM 10", col = "coral2")
plot(sPM21[,2], y_predsvm10, xlab = "Actual", ylab = "Predicted PM 10", xlim=c(-1,1), ylim=c(-1,1), col = "coral")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21[,2]; p<-y_predsvm10
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

