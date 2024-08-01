#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)
install.packages("ggplot2")
install.packages("e1071")
install.packages('caTools')
install.packages("corrplot")
library(ggplot2)
install.packages("caret")
library(caret)
library(caTools) 
require(e1071) #Contains the SVM 
library(tidyverse)
library(corrplot)

#loading full dataset for training 
df <- read.csv("C:/Users/patron/Desktop/DataPMyosemitestation.csv")
head(df)
#specifying columns 
data <- (df[7:13])
#removing na values
PMd <- (data %>% drop_na())
sPMd <- scale(PMd)
#check to make sure data looks right and number of rows is not too few 
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

print(y_predsvm2)

#plotting predicted y_hat vs actual y from test data PM 2.5 values 
plot(y_predsvm2, sPM21[,1], main = "PM 2.5 VS SVM Prediction 2021", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue3")

#R SQUARED error metric -- Coefficient of Determination
postResample(y_predsvm2, sPM21[,1])



#SVM for PM 10
svm.mod10 = svm(formula = PMTEN ~ AWND + PRCP + TAVG + WSF2 + WDF2, 
               data = Train, 
               type = 'nu-regression', 
               kernel = 'linear') 
print(svm.mod10)


#Predicting the Test set results (y_hat value)
y_predsvm10 = predict(svm.mod10, newdata = Test)

print(y_predsvm10)

#plotting predicted y_hat vs actual y from test data PM 10 values 
plot(y_predsvm10, sPM21[,2], main = "PM 10 VS SVM Prediction 2021", xlab = "Predicted", ylab = "Actual PM 10", col = "coral2")

#R SQUARED error metric -- Coefficient of Determination
postResample(y_predsvm10, sPM21[,2])

