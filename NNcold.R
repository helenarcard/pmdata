#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)

install.packages("neuralnet")
install.packages("caret")
library(caret)
library(tidyverse)
library(neuralnet)
library(corrplot)
library(caTools)

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
set.seed(4)

#Train data is COLD dataset 2015-2020, test data is 2021 COLD dataset minus pm data 
Train = sPMdcold
Test = sPM21cold[,-c(1,2)] 



#NEURAL NET FOR COLD DATASET
#nn for PM 2.5 
nn2cold <- neuralnet(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, 
                 Train, 
                 hidden = 1, 
                 linear.output = FALSE)

#nn plot 
plot(nn2cold)

# Make predictions on Test data
y_prednn2cold <- predict(nn2cold, newdata = Test)
head(y_prednn2cold)

#plotting predicted y_hat vs actual y from test data PM 2.5 values 
plot(y_prednn2cold, sPM21cold[,1], main = "PM 2.5 VS NN Prediction 2021", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue3")
plot(sPM21cold[,1], y_prednn2cold, xlab = "Actual", ylab = "Predicted PM2.5", xlim=c(-1,1), ylim=c(-1,1), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21cold[,1]; p<-y_prednn2cold
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


#nn for PM 10 
nn10cold <- neuralnet(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, 
                  Train, 
                  hidden = 1, 
                  linear.output = FALSE)

#nn plot 
plot(nn10cold)

# Make predictions on Test data
y_prednn10cold <- predict(nn10cold, newdata = Test)
head(y_prednn10cold)

#plotting predicted y_hat vs actual y from test data PM 10 values 
plot(y_prednn10cold, sPM21cold[,2], main = "PM 10 VS NN Prediction 2021", xlab = "Predicted", ylab = "Actual PM 10", col = "coral3")
plot(sPM21cold[,2], y_prednn10cold, xlab = "Actual", ylab = "Predicted PM 10", xlim=c(-1,1), ylim=c(-1,1), col = "coral")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21cold[,2]; p<-y_prednn10cold
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

