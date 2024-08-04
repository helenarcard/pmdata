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

#loading HOT dataset for training 
dfhot <- read.csv("C:/Users/patron/Desktop/hotseasonpmdata.csv")
head(dfhot)
datahot <- (dfhot[7:13])
PMdhot <- (datahot %>% drop_na())
sPMdhot <- scale(PMdhot)
head(sPMdhot)
nrow(sPMdhot)

#loading 2021 HOT dataset for testing
df21hot <- read.csv("C:/Users/patron/Desktop/2021hotseason.csv")
PMd21hot <- (df21hot[7:13])
PMd21cleanhot <- (PMd21hot %>% drop_na())
sPM21hot <- scale(PMd21cleanhot)
head(sPM21hot)
nrow(sPM21hot)

#setting seed so random can be reproduced 
set.seed(5)

#Train data is hot dataset 2015-2020, test data is 2021 hot dataset minus pm data 
Train = sPMdhot
Test = sPM21hot[,-c(1,2)] 



#NEURAL NET FOR HOT DATASET
#nn for PM 2.5 
nn2hot <- neuralnet(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, 
                     Train, 
                     hidden = 1, 
                     linear.output = FALSE)

#nn plot 
plot(nn2hot)

# Make predictions on Test data
y_prednn2hot <- predict(nn2hot, newdata = Test)
head(y_prednn2hot)

#plotting predicted y_hat vs actual y from test data PM 2.5 values 
plot(y_prednn2hot, sPM21hot[,1], main = "PM 2.5 VS NN Prediction 2021", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue3")
plot(sPM21hot[,1], y_prednn2hot, xlab = "Actual", ylab = "Predicted PM2.5", xlim=c(-1,1), ylim=c(-1,1), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21hot[,1]; p<-y_prednn2hot
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
nn10hot <- neuralnet(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, 
                      Train, 
                      hidden = 1, 
                      linear.output = FALSE)

#nn plot 
plot(nn10hot)

# Make predictions on Test data
y_prednn10hot <- predict(nn10hot, newdata = Test)
head(y_prednn10hot)

#plotting predicted y_hat vs actual y from test data PM 10 values 
plot(y_prednn10hot, sPM21hot[,2], main = "PM 10 VS NN Prediction 2021", xlab = "Predicted", ylab = "Actual PM 10", col = "coral3")
plot(sPM21hot[,2], y_prednn10hot, xlab = "Actual", ylab = "Predicted PM 10", xlim=c(-1,1), ylim=c(-1,1), col = "coral")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21hot[,2]; p<-y_prednn10hot
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

