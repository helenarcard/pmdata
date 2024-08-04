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
set.seed(2)

#Train data is full dataset 2015-2020, test data is 2021 dataset minus pm data 
Train = sPMd
Test = sPM21[,-c(1,2)] 



#NEURAL NET FOR FULL DATASET
#nn for PM 2.5 
nn2 <- neuralnet(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, 
                 Train, 
                 hidden = 1, 
                 linear.output = FALSE)

#nn plot 
plot(nn2)

# Make predictions on Test data
y_prednn2 <- predict(nn2, newdata = Test)
head(y_prednn2)

#plotting predicted y_hat vs actual y from test data PM 2.5 values 
plot(y_prednn2, sPM21[,1], main = "PM 2.5 VS NN Prediction 2021", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue3")
plot(sPM21[,1], y_prednn2, xlab = "Actual", ylab = "Predicted PM2.5", xlim=c(-1,1), ylim=c(-1,1), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21[,1]; p<-y_prednn2
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
nn10 <- neuralnet(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, 
                  Train, 
                  hidden = 2, 
                  linear.output = FALSE)

#nn plot 
plot(nn10)

# Make predictions on Test data
y_prednn10 <- predict(nn10, newdata = Test)
head(y_prednn10)

#plotting predicted y_hat vs actual y from test data PM 10 values 
plot(y_prednn10, sPM21[,2], main = "PM 10 VS NN Prediction 2021", xlab = "Predicted", ylab = "Actual PM 10", col = "coral3")
plot(sPM21[,2], y_prednn10, xlab = "Actual", ylab = "Predicted PM 10", xlim=c(-1,1), ylim=c(-1,1), col = "coral")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-sPM21[,2]; p<-y_prednn10
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

