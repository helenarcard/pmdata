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
set.seed(123)

#Train data is full dataset 2015-2020, test data is 2021 dataset minus pm data 
Train = sPMd
Test = sPM21[,-c(1,2)] 



#NEURAL NET FOR FULL DATASET
#nn for PM 2.5 
nn2 <- neuralnet(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, Train, hidden = 1, linear.output = FALSE)

#nn plot 
plot(nn2)

# Make predictions on Test data
y_prednn2 <- predict(nn2, newdata = Test)
print(y_prednn2)

#plotting predicted y_hat vs actual y from test data PM 2.5 values 
plot(y_prednn2, sPM21[,1], main = "PM 2.5 VS NN Prediction for 2021", xlab = "Predicted", ylab = "Actual PM 2.5")

#R SQUARED error metric -- Coefficient of Determination
postResample(y_prednn2, sPM21[,1])
