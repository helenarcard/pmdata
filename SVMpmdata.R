#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)

install.packages("e1071")
install.packages('caTools')
install.packages("corrplot")
library(caTools) 
require(e1071) #Contains the SVM 
library(tidyverse)
library(corrplot)

#getting data from excel
data <- read.csv("C:/Users/patron/Desktop/DataPMyosemitestation.csv")
head(data)
#specifying columns 
PMdata <- (data[6:15])
#removing na values
PMdata1 <- (PMdata %>% drop_na())
#check to make sure data looks right and number of rows is not too few 
head(PMdata1)
nrow(PMdata1)

# Encoding the target feature as factor
#data_levels <- c("PRCP", "AWND", "WSF2", "WSF5", "TAVG", "WDF2", "WDF5")
#PMdata1$PMTWO = factor(PMdata1$PMTWO, levels = data_levels)

#setting seed so random can be reproduced 
set.seed(1) 
#splitting data 75/25 ratio 
split = sample.split(PMdata1, SplitRatio = 0.75) 

Train = subset(PMdata1, split == TRUE) 
Test = subset(PMdata1, split == FALSE) 

#SVM formula 
svm.mod = svm(formula = PMTWO ~ AWND + PRCP + TAVG + WSF2 + WDF2, 
                 data = Train, 
                 type = 'nu-regression', 
                 kernel = 'linear') 
print(svm.mod)


# Predicting the Test set results 
y_pred = predict(svm.mod, newdata = Test)

print(y_pred)
