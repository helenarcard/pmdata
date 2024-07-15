r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

install.packages("e1071")
install.packages('caTools') 
library(caTools) 
require(e1071) #Contains the SVM 
library(readxl)
library(tidyverse)

data <- read_excel("C:/Users/patron/Desktop/noblanksdatacollectionmastersheet.xlsx", sheet = 1)
head(data)
PMdata <- (data[6:15])
PMdata1 <- (PMdata %>% drop_na())
head(PMdata1)
nrow(PMdata1)
# Encoding the target feature as factor
data_levels <- c("PRCP", "AWND", "WSF2", "WSF5", "TAVG", "WDF2", "WDF5")
PMdata1$PMTWO = factor(PMdata1$PMTWO, levels = data_levels)

set.seed(1) 
split = sample.split(PMdata1$PMTWO, SplitRatio = 0.75) 

Train = subset(PMdata1, split == TRUE) 
Test = subset(PMdata1, split == FALSE) 

classifier = svm(formula = PMTWO ~ ., 
                 data = Train, 
                 type = 'C-classification', 
                 kernel = 'linear') 
