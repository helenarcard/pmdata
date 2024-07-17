#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)

install.packages("neuralnet")
library(tidyverse)
library(neuralnet)
library(corrplot)

data <- read.csv("C:/Users/patron/Desktop/DataPMyosemitestation.csv")
head(data)
PMdata <- (data[7:15])
PMdata1 <- (PMdata %>% drop_na())
head(PMdata1)
nrow(PMdata1)

set.seed(123)

split = sample.split(PMdata1$PMTWO, SplitRatio = 0.75) 

Train = subset(PMdata1, split == TRUE) 
Test = subset(PMdata1, split == FALSE) 
nn <- neuralnet(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WDF5 + WSF2 + WSF5, Train, hidden = 1, linear.output = FALSE)


print(nn)


plot(nn)

# Make predictions on test data
#predicted <- round(predict(nn, Test))

# Evaluate the model
#confusion_matrix <- table(predicted, Test$PMTWO)
#accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
#print(confusion_matrix)
#print(paste("Accuracy:", accuracy))
