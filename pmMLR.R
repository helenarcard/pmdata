#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)
install.packages("ggplot2")
install.packages("GGally")
install.packages("psych")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("caret")
library(caret)
library(tidyverse)
library(Hmisc)
library(corrplot)

#loading full dataset 
df <- read.csv("C:/Users/patron/Desktop/DataPMyosemitestation.csv")
head(df)
data <- (df[7:13])

set.seed(3)

#scatterplot matrix
pairs(data, main = "Scatterplot Matrix for PM Data", col = "darkblue")




#MULTIPLE LINEAR REGRESSION FOR WHOLE DATASET
#MLR for PM 2.5 
pmtwo.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = df)
summary(pmtwo.mlr)

#MLR for PM 10 
pmten.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = df)
summary(pmten.mlr)





#predicting 2021 PM data from Full dataset MLR
#loading data 2021 only 
df21 <- read.csv("C:/Users/patron/Desktop/2021pmdataexcess.csv")
head(df21)
PMd21 <- (df21[7:13])
head(PMd21)
testPMd21 = PMd21[,-c(1,2)] 

#actual MLR for PM 2.5 2021 only
pmtwo21.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = df)
summary(pmtwo21.mlr)

#predicting PM 2.5 2021 using object pmtwo.mlr and PMd21 data 
y_predmlr2 = predict(pmtwo.mlr, newdata = testPMd21)
head(y_predmlr2)
head(PMd21$PMTWO)


#plotting predicted y_hat vs actual y from 2021 data PM 2.5 values 
plot(y_predmlr2, PMd21$PMTWO, main = "Actual PM 2.5 VS MLR Prediction 2021", xlab = "Predicted", ylab = "Actual PM 2.5", col = "royalblue")


#R SQUARED error metric -- Coefficient of Determination
postResample(y_predmlr2, PMd21$PMTWO)



#actual MLR for PM 10 2021 only 
pmten21.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = df)
summary(pmten21.mlr)

#predicting PM 10 2021 using object pmten.mlr and PMd21 data 
y_predmlr10 = predict(pmten.mlr, newdata = testPMd21)
head(y_predmlr10)
head(PMd21$PMTEN)


#plotting predicted y_hat vs actual y from 2021 data PM 10 values 
plot(y_predmlr10, PMd21$PMTEN, main = "Actual PM 10 VS MLR Prediction for 2021", xlab = "Predicted", ylab = "Actual PM 10", col = "coral")

#R SQUARED error metric -- Coefficient of Determination
postResample(y_predmlr10, PMd21$PMTEN)




#HEAT MAP CORRELATION MATRIX 
#compute correlation matrix 
head(data)
res <- cor(data, use = "complete.obs")
round(res, 2)

res2 <- rcorr(as.matrix(data))
res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P


# flattenCorrMatrix
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res2<-rcorr(as.matrix(data))
flattenCorrMatrix(res2$r, res2$P)

#corrplot heatmap correlation matrix 
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt =)
#Positive correlations are displayed in blue and negative correlations in red color. 
#Color intensity and the size of the circle are proportional to the correlation coefficients. 
#In the right side of the correlogram, the legend color shows the correlation coefficients 
#and the corresponding colors.

