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
s.data<-as.data.frame(scale(data))

#scatterplot matrix
pairs(data, main = "Scatterplot Matrix for PM Data", col = "darkblue")


#MULTIPLE LINEAR REGRESSION FOR WHOLE DATASET
#MLR for PM 2.5 
pmtwo.mlr <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = s.data)
summary(pmtwo.mlr)

#MLR for PM 10 
pmten.mlr <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WSF2, data = s.data)
summary(pmten.mlr)


#predicting 2021 PM data from Full dataset MLR
#loading data 2021 only 
df21 <- read.csv("C:/Users/patron/Desktop/2021pmdataexcess.csv")
head(df21); df21<-df21[7:13]
PMd21 <- df21[complete.cases(df21),]
s.PMd21<-as.data.frame(scale(PMd21))


#predicting PM 2.5 2021 using object pmtwo.mlr and PMd21 data 
y_predmlr2 = predict(pmtwo.mlr, newdata = s.PMd21)

#plotting predicted y_hat vs actual y from 2021 data PM 2.5 values 
plot(s.PMd21$PMTWO, y_predmlr2, xlab = "Actual", ylab = "Predicted PM2.5", xlim=c(-1,1), ylim=c(-1,1), col = "royalblue")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-s.PMd21$PMTWO; p<-y_predmlr2
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

#predicting PM 10 2021 using object pmten.mlr and PMd21 data 
y_predmlr10 = predict(pmten.mlr, newdata = s.PMd21)

#plotting predicted y_hat vs actual y from 2021 data PM 10 values 
plot(s.PMd21$PMTEN, y_predmlr10, xlab = "Actual", ylab = "Predicted PM10", xlim=c(-1,1), ylim=c(-1,1), col = "coral")
abline(0,1)

#R SQUARED error metric -- Coefficient of Determination
y<-s.PMd21$PMTEN; p<-y_predmlr10
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

