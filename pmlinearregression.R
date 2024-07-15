r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
install.packages("ggplot2")
install.packages("GGally")
install.packages("psych")
install.packages("Hmisc")
install.packages("corrplot")
library(tidyverse)
library(readxl)
library(Hmisc)
library(corrplot)

#loading data
data1 <- read_excel("C:/Users/patron/Desktop/noblanksdatacollectionmastersheet.xlsx", sheet = 1)
head(data1)
my_data <- (data1[7:15])
#scatterplot matrix 
pairs(my_data, main = "Scatterplot Matrix for PM Data")

#Multiple linear regression for PM 2.5 
pmtwo.regression <- lm(PMTWO ~ AWND + PRCP + TAVG + WDF2 + WDF5 + WSF2 + WSF5, data = data1)
summary(pmtwo.regression)

#Multiple linear regression for PM 10 
pmten.regression <- lm(PMTEN ~ AWND + PRCP + TAVG + WDF2 + WDF5 + WSF2 + WSF5, data = data1)
summary(pmten.regression)

#compute correlation matrix 
head(my_data)
res <- cor(my_data, use = "complete.obs")
round(res, 2)

res2 <- rcorr(as.matrix(my_data))
res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
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
res2<-rcorr(as.matrix(my_data))
flattenCorrMatrix(res2$r, res2$P)

#corrplot heatmap correlation matrix 
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt =)
#Positive correlations are displayed in blue and negative correlations in red color. 
#Color intensity and the size of the circle are proportional to the correlation coefficients. 
#In the right side of the correlogram, the legend color shows the correlation coefficients 
#and the corresponding colors.

#neural network prac 
apply(my_data,2,function(x) sum(is.na(x)))
#use = "complete.obs"
#index <- sample(1:nrow(my_data),round(0.75*nrow(my_data)))
#train <- my_data[index,]
#test <- my_data[-index,]
#lm.fit <- glm(medv~., my_data = train)
#summary(lm.fit)
