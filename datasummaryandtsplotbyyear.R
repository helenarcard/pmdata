#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)
install.packages("Hmisc")
install.packages("corrplot")
install.packages("fpp2")
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(fpp2)

#loading data 2015 to 2020 only 
df <- read.csv("C:/Users/patron/Desktop/DataPMyosemitestation.csv")
head(df)
TrainPM <- (df[7:8])
#summary of PM 2.5 and PM 10 columns from 2015 to 2020 
summary(TrainPM)

#loading data 2021 only 
df21 <- read.csv("C:/Users/patron/Desktop/2021pmdataexcess.csv")
head(df21)
dataPM21 <- (df21[7:8])
#summary of PM 2.5 and PM 10 columns from 2021 
summary(dataPM21)

#Time series plot by year 
#TS 2015  
data15 <- read.csv("C:/Users/patron/Desktop/2015pmdata.csv")
head(data15)
pm2.5_2015 <- (data15[7])
ts.plot(pm2.5_2015)
pm10_2015 <- (data15[8])
ts.plot(pm10_2015)

#TS 2016  
data16 <- read.csv("C:/Users/patron/Desktop/2016pmdata.csv")
head(data16)
pm2.5_2016 <- (data16[7])
ts.plot(pm2.5_2016)
pm10_2016 <- (data16[8])
ts.plot(pm10_2016)

#TS 2017  
data17 <- read.csv("C:/Users/patron/Desktop/2017pmdata.csv")
head(data17)
pm2.5_2017 <- (data17[7])
ts.plot(pm2.5_2017)
pm10_2017 <- (data17[8])
ts.plot(pm10_2017)

#TS 2018  
data18 <- read.csv("C:/Users/patron/Desktop/2018pmdata.csv")
head(data18)
pm2.5_2018 <- (data18[7])
ts.plot(pm2.5_2018)
pm10_2018 <- (data18[8])
ts.plot(pm10_2018)

#TS 2019  
data19 <- read.csv("C:/Users/patron/Desktop/2019pmdata.csv")
head(data19)
pm2.5_2019 <- (data19[7])
ts.plot(pm2.5_2019)
pm10_2019 <- (data19[8])
ts.plot(pm10_2019)

#TS 2020  
data20 <- read.csv("C:/Users/patron/Desktop/2020pmdata.csv")
head(data20)
pm2.5_2020 <- (data20[7])
ts.plot(pm2.5_2020)
pm10_2020 <- (data20[8])
ts.plot(pm10_2020)

