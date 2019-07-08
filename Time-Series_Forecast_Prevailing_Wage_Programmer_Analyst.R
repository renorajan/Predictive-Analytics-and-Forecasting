install.packages('psych')
install.packages('Hmisc')
install.packages('dplyr')
install.packages('zoo')
install.packages('xts')
install.packages('sqldf')
install.packages("ggplot2")
install.packages("ggplot")
install.packages("tree")
install.packages("scale")
install.packages("lmtest")
install.packages("tseries")

library(sqldf)
library(zoo)
library(xts)
library(psych)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(tree)
library(scales)
#library(ggplot)
library(lmtest)
library(tseries)

install.packages("forecast")
options(digits=4)   #Useability by way of rounding
library(forecast)

###Check for the presence of files prior to attempting to open it
#f1_check=file.exists("C:\\Users\\Shwetha Ak\\Desktop\\Coursework\\Sem2-Spring 2018\\R and Python\\Project\\H-1B_Disclosure_Data_FY15.csv")

#### Open the files and data of each financial year to a data frame
h1bfy15=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\H-1B_Disclosure_Data_FY15.csv", header=T, sep=",")
h1bfy16=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\H-1B_Disclosure_Data_FY16.csv", header=T, sep=",")
#h1bfy16=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\H-1B_Disclosure_Data_FY16_Mar 14.csv", header=T, sep=",")
h1bfy17=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\H-1B_Disclosure_Data_FY17.csv", header=T, sep=",")


###Consolidate all the data files and append it into single data frame
final_consolidated_H1B = rbind(h1bfy15,h1bfy16,h1bfy17)
names(final_consolidated_H1B)
head(final_consolidated_H1B)

### Extracting Case submission date, Wage detail and Job title
data1=final_consolidated_H1B[,c('CASE_SUBMITTED','WAGE_RATE_OF_PAY_FROM','JOB_TITLE')]
head(data1)
class(data1)
nrow(data1)
	
data2=na.omit(data1)
nrow(data2)
summary(data2)


head(data2)
sapply(data2,typeof)

# Excluding Wages with zero and less than 20000
#data2$WAGE_RATE_OF_PAY_FROM[data2$WAGE_RATE_OF_PAY_FROM== '' | data2$WAGE_RATE_OF_PAY_FROM==' '] <- NA
#data2$WAGE_RATE_OF_PAY_TO[data2$WAGE_RATE_OF_PAY_TO== '' | data2$WAGE_RATE_OF_PAY_TO==' '|data2$WAGE_RATE_OF_PAY_TO==0] <- NA

data2$WAGE_RATE_OF_PAY_FROM[data2$WAGE_RATE_OF_PAY_FROM==0] <- NA

data3=na.omit(data2)
nrow(data3)
summary(data3)

data4=data2[data2$WAGE_RATE_OF_PAY_FROM>20000,]
#data4=data4[data4$WAGE_RATE_OF_PAY_FROM<250000,]
nrow(data4)
sapply(data4,typeof)

data4$CASE_SUBMITTED=as.character(data4$CASE_SUBMITTED)
head(data4$CASE_SUBMITTED)

t=as.Date(data4$CASE_SUBMITTED,format="%m/%d/%Y")
head(t)
t=as.character(t)
head(t)
t1=substring(t,1,7)
head(t1)
typeof(t1)
data4$CASE_SUBMITTED=as.yearmon(t1)
head(data4$CASE_SUBMITTED)

head(data4)
nrow(data4)
na.omit(data4)
nrow(data4)
head(data4,25)

data5=data4

#sapply(data4,typeof)
#data4$CASE_SUBMITTED=as.Date(data4$CASE_SUBMITTED,format="%b %Y")
#sapply(data4,typeof)
#head(data4,7)

#### like PROGRAMMER ANALYST 
pgm_df=sqldf('select CASE_SUBMITTED,round(avg(WAGE_RATE_OF_PAY_FROM),0) as "AVG_WAGE" from data5 where JOB_TITLE like "%PROGRAMMER ANALYST%" group by CASE_SUBMITTED order by CASE_SUBMITTED ASC' )

head(pgm_df)
pgm_df

df1=pgm_df[6:74,]
df1

# Represent avg wage offered for H1B appplicants for the programmer analyst category
pgmts=ts(df1$AVG_WAGE,frequency=12,start=c(2012,1))
head(pgmts)
pgmts

# Assessment of Seasonality
# using boxplot 
par(mfrow = c(1, 1))
boxplot(pgmts ~ cycle(pgmts),main="Avg Wage offered Programmer Analyst by month")

pgmts_dec=decompose(pgmts)
plot(pgmts_dec)

# Using Std Dev ...
sd(pgmts)

sd(pgmts-pgmts_dec$seasonal)

# Part 1: Using a Simple Moving Average 

pgm_ma1=ma(pgmts,order=2,centre=FALSE)
pgm_ma1

pgm_ma2=ma(pgmts,order=5,centre=FALSE)
pgm_ma2

plot(pgmts,main="Avg Wage offered Programmer Analyst")
lines(pgm_ma1,col='red')
lines(pgm_ma2,col='green')

pgm_ma1=na.omit(pgm_ma1)
pgm_ma2=na.omit(pgm_ma2)

plot(pgmts,main="Avg Wage offered Programmer Analyst")

pgm_ma1_fc=forecast(pgm_ma1,h=9)
plot(pgm_ma1_fc,main=" Moving Avg order 2")
pgm_ma1_fc
typeof(pgm_ma1_fc)
class(pgm_ma1_fc)
accuracy(pgm_ma1_fc)

pgm_ma2_fc=forecast(pgm_ma2,h=9)
plot(pgm_ma2_fc,main=" Moving Avg order 5")
pgm_ma2_fc
pgm_ma2_fc$mean

par(mfrow = c(1, 1))

# Part 2: Using HW to model Avg wage
# Trend adjusted exponential smoothing 
par(mfrow = c(1, 1))
hw_1= HoltWinters(pgmts, beta = 0.1, gamma = 0.9, alpha = 0.3)
plot(pgmts)
plot(hw_1,main="beta = 0.1, gamma = 0.9, alpha = 0.3")

#Using HoltWinters to predict and access the model
par(mfrow=c(1,1))
hwm=HoltWinters(pgmts)
plot(hwm)
hwm

#Forecast the model beyond the known range of data
# Model 1
hw_1_fc = forecast(hw_1,h=9)
plot(hw_1_fc)

t=hw_1_fc$mean
t

#Assess constant variance
plot(hw_1_fc$residuals,main="Variance")
lines(c(2012, 2019), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(na.omit(hw_1_fc$residuals),'Assessing Normal Distribution')

#Model 2
hwm=HoltWinters(pgmts)
hwm_fc = forecast(hwm,h=9)
hwm_fc
plot(hwm_fc)

#Assess constant variance
plot(hwm_fc$residuals,main="Variance")
lines(c(2012, 2019), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(na.omit(hwm_fc$residuals),'Assessing Normal Distribution')
hwm_fc

#HW model Selection
par(mfrow = c(2, 2))

####### Comparison

#plot(appcnt_ts,main="Pgm Avg Wage")
#plot(hw_1_fc$mean,col='red')
#lines(hwm_fc$mean,col='green')

par(mfrow = c(2, 2))
plot(hw_1_fc,col='red')
plot(hwm_fc,col='green')
par(mfrow = c(1, 1))

newappma1=Lag(app_ma1_fc$mean,1)
#Forecasted values MA model
newappma1
#Forecasted values Holtwinters model
hwm_fc$mean
# Difference
difference=hwm_fc$mean-newappma1
difference

########

accuracy(hw_1_fc)
accuracy(hwm_fc)

plotForecastErrors = function(forecasterrors,forecasttitle) {
    #Function provided by Avril Coghlan
    forecasterrors = na.omit(forecasterrors)
    # make a histogram of the forecast errors:
    mybinsize = IQR(forecasterrors) / 4
    mysd = sd(forecasterrors)
    mymin = min(forecasterrors) - mysd * 5
    mymax = max(forecasterrors) + mysd * 3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(10000, mean = 0, sd = mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins, main=forecasttitle)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}

#========================================
# Use regression to assess stationarity 
# by regressing trend onto time
#========================================
pgm=pgmts-pgmts_dec$seasonal
pgm_trend_ts=data.frame(trend=c(pgm),time=c(time(pgm)))
class(pgmts)
pgm_trend_reg=lm(pgm_trend_ts$trend ~ pgm_trend_ts$time)
summary(pgm_trend_reg)




#=======================================
# Compare the means of the time series
# with and without the trend
#=======================================
#Remove season; assess mean for trend data
pgm=pgmts-pgmts_dec$seasonal
mean(pgm)
var(pgm)
plot(pgm)

#Remove trend and season; assess mean for data without trend
pgm_simple= pgmts-pgmts_dec$seasonal - pgmts_dec$trend
mean(na.omit(pgm_simple))
var(na.omit(pgm_simple))

#=======================================
# Augmented Dickey–Fuller (ADF) t-test
#=======================================
adf.test(pgm, k = 3, alternative = "stationary")
#### Result: Not significant, indicating non-stationarity
 
#================================================
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
#================================================
kpss.test(pgm,lshort=FALSE)

#### Result: Significant, indicating non-stationarity

# Autocorrelation Function
acf(pgmts,lag.max=20)



#Without seasonal component
# Assessing acf after removing the seasonal component
pgmnew=pgmts-pgmts_dec$seasonal
acf(pgmnew,lag.max=20)
pacf(pgmnew,lag.max=20)

# Determining Stationarity after differencing
 pgm_diff1=diff(pgmnew,differences=1)
 pgm_diff1=na.omit(pgm_diff1)
#=======================================
# Augmented Dickey–Fuller (ADF) t-test
#=======================================
adf.test(pgm_diff1, k = 3, alternative = "stationary")
#### Result: Not significant, indicating non-stationarity
 
#================================================
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
#================================================
kpss.test(pgm_diff1,lshort=FALSE)

#kpss.test(pgm_diff1,lshort=TRUE)
#### Result: Significant, indicating non-stationarity

par(mfrow = c(1, 1))
 acf(pgm_diff1,lag.max=20)
 pacf(pgm_diff1,lag.max=20)	
					
#ARIMA Models 
#011
pgm_m1= Arima(pgmnew, order = c(0, 1, 1), method = "ML")
pgm_m1
coeftest(pgm_m1)

#110
pgm_m2= Arima(pgmnew, order = c(1, 1, 0), method = "ML")
pgm_m2
coeftest(pgm_m2)

#111
#pgm_m3= Arima(pgmnew, order = c(1, 1, 1), method = "ML")
#pgm_m3
#coeftest(pgm_m3)

#313
pgm_m4= Arima(pgmnew, order = c(3, 1, 3), method = "ML")
pgm_m4
coeftest(pgm_m4)

#213
pgm_m5= Arima(pgmnew, order = c(2, 1, 3), method = "ML")
pgm_m5
coeftest(pgm_m5)

# Assessing AIC

# Forecasts 
#ARIMA(011)
fc1=forecast(pgm_m1,h=9)
accuracy(fc1)
fc1$mean

#ARIMA(110)
fc2=forecast(pgm_m2,h=9)
accuracy(fc2)
fc2$mean

#ARIMA(111)
#fc3=forecast(pgm_m3,h=9)
#accuracy(fc3)
#fc3$mean

#ARIMA(313)
fc4=forecast(pgm_m4,h=9)
accuracy(fc4)
fc4$mean

#ARIMA(213)
fc5=forecast(pgm_m5,h=9)
accuracy(fc5)
fc5$mean

# Forecast plots
plot(pgm_ma1_fc)
accuracy(pgm_ma1_fc)

hwm=HoltWinters(pgmts)
hwm_fc = forecast(hwm,h=9)
hwm_fc
plot(hwm_fc)
accuracy(hwm_fc)

#213
pgm_m5= Arima(pgmnew, order = c(2, 1, 3), method = "ML")
pgm_m5
coeftest(pgm_m5)
fc5=forecast(pgm_m5,h=9)
plot(fc5)
accuracy(fc5)
fc5$mean
#################
# Comparison of the 3 models with the Actual number of LCA applications from Sep 2017 to May 2018 :

t2=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\Data Only for TimseSeries Actual\\H-1B_Disclosure_Data_FY17new.csv", header=T, sep=",")
t1=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\Data Only for TimseSeries Actual\\H-1B_Disclosure_Data_FY18_v2.csv", header=T, sep=",")

head(t1)
	
final_consolidated_H1B = rbind(t1,t2)
names(final_consolidated_H1B)
head(final_consolidated_H1B)
nrow(final_consolidated_H1B)
typeof(final_consolidated_H1B)
dtypes(final_consolidated_H1B)
sapply(final_consolidated_H1B,class)


### Extracting Case submission date, Wage detail and Job title
data1=final_consolidated_H1B[,c('CASE_SUBMITTED','WAGE_RATE_OF_PAY_FROM','JOB_TITLE')]
head(data1)
class(data1)
nrow(data1)

data2=na.omit(data1)
nrow(data2)
summary(data2)
sapply(data2,typeof)

data2$CASE_SUBMITTED=as.character(data2$CASE_SUBMITTED)
data2$WAGE_RATE_OF_PAY_FROM=as.numeric(data2$WAGE_RATE_OF_PAY_FROM)
data2$JOB_TITLE=as.character(data2$JOB_TITLE)

# Excluding Wages with zero and less than 20000
#data2$WAGE_RATE_OF_PAY_FROM[data2$WAGE_RATE_OF_PAY_FROM== '' | data2$WAGE_RATE_OF_PAY_FROM==' '] <- NA
#data2$WAGE_RATE_OF_PAY_TO[data2$WAGE_RATE_OF_PAY_TO== '' | data2$WAGE_RATE_OF_PAY_TO==' '|data2$WAGE_RATE_OF_PAY_TO==0] <- NA

data2$WAGE_RATE_OF_PAY_FROM[data2$WAGE_RATE_OF_PAY_FROM==0 ] <- NA

data3=na.omit(data2)
nrow(data3)
summary(data3)

data4=data2[(data2$WAGE_RATE_OF_PAY_FROM>20000),]
#data4=data4[data4$WAGE_RATE_OF_PAY_FROM<250000,]
nrow(data4)
sapply(data4,typeof)

data4$CASE_SUBMITTED=as.character(data4$CASE_SUBMITTED)
head(data4$CASE_SUBMITTED)

t=as.Date(data4$CASE_SUBMITTED,format="%m/%d/%Y")
head(t)
t=as.character(t)
head(t)
t1=substring(t,1,7)
head(t1)
typeof(t1)
data4$CASE_SUBMITTED=as.yearmon(t1)
head(data4$CASE_SUBMITTED)

head(data4)
nrow(data4)
na.omit(data4)
nrow(data4)
head(data4,25)

data5=data4

#sapply(data4,typeof)
#data4$CASE_SUBMITTED=as.Date(data4$CASE_SUBMITTED,format="%b %Y")
#sapply(data4,typeof)
#head(data4,7)

#### like PROGRAMMER ANALYST 
pgm_df=sqldf('select CASE_SUBMITTED,round(avg(WAGE_RATE_OF_PAY_FROM),0) as "AVG_WAGE" from data5 where JOB_TITLE like "%PROGRAMMER%" group by CASE_SUBMITTED order by CASE_SUBMITTED ASC' )

head(pgm_df)
pgm_df

df1=pgm_df[6:74,]
df1 

#######################################3333




