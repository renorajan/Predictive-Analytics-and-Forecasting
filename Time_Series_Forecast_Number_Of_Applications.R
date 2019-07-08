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
f1_check=file.exists("C:\\Users\\Shwetha Ak\\Desktop\\Coursework\\Sem2-Spring 2018\\R and Python\\Project\\H-1B_Disclosure_Data_FY15.csv")

#### Open the files and data of each financial year to a data frame
h1bfy15=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\H-1B_Disclosure_Data_FY15.csv", header=T, sep=",")
h1bfy16=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\H-1B_Disclosure_Data_FY16.csv", header=T, sep=",")
#h1bfy16=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\H-1B_Disclosure_Data_FY16_Mar 14.csv", header=T, sep=",")
h1bfy17=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\H-1B_Disclosure_Data_FY17.csv", header=T, sep=",")

names(h1bfy17)

pgm_df=sqldf('select EMPLOYER_COUNTRY,count(CASE_NUMBER) as "No_of_App" from final_consolidated_H1B group by EMPLOYER_COUNTRY order by EMPLOYER_COUNTRY DESC' )

head(pgm_df)

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

# Excluding Wages with zero and less than 20000
#data2$WAGE_RATE_OF_PAY_FROM[data2$WAGE_RATE_OF_PAY_FROM== '' | data2$WAGE_RATE_OF_PAY_FROM==' '] <- NA
#data2$WAGE_RATE_OF_PAY_TO[data2$WAGE_RATE_OF_PAY_TO== '' | data2$WAGE_RATE_OF_PAY_TO==' '|data2$WAGE_RATE_OF_PAY_TO==0] <- NA

data2$WAGE_RATE_OF_PAY_FROM[data2$WAGE_RATE_OF_PAY_FROM==0] <- NA

data3=na.omit(data2)
nrow(data3)
summary(data3)

# Excluding invalid datas
data4=data2[data2$WAGE_RATE_OF_PAY_FROM>20000,]
#data4=data4[data4$WAGE_RATE_OF_PAY_FROM<250000,]
nrow(data4)
sapply(data4,typeof)

# Converting Date column into character to convert into month year format for subsequent time series analysis
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

#### Obtaining COUNT OF APPLICATIONS as a time series with time being in Month year format
pgm_df1=sqldf('select CASE_SUBMITTED,count(*)as "APP_COUNT" from data5 group by CASE_SUBMITTED order by CASE_SUBMITTED ASC' )

head(pgm_df1)
pgm_df1

# We are analyzing the H1B LCA application submissions from the year 2015 onwards
cnt_app=pgm_df1[53:85,]
appcnt_ts=ts(cnt_app$APP_COUNT,frequency=12,start=c(2015,1))
head(appcnt_ts,75)

#appcnt_ts contains app for the H1B LCA from year 2015 till 2017
appcnt_ts 

# Assessment of Seasonality
# using boxplot 
boxplot(appcnt_ts ~ cycle(appcnt_ts))

appcnt_ts_dec=decompose(appcnt_ts)
plot(appcnt_ts_dec)

# Using Std Dev ...
sd(appcnt_ts)

sd(appcnt_ts-appcnt_ts_dec$seasonal)

# Part 1: Using a Simple Moving Average to model the number of LCA applications.

app_ma1=ma(appcnt_ts,order=2,centre=FALSE)
app_ma1

app_ma2=ma(appcnt_ts,order=5,centre=FALSE)
app_ma2

#  Plotting Data with Moving average models given 
plot(appcnt_ts,main="No of LCA applications")
lines(app_ma1,col='red')
lines(app_ma2,col='green')
typeof(app_ma1)

app_ma1=na.omit(app_ma1)
app_ma2=na.omit(app_ma2)

plot(appcnt_ts,main="LCA application Time Series data")

# Forecast for the Moving Avg models
app_ma1_fc=forecast(app_ma1,h=9)
plot(app_ma1_fc,main=" Moving Avg order 2")
app_ma1_fc
typeof(app_ma1_fc)
class(app_ma1_fc)

app_ma2_fc=forecast(app_ma2,h=9)
plot(app_ma2_fc,main=" Moving Avg order 5")
app_ma2_fc
app_ma2_fc$mean

# Part 2: Using HoltWinters to model the number of LCA applications.

# Trend adjusted exponential smoothing based models...?
par(mfrow = c(1, 1))
hw_1= HoltWinters(appcnt_ts, beta = 0.75, gamma = 0.9, alpha = 0.9)
plot(appcnt_ts)
plot(hw_1,main="beta = 0.75, gamma = 0.9, alpha = 0.9")

#Using HoltWinters to predict and access the model
par(mfrow=c(1,1))
hwm=HoltWinters(appcnt_ts)
plot(hwm)

#Forecast the model beyond the known range of data
# Model 1  # Our Model 
hw_1_fc = forecast(hw_1,h=9)
plot(hw_1_fc)

#t=hw_1_fc$mean
#t

#Assess constant variance
plot(hw_1_fc$residuals,main="Variance")
lines(c(2015, 2019), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(na.omit(hw_1_fc$residuals),'Assessing Normal Distribution')

#Model 2 # With HoltWinters suggested paramters 
hwm=HoltWinters(appcnt_ts)
hwm_fc = forecast(hwm,h=9)
plot(hwm_fc)

#Assess constant variance
plot(hwm_fc$residuals,main="Variance")
lines(c(2015, 2019), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(na.omit(hwm_fc$residuals),'Assessing Normal Distribution')
hwm_fc

#HW model Selection
par(mfrow = c(2, 2))

####### Comparison of HoltWinters based models

#plot(appcnt_ts,main="No of LCA applications")
#plot(hw_1_fc$mean,col='red')
#lines(hwm_fc$mean,col='green')

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
appcnt_trend=appcnt_ts-appcnt_ts_dec$seasonal
appcnt_trend_ts=data.frame(trend=c(appcnt_trend),time=c(time(appcnt_trend)))
class(appcnt_trend_ts)
appcnt_trend_reg=lm(appcnt_trend_ts$trend ~ appcnt_trend_ts$time)
summary(appcnt_trend_reg)

#=======================================
# Compare the means of the time series
# with and without the trend
#=======================================
#Remove season; assess mean for trend data
appcnt_trend=appcnt_ts-appcnt_ts_dec$seasonal
mean(appcnt_trend)
var(appcnt_trend)
plot(appcnt_trend)

#Remove trend and season; assess mean for data without trend
appcnt_ts_simple= appcnt_ts-appcnt_ts_dec$seasonal - appcnt_ts_dec$trend
mean(na.omit(appcnt_ts_simple))
var(na.omit(appcnt_ts_simple))

#=======================================
# Augmented Dickey–Fuller (ADF) t-test
#=======================================
adf.test(appcnt_trend, k = 5, alternative = "stationary")
#### Result: Not significant, indicating non-stationarity
 
#================================================
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
#================================================
kpss.test(appcnt_trend,lshort=FALSE)

#kpss.test(appcnt_trend,lshort=TRUE)
#### Result: Significant, indicating non-stationarity

# Autocorrelation Function
acf(appcnt_ts,lag.max=20)



#Without seasonal component
# Assessing acf after removing the seasonal component
appcnt=appcnt_ts-appcnt_ts_dec$seasonal
acf(appcnt,lag.max=20)
pacf(appcnt,lag.max=20)

# Determining Stationarity after differencing
 appcnt_diff1=diff(appcnt_trend,differences=1)
 appcnt_diff1=na.omit( appcnt_diff1)
#=======================================
# Augmented Dickey–Fuller (ADF) t-test
#=======================================
adf.test(appcnt_diff1, k = 3, alternative = "stationary")
#### Result: Not significant, indicating non-stationarity
 
#================================================
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
#================================================
kpss.test(appcnt_diff1,lshort=FALSE)

#kpss.test(appcnt_trend,lshort=TRUE)
#### Result: Significant, indicating non-stationarity

par(mfrow = c(2, 1))
 acf(appcnt_diff1,lag.max=20)
 pacf(appcnt_diff1,lag.max=20)	
					
#ARIMA Models 
#011
appcnt_m1= Arima(appcnt_trend, order = c(0, 1, 1), method = "ML")
appcnt_m1
coeftest(appcnt_m1)

appcnt_m2= Arima(appcnt_trend, order = c(1, 1, 0), method = "ML")
appcnt_m2
coeftest(appcnt_m2)

appcnt_m3= Arima(appcnt_trend, order = c(1, 1, 1), method = "ML")
appcnt_m3
coeftest(appcnt_m3)

# Forecasts of ARIMA models 
#ARIMA(011)
fc1=forecast(appcnt_m1,h=9)
accuracy(fc1)
fc1$mean

#ARIMA(110)
fc2=forecast(appcnt_m2,h=9)
accuracy(fc2)
fc2$mean

#ARIMA(111)
fc3=forecast(appcnt_m3,h=9)
accuracy(fc3)
fc3$mean

plot(fc1)

# For the Comparison of the 3 models with the Actual number of LCA applications from Sep 2017 to May 2018 :
# Reading the latest published data from the Govt Website for the 2017 untill March 2018

t2=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\Data Only for TimseSeries Actual\\H-1B_Disclosure_Data_FY17new.csv", header=T, sep=",")
t1=read.csv("D:\\Reno_MS\\Semester Spring 2018\\Programming for DataScience\\H1B_Dataset\\Data Only for TimseSeries Actual\\H-1B_Disclosure_Data_FY18new.csv", header=T, sep=",")

head(t1)

#Data Consolidation
	
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
data4=data4[data4$WAGE_RATE_OF_PAY_FROM<250000,]
nrow(data4)
sapply(data4,typeof)

data4$CASE_SUBMITTED=as.character(data4$CASE_SUBMITTED)
head(data4$CASE_SUBMITTED)

# Converting date column into Month year
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

#### Obtaining COUNT OF APPLICATIONS as a time series with time being in Month year format
pgm_df1=sqldf('select CASE_SUBMITTED,count(*)as "APP_COUNT" from data5 group by CASE_SUBMITTED order by CASE_SUBMITTED ASC' )

head(pgm_df1)
pgm_df1
tail(pgm_df1)

# Extracting actual application count from the month of september 2017 to March 2018
actual_sep17_2_March18=pgm_df1[64:69,]
actual_sep17_2_March18

# Actual Data
rownames(actual_sep17_2_March18)=1:nrow(actual_sep17_2_March18)
actual_sep17_2_March18
plot(actual_sep17_2_March18,type='l')
plot(fc1,type='l')

# Moving avg MA2
# Accounting for the lag effect of the MA model 
app_ma1_fc=forecast(app_ma1,h=7)
newappma1=Lag(app_ma1_fc$mean,1)
#Forecasted values MA model
newappma1=na.omit(newappma1)

plot(newappma1,col='red')

#Forecasted values Holtwinters model
hwm_fc$mean
plot(hwm_fc)

#ARIMA(011)
fc1=forecast(appcnt_m1,h=9)
accuracy(fc1)
fc1$mean
plot(fc1)

# For Doing Accuracy Assessment with actual data : Data period Sept/Oct 2017 to March 2018
# Extracting the specified timeframe to match with the forecasted timeframe
actual=ts(actual_sep17_2_March18$APP_COUNT,frequency=12,start=c(2017,10))
actual

#SMA forecast values extraction from Sep 17 to March 18
temp_SMA_MA2=window(newappma1,Start=c(2017,10),End=c(2018,3),Frequency=12)
temp_SMA_MA2
accuracy(actual,temp_SMA_MA2)

# Comparing MA ord 2 forecast with the actual data
plot(actual, main="Actual in Black; SMA model in Red")
lines(temp_SMA_MA2,col='red')

# Holt Winters Forecast Data Sep 17 to March 18
hwm=HoltWinters(appcnt_ts)
hwm_fc = forecast(hwm,h=6)
plot(hwm_fc)
hwm_fc$mean 
temp_hw=window(hwm_fc$mean,Start=c(2017,10),End=c(2018,3),Frequency=12)
accuracy(actual,temp_hw)

# Comparing holtwinters forecast with the actual data
plot(actual, main="Actual in Black;  HW model in Red")
lines(temp_hw,col='red')

#ARIMA(022)
#022
appcnt_m1= Arima(appcnt_trend, order = c(0, 2, 2), method = "ML")
appcnt_m1
coeftest(appcnt_m1)
fc1=forecast(appcnt_m1,h=5)
fc1$mean
plot(fc1)

# ARIMA data Oct 17 to March 18
temp_ar=window(fc1$mean,Start=c(2017,10),End=c(2018,2),Frequency=12)
accuracy(actual,temp_ar)

# Comparing ARIMA(011) forecast with the actual data
plot(actual, main="Actual in Black;  ARIMA model in Red")
lines(temp_ar,col='red')


