####

#SLIM0405 Traditional Time Series Analysis

####
getwd()
install.packages("astsa")
install.packages("tseries")
install.packages("FitAR")
install.packages("forecast")
library(FitAR)
library(forecast)
library(tseries)
library(gplots)
library(aTSA)
library(astsa)
library(lmtest)

# Read in the data, and provide overview of the variables
foodcorp.df <-read.csv("../SLIM0405DataSeries.csv")


food.df <- read.csv("../SLIM0405DataSeries.csv")

str(foodcorp.df)


# Overview of the essentials
summary(foodcorp.df)



# Example 1: Autoregressive processes


plot(foodcorp.df[,c(1)],foodcorp.df[,c(2)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales over time")

adf.test(foodcorp.df[,c(2)])
foodacf = acf(foodcorp.df[,c(2)], lag.max = 10)
foodpacf = pacf(foodcorp.df[,c(2)], lag.max = 10)

bartry = Reduce(rbind,foodacf)
bartry2 = t(bartry)
bartry3 = as.numeric(bartry2[,c(1)])
bartryb = Reduce(rbind,foodpacf)
bartry2b = t(bartryb)
bartry3b = as.numeric(bartry2b[,c(1)])

bartry4 = as.matrix(bartry3)
bartry4help = t(bartry4)
bartry4help = bartry4help[,c(2,3,4,5,6,7,8,9,10,11)]
bartry4 = t(bartry4help)
bartry4 = t(bartry4)
bartry4b = as.matrix(bartry3b)
bartry5 = cbind(bartry4,bartry4b)
bartry6 = t(bartry5)
barplot(t(bartry4), ylim = c(-1,1), main = "ACF", col = "red")
barplot(t(bartry4b), ylim = c(-1,1), main = "PACF", col = "red")
barplot(bartry6, ylim = c(-1,1), main = "ACF (red) and PACF (grey) function", col = c("red", "grey"), beside = TRUE)

foodsel = foodcorp.df[,c(2)]
tsfoodsel = as.ts(foodsel)
foodarima <-arima(tsfoodsel, order = c(1,0,0) )
coeftest(foodarima)
auto.arima(tsfoodsel, test = c("adf"), trace = TRUE)
auto.arima(tsfoodsel, d=0, trace = TRUE)






# Example 2: Moving average processes

plot(foodcorp.df[,c(1)],foodcorp.df[,c(3)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales over time")

adf.test(foodcorp.df[,c(3)])
foodacf = acf(foodcorp.df[,c(3)], lag.max = 10)
foodpacf = pacf(foodcorp.df[,c(3)], lag.max = 10)

bartry = Reduce(rbind,foodacf)
bartry2 = t(bartry)
bartry3 = as.numeric(bartry2[,c(1)])
bartryb = Reduce(rbind,foodpacf)
bartry2b = t(bartryb)
bartry3b = as.numeric(bartry2b[,c(1)])

bartry4 = as.matrix(bartry3)
bartry4help = t(bartry4)
bartry4help = bartry4help[,c(2,3,4,5,6,7,8,9,10,11)]
bartry4 = t(bartry4help)
bartry4 = t(bartry4)
bartry4b = as.matrix(bartry3b)
bartry5 = cbind(bartry4,bartry4b)
bartry6 = t(bartry5)
barplot(t(bartry4), ylim = c(-1,1), main = "ACF", col = "red")
barplot(t(bartry4b), ylim = c(-1,1), main = "PACF", col = "red")
barplot(bartry6, ylim = c(-1,1), main = "ACF (red) and PACF (grey) function", col = c("red", "grey"), beside = TRUE)

foodsel = foodcorp.df[,c(3)]
tsfoodsel = as.ts(foodsel)
foodarima <-arima(tsfoodsel, order = c(0,0,1) )
coeftest(foodarima)
auto.arima(tsfoodsel, test = c("adf"), trace = TRUE)
auto.arima(tsfoodsel, d=0, trace = TRUE)


# Example 3: Autoregressive Moving average processes

plot(foodcorp.df[,c(1)],foodcorp.df[,c(10)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales over time")

adf.test(foodcorp.df[,c(10)])
foodacf = acf(foodcorp.df[,c(10)], lag.max = 10)
foodpacf = pacf(foodcorp.df[,c(10)], lag.max = 10)

bartry = Reduce(rbind,foodacf)
bartry2 = t(bartry)
bartry3 = as.numeric(bartry2[,c(1)])
bartryb = Reduce(rbind,foodpacf)
bartry2b = t(bartryb)
bartry3b = as.numeric(bartry2b[,c(1)])

bartry4 = as.matrix(bartry3)
bartry4help = t(bartry4)
bartry4help = bartry4help[,c(2,3,4,5,6,7,8,9,10,11)]
bartry4 = t(bartry4help)
bartry4 = t(bartry4)
bartry4b = as.matrix(bartry3b)
bartry5 = cbind(bartry4,bartry4b)
bartry6 = t(bartry5)
barplot(t(bartry4), ylim = c(-1,1), main = "ACF", col = "red")
barplot(t(bartry4b), ylim = c(-1,1), main = "PACF", col = "red")
barplot(bartry6, ylim = c(-1,1), main = "ACF (red) and PACF (grey) function", col = c("red", "grey"), beside = TRUE)

foodsel = foodcorp.df[,c(10)]
tsfoodsel = as.ts(foodsel)
foodarima <-arima(tsfoodsel, order = c(1,0,1) )
coeftest(foodarima)
auto.arima(tsfoodsel, test = c("adf"), trace = TRUE)
auto.arima(tsfoodsel, d=0, trace = TRUE)


# Example 4-5-6: Testing for unit roots

plot(foodcorp.df[,c(1)],foodcorp.df[,c(5)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales over time")
library(aTSA)
adf.test(foodcorp.df[,c(5)], nlag = 4, output = TRUE)
pp.test(foodcorp.df[,c(5)], output = TRUE)
kpss.test(foodcorp.df[,c(5)], output = TRUE)

plot(foodcorp.df[,c(1)],foodcorp.df[,c(6)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales over time")
library(aTSA)
adf.test(foodcorp.df[,c(6)], nlag = 4, output = TRUE)
pp.test(foodcorp.df[,c(6)], output = TRUE)
kpss.test(foodcorp.df[,c(6)], output = TRUE)

plot(foodcorp.df[,c(1)],foodcorp.df[,c(11)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales over time")
library(aTSA)
adf.test(foodcorp.df[,c(11)], nlag = 4, output = TRUE)
pp.test(foodcorp.df[,c(11)], output = TRUE)
kpss.test(foodcorp.df[,c(11)], output = TRUE)

plot(foodcorp.df[,c(1)],foodcorp.df[,c(7)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales over time")
library(aTSA)
adf.test(foodcorp.df[,c(7)], nlag = 4, output = TRUE)
pp.test(foodcorp.df[,c(7)], output = TRUE)
kpss.test(foodcorp.df[,c(7)], output = TRUE)




foodcorp.df <-read.csv("~/Teaching/RUG/SLIM/SLIM0506DataSeries.csv")








# SLIM06

# Example: ARIMA

plot(foodcorp.df[,c(1)],foodcorp.df[,c(8)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales over time")

adf.test(foodcorp.df[,c(8)], nlag = 4, output = TRUE)
pp.test(foodcorp.df[,c(8)], output = TRUE)

foodacf = acf(foodcorp.df[,c(8)], lag.max = 10)
foodpacf = pacf(foodcorp.df[,c(8)], lag.max = 10)

bartry = Reduce(rbind,foodacf)
bartry2 = t(bartry)
bartry3 = as.numeric(bartry2[,c(1)])
bartryb = Reduce(rbind,foodpacf)
bartry2b = t(bartryb)
bartry3b = as.numeric(bartry2b[,c(1)])

bartry4 = as.matrix(bartry3)
bartry4help = t(bartry4)
bartry4help = bartry4help[,c(2,3,4,5,6,7,8,9,10,11)]
bartry4 = t(bartry4help)
bartry4 = t(bartry4)
bartry4b = as.matrix(bartry3b)
bartry5 = cbind(bartry4,bartry4b)
bartry6 = t(bartry5)
barplot(t(bartry4), ylim = c(-1,1), main = "ACF", col = "red")
barplot(t(bartry4b), ylim = c(-1,1), main = "PACF", col = "red")
barplot(bartry6, ylim = c(-1,1), main = "ACF (red) and PACF (grey) function", col = c("red", "grey"), beside = TRUE)

foodsel = foodcorp.df[,c(8)]
fooddif = diff(foodsel)

adf.test(fooddif, nlag = 4, output = TRUE)
pp.test(fooddif, output = TRUE)

foodacf = acf(fooddif, lag.max = 10)
foodpacf = pacf(fooddif, lag.max = 10)

bartry = Reduce(rbind,foodacf)
bartry2 = t(bartry)
bartry3 = as.numeric(bartry2[,c(1)])
bartryb = Reduce(rbind,foodpacf)
bartry2b = t(bartryb)
bartry3b = as.numeric(bartry2b[,c(1)])

bartry4 = as.matrix(bartry3)
bartry4help = t(bartry4)
bartry4help = bartry4help[,c(2,3,4,5,6,7,8,9,10,11)]
bartry4 = t(bartry4help)
bartry4 = t(bartry4)
bartry4b = as.matrix(bartry3b)
bartry5 = cbind(bartry4,bartry4b)
bartry6 = t(bartry5)
barplot(t(bartry4), ylim = c(-1,1), main = "ACF", col = "red")
barplot(t(bartry4b), ylim = c(-1,1), main = "PACF", col = "red")
barplot(bartry6, ylim = c(-1,1), main = "ACF (red) and PACF (grey) function", col = c("red", "grey"), beside = TRUE)

foodsel = foodcorp.df[,c(8)]
tsfoodsel = as.ts(foodsel)
foodarima <-arima(tsfoodsel, order = c(1,1,1) ) #ARIMA function does not allow for a drift to be included when integrated
coeftest(foodarima)
auto.arima(tsfoodsel, test = c("adf"), trace = TRUE)
auto.arima(tsfoodsel, trace = TRUE)



# Example: Intervention analysis
plot(foodcorp.df[,c(1)],foodcorp.df[,c(9)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales over time", ylim = c(0,100))


# Part before the interventions
foodsel = foodcorp.df[,c(9)]
foodsel2 = foodsel[c(1:144)]

foodacf = acf(foodsel2, lag.max = 10)
foodpacf = pacf(foodsel2, lag.max = 10)

bartry = Reduce(rbind,foodacf)
bartry2 = t(bartry)
bartry3 = as.numeric(bartry2[,c(1)])
bartryb = Reduce(rbind,foodpacf)
bartry2b = t(bartryb)
bartry3b = as.numeric(bartry2b[,c(1)])

bartry4 = as.matrix(bartry3)
bartry4help = t(bartry4)
bartry4help = bartry4help[,c(2,3,4,5,6,7,8,9,10,11)]
bartry4 = t(bartry4help)
bartry4 = t(bartry4)
bartry4b = as.matrix(bartry3b)
bartry5 = cbind(bartry4,bartry4b)
bartry6 = t(bartry5)
barplot(t(bartry4), ylim = c(-1,1), main = "ACF", col = "red")
barplot(t(bartry4b), ylim = c(-1,1), main = "PACF", col = "red")
barplot(bartry6, ylim = c(-1,1), main = "ACF (red) and PACF (grey) function", col = c("red", "grey"), beside = TRUE)

adf.test(foodsel2)
pp.test(foodsel2)

tssales = as.ts(foodsel2)
tsfoodsel3 = ts.intersect(tssales,tssales1=lag(tssales,-1))

m1 <-lm(tssales~tssales1, data=tsfoodsel3, na.action = NULL)
summary(m1)

foodarima <-arima(tssales,order = c(1,0,0) )
coeftest(foodarima)
auto.arima(tssales, test = c("adf"), trace = TRUE)
auto.arima(tssales, d=0, trace = TRUE)


# Part after the intervention
tsfoodsel2 = as.ts(foodcorp.df)
tssales = tsfoodsel2[,c(9)]
tsintervent1 = tsfoodsel2[,c(13)]
tsintervent2 = tsfoodsel2[,c(14)]
tsfoodsel4 = ts.intersect(tssales, tssales1=lag(tssales,-1), tsintervent1, tsintervent2)

m2 <-lm(tssales~tssales1 + tsintervent1 + tsintervent2, data=tsfoodsel4, na.action = NULL)
summary(m2)

tsintervent = tsfoodsel2[,c(13,14)]
auto.arima(tssales, d=0, trace = TRUE, xreg = tsintervent)
auto.arima(tssales, test = c("adf"), trace = TRUE, xreg = tsintervent)

foodarima <-arima(tssales, order = c(1,0,0), xreg = tsintervent)
coeftest(foodarima)



