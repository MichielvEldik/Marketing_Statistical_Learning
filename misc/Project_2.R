# libraries
library(ggplot2)
library(xts)
library(reshape2)
library(FitAR)
library(forecast)
library(tseries)
library(gplots)
library(aTSA)
library(astsa)
library(lmtest)
library(aTSA)
library(vars)
library(gplots)
library(RColorBrewer)
library(grDevices)

# Create backup of data and more convenient name
library(readr)
SLIMBeerData <- read.csv("SLIMBeerData.csv")
View(SLIMBeerData)
Backup_Beer_Data <- SLIMBeerData
Beer_Data <- SLIMBeerData


# Graphs
first_year <- data.frame(Beer_Data[1:53,]$B1_Sal, Beer_Data[1:53,]$Week)
second_year <- data.frame(Beer_Data[54:106,]$B1_Sal, Beer_Data[54:106,]$Week)
third_year <- data.frame(Beer_Data[107:159,]$B1_Sal, Beer_Data[107:159,]$Week)
bfirst_year <- data.frame(Beer_Data[1:53,]$B3_Sal, Beer_Data[1:53,]$Week)
bsecond_year <- data.frame(Beer_Data[54:106,]$B3_Sal, Beer_Data[54:106,]$Week)
bthird_year <- data.frame(Beer_Data[107:159,]$B3_Sal, Beer_Data[107:159,]$Week)
B3_sal_df <- cbind(bfirst_year, bsecond_year, bthird_year)
B1_sal_df <-cbind(first_year, second_year, third_year)

ggplot(data=B1_sal_df, aes(x=Beer_Data.1.53....Week)) +
  geom_line(aes(y=B1_sal_df$Beer_Data.1.53....B1_Sal), color="firebrick") +geom_line(aes(y=B1_sal_df$Beer_Data.54.106....B1_Sal), color="firebrick") +
  geom_line(aes(y=B1_sal_df$Beer_Data.107.159....B1_Sal), color="firebrick") +
  geom_line(aes(y= 1886841), color="black") +
  ggtitle("Brand 1 yearly sales volumes") +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5)) +
  labs(x = "Weeks", y = "Unit Change (EUR)", color = "Legend") +
  scale_color_manual(values = c("firebrick", "black", "blue"))

ggplot(data=B3_sal_df, aes(x=B3_sal_df$Beer_Data.1.53....Week)) +
  geom_line(aes(y=B3_sal_df$Beer_Data.1.53....B3_Sal), color="blue") +
  geom_line(aes(y=B3_sal_df$Beer_Data.54.106....B3_Sal), color="blue") +
  geom_line(aes(y=B3_sal_df$Beer_Data.107.159....B3_Sal), color="blue") +
  geom_line(aes(y= 1273445), color="black") +
  ggtitle("Brand 3 yearly sales volumes") +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5)) +
  labs(x = "Weeks", y = "Unit Change (EUR)", color = "Legend") +
  scale_color_manual(values = c("firebrick", "black", "blue"))

# first look at data
head(SLIMBeerData)


# Question 1
# Question 1.1

# Brand 1 sales plot
plot(Beer_Data$Week, Beer_Data$B1_Sal, type="l", col="red", lwd=5, xlab="weeks",
     ylab="sales", main="Sales Brand 1")

# Brand 3 sales plot
plot(Beer_Data$Week, Beer_Data$B3_Sal, type="l", col="red", lwd=5, xlab="weeks",
     ylab="sales", main="Sales Brand 3")

# Question 1.2
# Brand 1 sales - stationary. Stationary = alternative hypothesis! = low p value
adf.test(Beer_Data$B1_Sal, nlag = 4)
pp.test(Beer_Data$B1_Sal, output = TRUE)
kpss.test(Beer_Data$B1_Sal, output = TRUE)
Beer1ACF = acf(Beer_Data$B1_Sal, lag.max = 10)
Beer1PACF = pacf(Beer_Data$B1_Sal, lag.max = 10)
# if type 1 is significant, basically means nothing going on as the mean would be 0 then
# adf.test shows type 2 is significant, that is mean stationary.
# interpretation: ACF shows slow decay, PACF shows one spike, so probably only an AR process is
# going on (shock comes from the y variable)#Brand 3 sales - stationary
adf.test(Beer_Data$B3_Sal, nlag = 4)
pp.test(Beer_Data$B3_Sal, output = TRUE)
kpss.test(Beer_Data$B3_Sal, output = TRUE)
Beer3ACF = acf(Beer_Data$B3_Sal, lag.max = 10)
Beer3PACF = pacf(Beer_Data$B3_Sal, lag.max = 10)
# again type 2 is significant, means that it is mean stationary
# interpretation: ACF shows slow decay, PACF shows one spike, so probably only an AR process is
# going on (shock comes from the y variable)




# Question 1.3
# What type of ARIMA model best describes the two series?
# ARIMA for brand 1
BeerSel1 = Beer_Data$B1_Sal
tsBeerSel1 = as.ts(BeerSel1)
Beer1arima <- arima(tsBeerSel1, order = c(1, 0, 0))
coeftest(Beer1arima)
#check whether the results of auto.arima show that order (1, 0, 0) is indeed the best.
auto.arima(tsBeerSel1, test = c("adf"), trace = TRUE)
auto.arima(tsBeerSel1, d = 0, trace = TRUE)
#interpretation: value of phi(?) is 0.70, mean is 1886840.80

#ARIMA for brand 3
BeerSel3 = Beer_Data$B3_Sal
tsBeerSel3 = as.ts(BeerSel3)
Beer3arima <- arima(tsBeerSel3, order = c(1, 0, 0))
coeftest(Beer3arima)
#check whether the results of auto.arima show that order (1, 0, 0) is indeed the best. It is not, also
# MA, so (1, 0, 1)
auto.arima(tsBeerSel3, test = c("adf"), trace = TRUE)
auto.arima(tsBeerSel3, d = 0, trace = TRUE)
# interpretation: so we have an AR and an MA process going on, which means that the change of the
# sales depend both on the sales of previous period (AR)
# and on some random shock from the error component (MA)
# it shows a -0.1959 ma1, this means that 20% of the shock carries over, with a negative effect on
# sales. This could be a continuing negative effect,
# but also a compensation effect of e.g. a price promotion in the previous period (something positive)
# we have an MA coef of -0.1959, this means that there was a shock from the random component
# this period which had a certain effect on sales.#this effect is still there next period multiplied by -0.1959. so if shock is +50 this period, then next
# period it is +50 * -0.1959 = -10 effect on sales next period
# and if shock is -50 this period, then next period it is -50 * -0.1959 = +10 effect on sales next period.
# the period after next period, the shock disappeared again as it came from the random component




# Question 2
# Question 2.1
# Brand 1 log sales plot
plot(Beer_Data$Week, Beer_Data$B1_lnSal, type="l", col="red", lwd=5, xlab="weeks"
     , ylab="sales log", main="Sales log Brand 1")
# Brand 1 log advertising plot
plot(Beer_Data$Week, Beer_Data$B1_lnAdv, type="l", col="red", lwd=5, xlab="weeks"
     , ylab="advertising log", main="Advertising log Brand 1")
# Brand 1 log price plot
plot(Beer_Data$Week, Beer_Data$B1_lnPrice, type="l", col="red", lwd=5, xlab="weeks"
     , ylab="price log", main="Price log Brand 1")
# Brand 3 log sales plot
plot(Beer_Data$Week, Beer_Data$B3_lnSal, type="l", col="red", lwd=5, xlab="weeks"
     , ylab="sales log", main="Sales log Brand 3")
# Brand 3 log advertising plot
plot(Beer_Data$Week, Beer_Data$B3_lnAdv, type="l", col="red", lwd=5, xlab="weeks"
     , ylab="advertising log", main="Advertising log Brand 3")
# Brand 3 log price plot
plot(Beer_Data$Week, Beer_Data$B3_lnPrice, type="l", col="red", lwd=5, xlab="weeks"
     , ylab="price log", main="Price log Brand 3")



# Question 2.2

####### Brand 1 Granger causality tests
# H0: no cause

# Advertising is not granger-causing Sales (insignificant!)
grangertest(Beer_Data$B1_lnSal~Beer_Data$B1_lnAdv, order = 13, data = SLIMBeerData)
# Price is granger-causing Sales (insignificant to 5% level but significant to 10% level)
grangertest(Beer_Data$B1_lnSal~Beer_Data$B1_lnPrice, order = 13, data = SLIMBeerData)
# Sales is granger-causing Advertisement (significant p-value = .00185)
grangertest(Beer_Data$B1_lnAdv~Beer_Data$B1_lnSal, order = 13, data = SLIMBeerData)#Price is not granger-causing Advertisement (non-significant p-value!)
grangertest(Beer_Data$B1_lnAdv~Beer_Data$B1_lnPrice, order = 13, data = SLIMBeerData)
# Sales is not granger-causing Price (non-significant p-value!)
grangertest(Beer_Data$B1_lnPrice~Beer_Data$B1_lnSal, order = 13, data = SLIMBeerData)
# Advertising is not granger-causing Price (non-significant p-value!)
grangertest(Beer_Data$B1_lnPrice~Beer_Data$B1_lnAdv, order = 13, data = SLIMBeerData)


#######Brand 3 Granger causality tests
#Sales granger caused by advertising: no
grangertest(Beer_Data$B3_lnSal~Beer_Data$B3_lnAdv, order = 13, data = SLIMBeerData)
#Sales granger caused by price: no
grangertest(Beer_Data$B3_lnSal~Beer_Data$B3_lnPrice, order = 13, data = SLIMBeerData)
#Advertising granger caused by sales: no
grangertest(Beer_Data$B3_lnAdv~Beer_Data$B3_lnSal, order = 13, data = SLIMBeerData)
#Advertising granger caused by price: no
grangertest(Beer_Data$B3_lnAdv~Beer_Data$B3_lnPrice, order = 13, data = SLIMBeerData)
#Price granger caused by sales: no
grangertest(Beer_Data$B3_lnPrice~Beer_Data$B3_lnSal, order = 13, data = SLIMBeerData)
#Price granger caused by advertising: yes 10% level
grangertest(Beer_Data$B3_lnPrice~Beer_Data$B3_lnAdv, order = 13, data = SLIMBeerData)


######causality tests Brand 3 on Brand 1
#Sales Brand 1 granger-caused by Sales Brand 3? 10% level
grangertest(Beer_Data$B1_lnSal~Beer_Data$B3_lnSal, order = 13, data = SLIMBeerData)
#Sales Brand 1 granger-caused by Advertising Brand 3? No
grangertest(Beer_Data$B1_lnSal~Beer_Data$B3_lnAdv, order = 13, data = SLIMBeerData)
#Sales Brand 1 granger-caused by Price Brand 3? No
grangertest(Beer_Data$B1_lnSal~Beer_Data$B3_lnPrice, order = 13, data = SLIMBeerData)
#Advertising Brand 1 granger-caused by Sales Brand 3? Yes 5% level
grangertest(Beer_Data$B1_lnAdv~Beer_Data$B3_lnSal, order = 13, data = SLIMBeerData)
#Advertising Brand 1 granger-caused by Advertising Brand 3? NO
grangertest(Beer_Data$B1_lnAdv~Beer_Data$B3_lnAdv, order = 13, data = SLIMBeerData)
#Advertising Brand 1 granger-caused by Price Brand 3? NO
grangertest(Beer_Data$B1_lnAdv~Beer_Data$B3_lnPrice, order = 13, data = SLIMBeerData)
#Price Brand 1 granger-caused by Sales Brand 3? No
grangertest(Beer_Data$B1_lnPrice~Beer_Data$B3_lnSal, order = 13, data = SLIMBeerData)
#Price Brand 1 granger-caused by Advertising Brand 3? Yes 5%-level
grangertest(Beer_Data$B1_lnPrice~Beer_Data$B3_lnAdv, order = 13, data = SLIMBeerData)
#Price Brand 1 granger-caused by Price Brand 3? No
grangertest(Beer_Data$B1_lnPrice~Beer_Data$B3_lnPrice, order = 13, data = SLIMBeerData)



#######granger-causality tests Brand 1 on Brand 3
#Sales Brand 3 granger-caused by Sales Brand 1? 5% level
grangertest(Beer_Data$B3_lnSal~Beer_Data$B1_lnSal, order = 13, data = SLIMBeerData)
#Sales Brand 3 granger-caused by Advertising Brand 1? No
grangertest(Beer_Data$B3_lnSal~Beer_Data$B1_lnAdv, order = 13, data = SLIMBeerData)
#Sales Brand 3 granger-caused by Price Brand 1? 10%-level
grangertest(Beer_Data$B3_lnSal~Beer_Data$B1_lnPrice, order = 13, data = SLIMBeerData)
#Advertising Brand 3 granger-caused by Sales Brand 1? No
grangertest(Beer_Data$B3_lnAdv~Beer_Data$B1_lnSal, order = 13, data = SLIMBeerData)
#Advertising Brand 3 granger-caused by Advertising Brand 1? No
grangertest(Beer_Data$B3_lnAdv~Beer_Data$B1_lnAdv, order = 13, data = SLIMBeerData)
#Advertising Brand 3 granger-caused by Price Brand 1? No
grangertest(Beer_Data$B3_lnAdv~Beer_Data$B1_lnPrice, order = 13, data = SLIMBeerData)
#Price Brand 3 granger-caused by Sales Brand 1? NO
grangertest(Beer_Data$B3_lnPrice~Beer_Data$B1_lnSal, order = 13, data = SLIMBeerData)
#Price Brand 3 granger-caused by Advertising Brand 1? NO
grangertest(Beer_Data$B3_lnPrice~Beer_Data$B1_lnAdv, order = 13, data = SLIMBeerData)
#Price Brand 3 granger-caused by Price Brand 1? NO
grangertest(Beer_Data$B3_lnPrice~Beer_Data$B1_lnPrice, order = 13, data = SLIMBeerData)




# Stationarity tests for B1: No unit roots
# Sales: No Unit Root -> Stationary
adf.test(Beer_Data$B1_lnSal, nlag = 4, output = TRUE)
pp.test(Beer_Data$B1_lnSal, output = TRUE)
kpss.test(Beer_Data$B1_lnSal, output = TRUE)
# Advertising ADF and PP say stationary, KPSS most likely also stationary
adf.test(Beer_Data$B1_lnAdv, nlag = 4, output = TRUE)
pp.test(Beer_Data$B1_lnAdv, output = TRUE)
kpss.test(Beer_Data$B1_lnAdv, output = TRUE)
# Price All say stationary
adf.test(Beer_Data$B1_lnPrice, nlag = 4, output = TRUE)
pp.test(Beer_Data$B1_lnPrice, output = TRUE)
kpss.test(Beer_Data$B1_lnPrice, output = TRUE)



# Stationarity tests for B3
# Sales: All say stationary
adf.test(Beer_Data$B3_lnSal, nlag = 4, output = TRUE)
pp.test(Beer_Data$B3_lnSal, output = TRUE)
kpss.test(Beer_Data$B3_lnSal, output = TRUE)
# Advertising ADF and PP say stationary, KPSS most likely also stationary
adf.test(Beer_Data$B3_lnAdv, nlag = 4, output = TRUE)
pp.test(Beer_Data$B3_lnAdv, output = TRUE)
kpss.test(Beer_Data$B3_lnAdv, output = TRUE)
# Price two say stationary, kpss says unit root?, 2v1 so stationary
adf.test(Beer_Data$B3_lnPrice, nlag = 4, output = TRUE)
pp.test(Beer_Data$B3_lnPrice, output = TRUE)
kpss.test(Beer_Data$B3_lnPrice, output = TRUE)

# so cointegration is not possible, because there are no unit roots!
# Business as usual scenario!#Determining which variables are endogenous and which are exogenous
# all control variables are exogenous and all other endogenous
# testing for seasonality!!!

# create sine and cosine
sin.weeks <- sin(Beer_Data$Week/52*2*pi)
cos.weeks <- cos(Beer_Data$Week/52*2*pi)
Beer_Data <- cbind (Beer_Data, sin.weeks, cos.weeks)


# Showing the sine and cosine function to control for seasonal effects
plot(Beer_Data[,c(1)],Beer_Data$sin.weeks, type="l", col="red", lwd=5,
     xlab="weeks", ylab="sinus and cosinus",
     main="Sine (grey) and cosine (red) over time")
lines(Beer_Data$cos.weeks, type="l", col="grey", lwd=5)

# Determining lag length of the endogenous variables
# Exogenous
# Seasonality (sine + cosine)
# Distribution B1 and B3
# Feature B1 and B3
# Display B1 and B3
# Feature + display B1 and B3
# Endogenous
# Sales B1 and B3
# Advertising B1 and B3
# Price B1 and B3
# Determining lag length of the endogenous variables
Beer_Data_Endo = Beer_Data[, c('B1_lnSal', 
                               'B3_lnSal', 
                               'B1_lnAdv', 
                               'B3_lnAdv',
                               'B1_lnPrice', 
                               'B3_lnPrice')
                           ]
Beer_Data_Exo = Beer_Data[, c('cos.weeks', 
                              'sin.weeks', 
                              'B1_lnDist', 
                              'B3_lnDist',
                              'B1_lnFeat', 
                              'B3_lnFeat', 
                              'B1_lnDisp', 
                              'B3_lnDisp',
                              'B1_lnFeatDisp', 
                              'B3_lnFeatDisp')
                          ]

# The trend does not have to be added here, as it can be immediately added through type "both"VARselect(Beer_Data_Endo,lag.max = 4, type = "both", exogen = Beer_Data_Exo)
# SC says 1 lag which is the BIC so use 1 lag!!!
# Symmetry or asymmetry? Probably symmetry as asymmetry is too difficult
# (Include the evolving series in first differences)
# Include the stationary series in levels
# All stationary so all in levels
# Estimating the VAR model, and reporting results for the individual 6 equations
Beer_Data_Est <- VAR(Beer_Data_Endo, p=1, type = "both", exogen = Beer_Data_Exo)

# immediate effect is the covariance matrix with its divisions! Not possible to
# say something about the significance!
summary(Beer_Data_Est, "B1_lnSal")
summary(Beer_Data_Est,"B3_lnSal")
summary(Beer_Data_Est,"B1_lnAdv")
summary(Beer_Data_Est,"B3_lnAdv")
summary(Beer_Data_Est,"B1_lnPrice")
summary(Beer_Data_Est,"B3_lnPrice")

# Generating the IRFs!!!
# n.ahead = 13!!!! -> 1 Quarter of the year
# cumulative IRF results is the dynamic effect!
# Generating the IRFs B1
# IRF B1 Sales - No significant results
Beer_IRF1 <- irf(Beer_Data_Est, impulse = NULL, response = "B1_lnSal", n.ahead = 13,
                 ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.90,
                 runs = 500)
plot(Beer_IRF1)
Beer_IRF2 <- irf(Beer_Data_Est, impulse = NULL, response = "B1_lnSal", n.ahead = 13,
                 ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.90,
                 runs = 500)
plot(Beer_IRF2)
#IRF B1 Advertising - A shock in sales of B1 increases advertising (cumulatively) - goes to around 0.45
# after 5 weeks
Beer_IRF3 <- irf(Beer_Data_Est, impulse = NULL, response = "B1_lnAdv", n.ahead = 13,
                 ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.90,runs = 500)
plot(Beer_IRF3)
Beer_IRF4 <- irf(Beer_Data_Est, impulse = NULL, response = "B1_lnAdv", n.ahead = 13,
                 ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.90,
                 runs = 500)
plot(Beer_IRF4)
#IRF B1 Price - No significant results
Beer_IRF5 <- irf(Beer_Data_Est, impulse = NULL, response = "B1_lnPrice", n.ahead = 13,
                 ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.90,
                 runs = 500)
plot(Beer_IRF5)
Beer_IRF6 <- irf(Beer_Data_Est, impulse = NULL, response = "B1_lnPrice", n.ahead = 13,
                 ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.90,
                 runs = 500)
plot(Beer_IRF6)
#Generating IRFs B3 - A shock in sales of B1 increases sales of B3 (goes to around .10), a shock in
# sales of B1 increases sales of B3 (cumulatively) - goes to around 0.17 after 8 weeks
#IRF B3 Sales
Beer_IRF7 <- irf(Beer_Data_Est, impulse = NULL, response = "B3_lnSal", n.ahead = 13,
                 ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.90,
                 runs = 500)
plot(Beer_IRF7)
Beer_IRF8 <- irf(Beer_Data_Est, impulse = NULL, response = "B3_lnSal", n.ahead = 13,
                 ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.90,
                 runs = 500)
plot(Beer_IRF8)
#IRF B3 Advertising - A shock in sales of B3 increases advertising of B3 (to 0.2 (week 1) and 0.4 (week
# 2)), an increase in sales of B3 increases advertising of B3 (cumulatively) - to around 0.7 after 4 weeks
Beer_IRF9 <- irf(Beer_Data_Est, impulse = NULL, response = "B3_lnAdv", n.ahead = 13,
                 ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.90,
                 runs = 500)
plot(Beer_IRF9)
Beer_IRF10 <- irf(Beer_Data_Est, impulse = NULL, response = "B3_lnAdv", n.ahead = 13,ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.90,
                  runs = 500)
plot(Beer_IRF10)
#IRF B3 Price - A shock in sales of B3 decreases price of B3 to -0.0035 (week 1). A shock in sales of
# brand 3 decreases price of brand 3 to around -0.0045 after 8 weeks (cumulatively). A shock in price
# of B1 increases price of B3 to around 0.055 after 12 weeks (cumulatively).
Beer_IRF11 <- irf(Beer_Data_Est, impulse = NULL, response = "B3_lnPrice", n.ahead = 13,
                  ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.90,
                  runs = 500)
plot(Beer_IRF11)
Beer_IRF12 <- irf(Beer_Data_Est, impulse = NULL, response = "B3_lnPrice", n.ahead = 13,
                  ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.90,
                  runs = 500)
plot(Beer_IRF12)
###Generating the FEVDs
#FEVDs with pre-specified causal ordering required
Beer_FEVD1 <-fevd(Beer_Data_Est,n.ahead = 13)
Beer_FEVD1
barbasis1 = Beer_FEVD1[1]
barbasis2 = as.matrix(unlist(barbasis1),ncol =6, byrow = TRUE)
#Only select weeks of intervals of 13
bartry = Reduce(rbind,Beer_FEVD1)
bartry2 = t(bartry)
bartry2 = bartry2[,c(13, 26, 39, 52, 65, 78)]



#Plot graphics
#FEVD plot
barplot(bartry2, col = c("red", "black", "white", "blue", "green", "yellow"), xlab = "FEVD Brand 1 and
3", ylab = "Sales, Advertising, Price", names.arg = c("Sales B1", "Sales B3", "Advertising B1",
                                                      "Advertising B3", "Price B1", "Price B3"))

legend("topright", legend = c("Sales B1", "Sales B3", "Advertising B1", "Advertising B3", "Price B1",
                              "Price B3"), fill = c("red", "black", "white", "blue", "green", "yellow"))
