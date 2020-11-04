####

#SLIM0203: The General Linear Model

####



# Read in the data, and provide overview of the variables
finlask.df <-read.csv("~/Teaching/RUG/SLIM/SLIM0203FinLask.csv")
str(finlask.df)


# Overview of the essentials
summary(finlask.df)
summary(finlask.df[,c(15, 3, 23)])
plot(finlask.df[,c(15, 3, 23)])
plot(finlask.df[,c(1)],finlask.df[,c(3)], type="l", col="red", lwd=5, xlab="time", ylab="advertising", main="Advertising over time")
plot(finlask.df[,c(1)],finlask.df[,c(15)], type="l", col="red", lwd=5, xlab="time", ylab="volume sales", main="Volume sales over time")
plot(finlask.df[,c(1)],finlask.df[,c(23)], type="l", col="red", lwd=5, xlab="time", ylab="price", main="Price over time")


# check basic data suitability -  not in slides
library(car)
scatterplotMatrix(finlask.df[,c(15, 3, 23, 18)])
scatterplotMatrix(finlask.df[,c(16, 4, 24, 19)])


# Are the focal variables showing some correlations?
library(corrplot)
corrplot.mixed(cor(finlask.df[ , c(15, 3, 23, 18)]), upper="ellipse")
corrplot.mixed(cor(finlask.df[ , c(16, 4, 24, 19)]), upper="ellipse")



# Fitting a model with a single predictor
lm(lnvsal~lnadv, data=finlask.df)

# Try it!: sd(m1$residuals)
m1 <- lm(lnvsal~lnadv, data=finlask.df)
summary(m1)
AIC(m1)
BIC(m1)

plot(lnvsal~lnadv, data=finlask.df,
     xlab="Advertising (log-transformed)", ylab="Volume sales (log-transformed")
abline(m1, col='red')

str(m1)
m1$coefficients
confint(m1)
anova(m1)



# Fitting a model with multiple predictors
m2 <-lm(lnvsal~lnadv + lnprice + laglnvsal, data=finlask.df)
summary(m2)
AIC(m2)
BIC(m2)

m3 <-lm(lnvsal~lnadv + lnprice + laglnvsal + lncompadv + lncomppriceavg, data=finlask.df)
summary(m3)
AIC(m3)
BIC(m3)

#AIC and BIC of models 1, 2, 3
AIC(m1)
BIC(m1)
AIC(m2)
BIC(m2)
AIC(m3)
BIC(m3)



#ANOVA analysis

m4 <-lm(vsal~PreEvent2 + Event + PostEvent2, data=finlask.df)
summary(m4)

m5 <-lm(vsal~1, data=finlask.df)
summary(m5)

m4 <-lm(vsal~PreEvent2 + Event + PostEvent2, data=finlask.df)

m5 <-lm(vsal~1, data=finlask.df)

anova(m5,m4)


#ANCOVA analysis

m6 <-lm(vsal~adv, data=finlask.df)
summary(m6)
m7 <-lm(vsal~adv + PreEvent2 + Event + PostEvent2, data=finlask.df)
summary(m7)


m6 <-lm(vsal~adv, data=finlask.df)
m7 <-lm(vsal~adv + PreEvent2 + Event + PostEvent2, data=finlask.df)

anova(m6,m7)

m7 <-lm(vsal~adv + PreEvent2 + Event + PostEvent2, data=finlask.df)
m8 <-lm(vsal~adv + PreEvent2 + Event + PostEvent2 +adv*PreEvent2 + adv*Event + adv*PostEvent2, data=finlask.df)

anova(m7,m8)





#SLIM04

#Testing for the difference between model coefficients

library(car)
m2 <-lm(lnvsal~lnadv + lnprice + laglnvsal , data=finlask.df) 
summary(m2)
linearHypothesis(m2, "lnadv = lnprice")

m3 <-lm(lnvsal~lnadv + lnprice + laglnvsal + lncompadv + lncomppriceavg, data=finlask.df)
summary(m3)
linearHypothesis(m3, "lncompadv = lncomppriceavg")



#Obtaining standardized beta
m2 <-lm(lnvsal~lnadv + lnprice + laglnvsal , data=finlask.df) 
summary(m2)

#standardize the variables
m2formula <- lnvsal~lnadv + lnprice + laglnvsal
finlaskstd.df <-lapply(finlask.df[,all.vars(m2formula)],scale)
m2b <-lm(lnvsal~lnadv + lnprice + laglnvsal , data=finlaskstd.df) 
summary(m2b)
linearHypothesis(m2b, "lnadv = lnprice")



#Testing for multicollinearity
library(car)
#create vector of VIF values and a vector of tolerance values
vif_values <- vif(m2)
tolerance <- 1/vif_values
vif_values
tolerance

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, xlim = c(0,12), col = "red")
#add vertical line at 4 and 10
abline(v = 4, lwd = 3, lty = 2)
abline(v = 10, lwd = 3, lty = 2)





#Moderation analysis

#Mean-center the focal independent variables
m2helpa = finlask.df[,c(4, 24, 19)]
lnvsal = finlask.df[,c(16)]
center_scale <- function(x){scale(x,scale = FALSE)}
m2helpb <- center_scale(m2helpa)
finlaskmc.df <-data.frame(m2helpb, lnvsal)

#estimate the model with the mean-centered independent variables
m2mc <-lm(lnvsal~lnadv + lnprice + laglnvsal , data=finlaskmc.df) 
summary(m2mc)

#add the event-related dummies: are price discounts more/less effective during national vs international sports events vs normal times?
EventBase <- data.frame(finlaskmc.df,finlask.df[,c(44, 45)])
m2event <-lm(lnvsal~ lnadv + lnprice + laglnvsal + National + International + lnprice*National + lnprice*International, data=EventBase)
summary(m2event)

pricenorm <-summary(m2event)$coefficients[3,1]
pricenatdummy <-summary(m2event)$coefficients[7,1]
pricenorm
pricenatdummy
pricenational <- pricenorm+pricenatdummy
pricevalues <-cbind(pricenorm,pricenational)
barplot(pricevalues, main = "Price Elasticities", ylim = c(-2,0), col = "red")


#can we use advertising to boost the effect of price discounts?
m2synergy <-lm(lnvsal~lnadv + lnprice +laglnvsal + lnprice*lnadv, data=finlaskmc.df)
summary(m2synergy)

