####

#SLIM0607 Modern Time Series Analysis

####



# Read in the data, and provide overview of the variables
railways.df <-read.csv("SLIM0607Railways.csv")
str(railways.df)


# Overview of the essentials
summary(railways.df)
summary(railways.df[,c(2, 3, 4)])
plot(railways.df[,c(2,3,4)])
plot(railways.df[,c(1)],railways.df[,c(2)], type="l", col="red", lwd=5, xlab="months", ylab="passkm", main="Pass-based travel over time")
plot(railways.df[,c(1)],railways.df[,c(3)], type="l", col="red", lwd=5, xlab="months", ylab="ticketkm", main="Ticket-based travel over time")
plot(railways.df[,c(1)],railways.df[,c(4)], type="l", col="red", lwd=5, xlab="months", ylab="satisfaction", main="Satisfaction over time")
plot(railways.df[,c(1)],railways.df[,c(6)], type="l", col="red", lwd=5, xlab="months", ylab="advertising", main="Advertising over time")
plot(railways.df[,c(1)],railways.df[,c(11)], type="l", col="red", lwd=5, xlab="months", ylab="adstock", main="Adstock over time")
barplot(railways.df[,c(7)], main = "Retailer promotions", col = "red")
barplot(railways.df[,c(8)], main = "Own-channel promotions", col = "red")
barhelp.data = data.matrix(railways.df[,c(7,8)])
barhelp2.data = t(barhelp.data)
barplot(barhelp2.data, main = "Retailer (red) and Own-channel (grey) promotions", col = c("red","grey"), xlab = "months")



# Granger causality tests
library(vars)

install.packages("lmtest")
library(lmtest)
grangertest(LNKMABOFIX2~LNKMLOSFIX2, order = 2, data = railways.df)
grangertest(LNKMABOFIX2~LNKMLOSFIX2, order = 4, data = railways.df)
grangertest(LNKMABOFIX2~LNKMLOSFIX2, order = 6, data = railways.df)
grangertest(LNKMABOFIX2~LNKMLOSFIX2, order = 8, data = railways.df)

grangertest(LNKMABOFIX2~LNAOAVERAGE_2, order = 2, data = railways.df)
grangertest(LNKMABOFIX2~LNAOAVERAGE_2, order = 4, data = railways.df)
grangertest(LNKMABOFIX2~LNAOAVERAGE_2, order = 6, data = railways.df)
grangertest(LNKMABOFIX2~LNAOAVERAGE_2, order = 8, data = railways.df)

grangertest(LNKMLOSFIX2~LNKMABOFIX2, order = 2, data = railways.df)
grangertest(LNKMLOSFIX2~LNKMABOFIX2, order = 4, data = railways.df)
grangertest(LNKMLOSFIX2~LNKMABOFIX2, order = 6, data = railways.df)
grangertest(LNKMLOSFIX2~LNKMABOFIX2, order = 8, data = railways.df)

grangertest(LNKMLOSFIX2~LNAOAVERAGE_2, order = 2, data = railways.df)
grangertest(LNKMLOSFIX2~LNAOAVERAGE_2, order = 4, data = railways.df)
grangertest(LNKMLOSFIX2~LNAOAVERAGE_2, order = 6, data = railways.df)
grangertest(LNKMLOSFIX2~LNAOAVERAGE_2, order = 8, data = railways.df)

grangertest(LNAOAVERAGE_2~LNKMABOFIX2, order = 2, data = railways.df)
grangertest(LNAOAVERAGE_2~LNKMABOFIX2, order = 4, data = railways.df)
grangertest(LNAOAVERAGE_2~LNKMABOFIX2, order = 6, data = railways.df)
grangertest(LNAOAVERAGE_2~LNKMABOFIX2, order = 8, data = railways.df)

grangertest(LNAOAVERAGE_2~LNKMLOSFIX2, order = 2, data = railways.df)
grangertest(LNAOAVERAGE_2~LNKMLOSFIX2, order = 4, data = railways.df)
grangertest(LNAOAVERAGE_2~LNKMLOSFIX2, order = 6, data = railways.df)
grangertest(LNAOAVERAGE_2~LNKMLOSFIX2, order = 8, data = railways.df)


#Testing for unit roots
install.packages("aTSA")
library(aTSA)
adf.test(railways.df[,c(2)], nlag = 2, output = TRUE)
pp.test(railways.df[,c(2)], output = TRUE)
kpss.test(railways.df[,c(2)], output = TRUE)

adf.test(railways.df[,c(3)], nlag = 2, output = TRUE)
pp.test(railways.df[,c(3)], output = TRUE)
kpss.test(railways.df[,c(3)], output = TRUE)

adf.test(railways.df[,c(4)], nlag = 2, output = TRUE)
pp.test(railways.df[,c(4)], output = TRUE)
kpss.test(railways.df[,c(4)], output = TRUE)



#Determining lag length of the endogenous variables
railendo = railways.df[,c(3,2,5)]
railexo = railways.df[,c(11,7,8,13,9,10)]    #The trend does not have to be added here, as it can be immediately added through type "both"
VARselect(railendo,lag.max = 4, type = "both", exogen = railexo)



#Showing the sine and cosine function to control for seasonal effects
plot(railways.df[,c(1)],railways.df[,c(9)], type="l", col="red", lwd=5, xlab="months", ylab="sinus and cosinus", main="Sine (grey) and cosine (red) over time")
lines(railways.df[,c(10)], type="l", col="grey", lwd=5)



#Estimating the VAR model, and reporting results for the individual equations
railest <- VAR(railendo, p=1, type = "both", exogen = railexo)
summary(railest,"LNKMABOFIX2")
summary(railest,"LNKMLOSFIX2")
summary(railest,"DLNAOAVERAGE_2L")



#Generating the IRFs
railirf1 <- irf(railest, impulse = NULL, response = "LNKMABOFIX2", n.ahead = 12,
               ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.95,
               runs = 500)  
plot(railirf1)

railirf2 <- irf(railest, impulse = NULL, response = "LNKMABOFIX2", n.ahead = 12,
               ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.95,
               runs = 500)  
plot(railirf2)

railirf3 <- irf(railest, impulse = NULL, response = "LNKMLOSFIX2", n.ahead = 12,
               ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.95,
               runs = 500)  
plot(railirf3)

railirf4 <- irf(railest, impulse = NULL, response = "LNKMLOSFIX2", n.ahead = 12,
               ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.95,
               runs = 500)  
plot(railirf4)

railirf5 <- irf(railest, impulse = NULL, response = "DLNAOAVERAGE_2L", n.ahead = 12,
                ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.95,
                runs = 500)  
plot(railirf5)


railirf6 <- irf(railest, impulse = NULL, response = "DLNAOAVERAGE_2L", n.ahead = 12,
                ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.90,
                runs = 500) 
plot(railirf6)



#Generating the FEVDs
railfevd1 <-fevd(railest,n.ahead = 12)
railfevd1
barbasis1 = railfevd1[1]
barbasis2 = as.matrix(unlist(barbasis1),ncol =3, byrow = TRUE)

bartry = Reduce(rbind,railfevd1)
bartry2 = t(bartry)
bartry2 = bartry2[,c(12,24,36)]
library(gplots)
library(RColorBrewer)

barplot(bartry2, col = c("red", "grey", "white"), xlab = "TBT - PBT - CSAT") 
barplot(bartry2, names.arg = c("TBT","PBT","CSAT"), xlab = "TBT - PBT - CSAT",  ylab = "TBT - PBT - CSAT" ) 
barplot(bartry2,  col = brewer.pal(3, "YlOrRd"), names.arg = c("TBT","PBT","CSAT"), xlab = "TBT - PBT - CSAT",  ylab = "TBT - PBT - CSAT" ) 







# Read in the data, and provide overview of the variables
flsc.df <-read.csv("SLIM0607FLSC.csv")
str(flsc.df)


# Overview of the essentials
summary(flsc.df)
summary(flsc.df[,c(2, 5, 8, 11, 14, 17)])
plot(flsc.df[,c(1)],flsc.df[,c(2)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales FinL?sk over time")
plot(flsc.df[,c(1)],flsc.df[,c(5)], type="l", col="red", lwd=5, xlab="weeks", ylab="advertising", main="Advertising FinL?sk over time")
plot(flsc.df[,c(1)],flsc.df[,c(8)], type="l", col="red", lwd=5, xlab="weeks", ylab="unit price", main="Unit price FinL?sk over time")
plot(flsc.df[,c(1)],flsc.df[,c(11)], type="l", col="red", lwd=5, xlab="weeks", ylab="sales", main="Volume sales SodaClub over time")
plot(flsc.df[,c(1)],flsc.df[,c(14)], type="l", col="red", lwd=5, xlab="weeks", ylab="advertising", main="Advertising SodaClub over time")
plot(flsc.df[,c(1)],flsc.df[,c(17)], type="l", col="red", lwd=5, xlab="weeks", ylab="unit price", main="Unit price SodaClub over time")



# Granger causality tests
library(vars)

install.packages("lmtest")
library(lmtest)
grangertest(FLlnvsal~FLlnadv, order =12, data = flsc.df)
grangertest(FLlnvsal~FLlnprice, order =12, data = flsc.df)
grangertest(FLlnvsal~SClnvsal, order =12, data = flsc.df)
grangertest(FLlnvsal~SClnadv, order =12, data = flsc.df)
grangertest(FLlnvsal~SClnprice, order =12, data = flsc.df)

grangertest(FLlnadv~FLlnvsal, order =12, data = flsc.df)
grangertest(FLlnadv~FLlnprice, order =12, data = flsc.df)
grangertest(FLlnadv~SClnvsal, order =12, data = flsc.df)
grangertest(FLlnadv~SClnadv, order =12, data = flsc.df)
grangertest(FLlnadv~SClnprice, order =12, data = flsc.df)

grangertest(FLlnprice~FLlnvsal, order =12, data = flsc.df)
grangertest(FLlnprice~FLlnadv, order =12, data = flsc.df)
grangertest(FLlnprice~SClnvsal, order =12, data = flsc.df)
grangertest(FLlnprice~SClnadv, order =12, data = flsc.df)
grangertest(FLlnprice~SClnprice, order =12, data = flsc.df)

grangertest(SClnvsal~SClnadv, order =12, data = flsc.df)
grangertest(SClnvsal~SClnprice, order =12, data = flsc.df)
grangertest(SClnvsal~FLlnvsal, order =12, data = flsc.df)
grangertest(SClnvsal~FLlnadv, order =12, data = flsc.df)
grangertest(SClnvsal~FLlnprice, order =12, data = flsc.df)

grangertest(SClnadv~SClnvsal, order =12, data = flsc.df)
grangertest(SClnadv~SClnprice, order =12, data = flsc.df)
grangertest(SClnadv~FLlnvsal, order =12, data = flsc.df)
grangertest(SClnadv~FLlnadv, order =12, data = flsc.df)
grangertest(SClnadv~FLlnprice, order =12, data = flsc.df)

grangertest(SClnprice~SClnvsal, order =12, data = flsc.df)
grangertest(SClnprice~SClnadv, order =12, data = flsc.df)
grangertest(SClnprice~FLlnvsal, order =12, data = flsc.df)
grangertest(SClnprice~FLlnadv, order =12, data = flsc.df)
grangertest(SClnprice~FLlnprice, order =12, data = flsc.df)



#Testing for unit roots
install.packages("aTSA")
library(aTSA)
adf.test(flsc.df[,c(3)], nlag = 2, output = TRUE)
pp.test(flsc.df[,c(3)], output = TRUE)
kpss.test(flsc.df[,c(3)], output = TRUE)

adf.test(flsc.df[,c(6)], nlag = 2, output = TRUE)
pp.test(flsc.df[,c(6)], output = TRUE)
kpss.test(flsc.df[,c(6)], output = TRUE)

adf.test(flsc.df[,c(9)], nlag = 2, output = TRUE)
pp.test(flsc.df[,c(9)], output = TRUE)
kpss.test(flsc.df[,c(9)], output = TRUE)

adf.test(flsc.df[,c(12)], nlag = 2, output = TRUE)
pp.test(flsc.df[,c(12)], output = TRUE)
kpss.test(flsc.df[,c(12)], output = TRUE)

adf.test(flsc.df[,c(15)], nlag = 2, output = TRUE)
pp.test(flsc.df[,c(15)], output = TRUE)
kpss.test(flsc.df[,c(15)], output = TRUE)

adf.test(flsc.df[,c(18)], nlag = 2, output = TRUE)
pp.test(flsc.df[,c(18)], output = TRUE)
kpss.test(flsc.df[,c(18)], output = TRUE)



#Determining lag length of the endogenous variables
flscendo = flsc.df[,c(3,6,9,12,15,18)]
flscexo = flsc.df[,c(22,23,24)]    #The trend does not have to be added here, as it can be immediately added through type "both"
VARselect(flscendo,lag.max = 12, type = "both", exogen = flscexo)



#Showing the sine and cosine function to control for seasonal effects
plot(railways.df[,c(1)],railways.df[,c(9)], type="l", col="red", lwd=5, xlab="months", ylab="sinus and cosinus", main="Sine (grey) and cosine (red) over time")
lines(railways.df[,c(10)], type="l", col="grey", lwd=5)



#Estimating the VAR model, and reporting results for the individual equations
flscest <- VAR(flscendo, p=1, type = "both", exogen = flscexo)
summary(flscest,"FLlnvsal")
summary(flscest,"FLlnadv")
summary(flscest,"FLlnprice")
summary(flscest,"SClnvsal")
summary(flscest,"SClnadv")
summary(flscest,"SClnprice")



#Generating the IRFs
flscirf1 <- irf(flscest, impulse = NULL, response = "FLlnvsal", n.ahead = 13,
                ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.95,
                runs = 100)  
plot(flscirf1)

flscirf2 <- irf(flscest, impulse = NULL, response = "FLlnvsal", n.ahead = 13,
                ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.95,
                runs = 100)  
plot(flscirf2)

railirf3 <- irf(railest, impulse = NULL, response = "LNKMLOSFIX2", n.ahead = 12,
                ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.95,
                runs = 100)  
plot(railirf3)

railirf4 <- irf(railest, impulse = NULL, response = "LNKMLOSFIX2", n.ahead = 12,
                ortho = FALSE, cumulative = TRUE, boot = TRUE, ci = 0.95,
                runs = 100)  
plot(railirf4)

railirf5 <- irf(railest, impulse = NULL, response = "DLNAOAVERAGE_2L", n.ahead = 12,
                ortho = FALSE, cumulative = TRUE, boot = TRUE, ci = 0.95,
                runs = 100)  
plot(railirf5)

railirf6 <- irf(railest, impulse = NULL, response = "DLNAOAVERAGE_2L", n.ahead = 12,
                ortho = FALSE, cumulative = TRUE, boot = TRUE, ci = 0.95,
                runs = 100) 
plot(railirf6)



#Generating the FEVDs
railfevd1 <-fevd(railest,n.ahead = 12)
railfevd1
barbasis1 = railfevd1[1]
barbasis2 = as.matrix(unlist(barbasis1),ncol =3, byrow = TRUE)

bartry = Reduce(rbind,railfevd1)
bartry2 = t(bartry)
bartry2 = bartry2[,c(12,24,36)]
library(gplots)
library(RColorBrewer)

barplot(bartry2, col = c("red", "grey", "white"), xlab = "TBT - PBT - CSAT") 
barplot(bartry2,  col = brewer.pal(3, "YlOrRd"), names.arg = c("TBT","PBT","CSAT"), xlab = "TBT - PBT - CSAT",  ylab = "TBT - PBT - CSAT" ) 
barplot(bartry2, names.arg = c("TBT","PBT","CSAT"), xlab = "TBT - PBT - CSAT",  ylab = "TBT - PBT - CSAT" ) 



