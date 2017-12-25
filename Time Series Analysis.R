
############################ Giant Sales Forecasting - TS ###########################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building -a) Exponential Smoothing b) Classical Decomposition c) Auto Arima for each of the following
#  4.1 Europe Consumer Sales (Row No. 147 to 308)
#  4.2 Europe Consumer Quantity (Row No. 310 to 472)
#  4.3 APAC Consumer Sales (Row No. 474 to 636)
#  4.4 APAC Consumer Quantity (Row No. 638 to 798)
# 5 Model Evaluation (MAPE) for the last 6 months prediction Vs Actual

#####################################################################################

# 1. Business Understanding: 

# Global Mart - Giant Online Store having world wide operations. 
# it takes orders and delivers across the globe. 
# The objective is to understand & model the plan for next 6 months by 
# Forecasting sales & demand for next 6 months To help manage revenue & inventory

#####################################################################################

# 2. Data Understanding: 
# segment the data into the 21 subsets on market and customer type
# "Market" has 7 geographic levels and"Segment" has 3 levels that the customer belongs to
# These aggregations need to be done on "Order Date"

#3. Data Preparation: 

#Loading the required libraries

library(graphics)
library(forecast)
library(stringr)
library(ggplot2)
library(dplyr)
library(tseries)

#loading the file into R

store <- read.csv("Global Superstore.csv", header = T)

#Missing value treatment
sum(is.na(store)) # has 41296 na values
sum(is.na(store$Postal.Code)) # all NA values in Postal Code
store <- store [,-12] # removing Postal Code as more than 90% of its values are NA's
sum(is.na(store)) # has Zero na values now

#Check for duplicate values
sum(duplicated(store$Row.ID)) # 0 duplicates

#Make the time separator consistent
store$Order.Date <- str_replace_all(store$Order.Date, "[/]",  "-")
store$Ship.Date <- str_replace_all(store$Ship.Date, "[/]",  "-")

# convert time columns to datetime object
store$Order.Date <- as.Date(store$Order.Date, format = "%d-%m-%Y")
store$Ship.Date <- as.Date(store$Ship.Date, format = "%d-%m-%Y")

# convert time columns to YYYY-MM format
store$OrderDt.YYMMM <- format(store$Order.Date, "%Y-%m")

#Putting together the Market and Segment so that we can use for aggregation
store$MrktSeg <- paste(store$Market,store$Segment, sep = "-")

#aggregating **month wise** profit, sales & quantity for the 21 Market Segments
rollup_profit <- aggregate(store$Profit, by=list(store$OrderDt.YYMMM, store$MrktSeg), FUN=sum)
rollup_sales <- aggregate(store$Sales , by=list(store$OrderDt.YYMMM, store$MrktSeg), FUN=sum)
rollup_quantity <- aggregate(store$Quantity , by=list(store$OrderDt.YYMMM, store$MrktSeg), FUN=sum)

#aggregating **Overall** profit, sales & quantity for the 21 Market Segments
rollup_profit_TotalSum <- aggregate(store$Profit, by=list(store$MrktSeg), FUN=sum)
rollup_sales_TotalSum <- aggregate(store$Sales , by=list(store$MrktSeg), FUN=sum)
rollup_quantity_TotalSum <- aggregate(store$Quantity , by=list(store$MrktSeg), FUN=sum)

#plotting the aggregated **Overall** profit, sales & quantity for the 21 Market Segments
ggplot(rollup_profit_TotalSum, aes(x=factor(Group.1),y=x))+ geom_bar(stat = "identity") +coord_flip()
ggplot(rollup_sales_TotalSum, aes(x=factor(Group.1),y=x))+ geom_bar(stat = "identity") +coord_flip()
ggplot(rollup_quantity_TotalSum, aes(x=factor(Group.1),y=x))+ geom_bar(stat = "identity") +coord_flip()

#Calculating "Coefficient of Variation" on profits to arrive at "Consistantly Profitable"
rollup_profit_mean <- aggregate(store$Profit, by=list(store$MrktSeg), FUN=mean)
rollup_profit_staDev <- aggregate(store$Profit, by=list(store$MrktSeg), FUN=sd)
rollup_profit_staDev$CV_abs<- rollup_profit_staDev$x/ (abs(rollup_profit_mean$x))

#plotting "Coefficient of Variation" on profits for the 21 Market Segments
ggplot(rollup_profit_staDev, aes(x=factor(Group.1),y=CV_abs))+ geom_bar(stat = "identity") +coord_flip()

#----*Calculating two "most profitable" + "Consistantly Profitable"*---

#putting together Total Profit & CV and removing the remaining variables from date frame
rollup_profit_staDev <- cbind(rollup_profit_staDev,rollup_profit_TotalSum$x)
rollup_profit_staDev <- rollup_profit_staDev [ ,-c(2)]

#normalising CV_abs & Total Profit
rollup_profit_staDev$CV_abs_Norm <- scale (rollup_profit_staDev$CV_abs)
rollup_profit_staDev$Total_profit_Norm <- scale (rollup_profit_staDev$`rollup_profit_TotalSum$x`)

# calculating wighteg average
rollup_profit_staDev$weighted <- rollup_profit_staDev$CV_abs_Norm*0.5 + rollup_profit_staDev$Total_profit_Norm*0.5

#Note : most profitable is "higher" the better & CV is "lower" the better
#using the logic: CV_abs has to minimused is the same as maximising (1 minus CV_abs)
rollup_profit_staDev$weighted_V1 <- (1-rollup_profit_staDev$CV_abs_Norm)*0.5 + rollup_profit_staDev$Total_profit_Norm*0.5

# The Top two values of market segments are APAC-Consumer & EU-Consumer, as per "weigted V1" method

#Group.1	      CV_abs	    profit_sum	CV_abs_Norm	  profit_Norm	 weighted	  weighted_V1
#APAC-Consumer	4.20670171	222817.5604	-0.589725879	2.427869482	0.919071801	2.00879768
#EU-Consumer	  4.718083724	188687.7075	-0.496955338	1.886065087	0.694554874	1.691510212
#APAC-Corporate	4.231300718	129737.2348	-0.585263338	0.950238458	0.18248756	1.267750898
#EU-Corporate	  4.776482057	123393.9795	-0.486361213	0.849540584	0.181589686	1.167950899
#LATAM-Consumer	5.438845479	120632.932	-0.36620092	  0.805709523	0.219754302	1.085955221

#------------------------------------------------------------

# The Top two values of market segments are APAC-Consumer & EU-Consumer
EU_Con_Sales_Slice <-subset(rollup_sales, Group.2=="EU-Consumer")
EU_Con_Quantity_Slice <- subset(rollup_quantity, Group.2=="EU-Consumer")
APAC_Con_Sales_Slice <-subset(rollup_sales, Group.2=="APAC-Consumer")
APAC_Con_Quantity_Slice <- subset(rollup_quantity, Group.2=="APAC-Consumer")

write.csv(EU_Con_Sales_Slice, "EU_Con_Sales_Slice.csv", row.names = FALSE)
write.csv(EU_Con_Quantity_Slice, "EU_Con_Quantity_Slice.csv", row.names = FALSE)
write.csv(APAC_Con_Sales_Slice, "APAC_Con_Sales_Slice.csv", row.names = FALSE)
write.csv(APAC_Con_Quantity_Slice, "APAC_Con_Quantity_Slice.csv", row.names = FALSE)

EU_Con_Sales_Slice <-read.csv("EU_Con_Sales_Slice.csv", header = T)
EU_Con_Quantity_Slice <-read.csv("EU_Con_Quantity_Slice.csv", header = T)
APAC_Con_Sales_Slice <-read.csv("APAC_Con_Sales_Slice.csv", header = T)
APAC_Con_Quantity_Slice <-read.csv("APAC_Con_Quantity_Slice.csv", header = T)

#Creating time intervals and adding it to the data frame
timevals <- c(1:nrow(EU_Con_Sales_Slice))
EU_Con_Salesdf <- as.data.frame(cbind(timevals, as.vector(EU_Con_Sales_Slice$x )))
EU_Con_Quantitydf <- as.data.frame(cbind(timevals, as.vector(EU_Con_Quantity_Slice$x)))
APAC_Con_Salesdf <- as.data.frame(cbind(timevals, as.vector(APAC_Con_Sales_Slice$x )))
APAC_Con_Quantitydf <- as.data.frame(cbind(timevals, as.vector(APAC_Con_Quantity_Slice$x)))
colnames(EU_Con_Salesdf) <- c('Month', 'Sales')
colnames(EU_Con_Quantitydf) <- c('Month', 'Quantity')
colnames(APAC_Con_Salesdf) <- c('Month', 'Sales')
colnames(APAC_Con_Quantitydf) <- c('Month', 'Quantity')

# 4. Model Building -a) Exponential Smoothing b) Classical Decomposition c) Auto Arima for each of the following

#------  4.1 Modeling code EU Consumer Sales (lines 147 to 308)---------

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

total_timeserEuSa <- ts(EU_Con_Salesdf$Sales)
indataEUSa <- EU_Con_Salesdf[1:42,]
timeserEuSa <- ts(indataEUSa$Sales)

ylab1 <- c("EU_Con_Sales_TS")
xlab1 <- c("Months from Jan 2011")
title1 <- c("EU_Con_Sales: Jan 2011 to Jul 2014")

plot(timeserEuSa, main=title1, xlab = xlab1, ylab = ylab1)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.3, 0.5,0.7)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedEuSa <- HoltWinters(timeserEuSa, alpha=alphas[i],
                              beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedEuSa)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#taking the value of alpha =0.5 as the final one and plotting it
plot(timeserEuSa, main=title1, xlab = xlab1, ylab = ylab1)
smoothedEuSa <- HoltWinters(timeserEuSa, alpha=0.5,
                            beta=FALSE, gamma=FALSE)
lines(fitted(smoothedEuSa)[,1], col="red", lwd=2)

#RHS smothing, adding the 42nd value, trying to add ordelete any element from a TS, gives an error
#converting to Matrix and back to DF to get rid of the TS
smoothedEuSa1 <- as.matrix(smoothedEuSa$fitted[,1])
smoothedEuSa <- as.data.frame(smoothedEuSa1)
smoothedEuSa[42,1] <-smoothedEuSa[41,1]

#------
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- c(1:nrow(smoothedEuSa))
smoothedEuSadf <- as.data.frame(cbind(timevals_in, smoothedEuSa))
colnames(smoothedEuSadf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.6*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smoothedEuSadf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timevals_in, global_pred, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeserEuSa-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit) # incase you get an error figure margins too large
# use these three commands to get the graph graphics.off() par("mar") par(mar=c(1,1,1,1))
armafit # an ARIMA of (0,0,0) tells us that we have been do a good fit

#Series: local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 113237555:  log likelihood=-449.04
#AIC=900.08   AICc=900.18   BIC=901.82

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary") # since p-vaule is less than 0.05:"there is not 
#enough evidence to support Null Hypo, hence we go with Alt Hypo - Series is "Stationary"

#Augmented Dickey-Fuller Test

#data:  resi
#Dickey-Fuller = -4.4433, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(resi) # since pvalue is more than 0.05, hence series is stationary

#KPSS Test for Level Stationarity

#data:  resi
#KPSS Level = 0.032182, Truncation lag parameter = 1, p-value = 0.1

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdataEuSa <- EU_Con_Salesdf[43:48,]
timevals_out <- outdataEuSa$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdataEuSa[,2])[5]
MAPE_class_dec # MAPE of 25.00224 is a very good one

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeserEuSa, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeserEuSa)
autoarima
#Series: timeserEuSa 
#ARIMA(2,1,0) 

#Coefficients:
#  ar1      ar2
#-0.5796  -0.4906
#s.e.   0.1346   0.1310

#sigma^2 estimated as 168564623:  log likelihood=-445.84
#AIC=897.67   AICc=898.32   BIC=902.81
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeserEuSa - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#Augmented Dickey-Fuller Test

#data:  resi_auto_arima
#Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(resi_auto_arima)

#KPSS Test for Level Stationarity

#data:  resi_auto_arima
#KPSS Level = 0.05314, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdataEuSa[,2])[5]
MAPE_auto_arima # 28.9226 (Classical Model gave a better MAPE)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeserEuSa, col = "black")
lines(auto_arima_pred, col = "red")

#------  4.2 Modeling code EU Consumer Quanitity (lines 310 to 472)---------

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

total_timeserEuQa <- ts(EU_Con_Quantitydf$Quantity)
indataEuQa <- EU_Con_Quantitydf[1:42,]
timeserEuQa <- ts(indataEuQa$Quantity)

ylab1 <- c("EU_Con_Quantity_TS")
xlab1 <- c("Months from Jan 2011")
title1 <- c("EU_Con_Quantity: Jan 2011 to Jul 2014")

plot(timeserEuQa, main=title1, xlab = xlab1, ylab = ylab1)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.3, 0.5,0.7)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedEuQa <- HoltWinters(timeserEuQa, alpha=alphas[i],
                              beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedEuQa)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#taking the value of alpha =0.3 as the final one and plotting it
plot(timeserEuQa, main=title1, xlab = xlab1, ylab = ylab1)
smoothedEuQa <- HoltWinters(timeserEuQa, alpha=0.3,
                            beta=FALSE, gamma=FALSE)
lines(fitted(smoothedEuQa)[,1], col="red", lwd=2)

#RHS smothing, adding the 42nd value, trying to add ordelete any element from a TS, gives an error
#converting to Matrix and back to DF to get rid of the TS
smoothedEuQa1 <- as.matrix(smoothedEuQa$fitted[,1])
smoothedEuQa <- as.data.frame(smoothedEuQa1)
smoothedEuQa[42,1] <-smoothedEuQa[41,1]

#------
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- c(1:nrow(smoothedEuQa))
smoothedEuQadf <- as.data.frame(cbind(timevals_in, smoothedEuQa))
colnames(smoothedEuQadf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.6*Month) * poly(Month,3) + cos(0.3*Month) * poly(Month,3)
            + Month, data=smoothedEuQadf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timevals_in, global_pred, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeserEuQa-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit) # incase you get an error figure margins too large
# use these three commands to get the graph graphics.off() par("mar") par(mar=c(1,1,1,1))
armafit # an ARIMA of (0,0,0) tells us that we have been do a good fit

#Series: local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 17194:  log likelihood=-264.39
#AIC=530.79   AICc=530.89   BIC=532.53

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary") # since p-vaule is less than 0.05:"there is not 
#enough evidence to support Null Hypo, hence we go with Alt Hypo - Series is "Stationary"

#Augmented Dickey-Fuller Test

#data:  resi
#Dickey-Fuller = -3.5473, Lag order = 3, p-value = 0.04911
#alternative hypothesis: stationary

kpss.test(resi) # since pvalue is more than 0.05, hence series is stationary

#KPSS Test for Level Stationarity

#data:  resi
#KPSS Level = 0.043307, Truncation lag parameter = 1, p-value = 0.1

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdataEuQa <- EU_Con_Quantitydf[43:48,]
timevals_out <- outdataEuQa$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdataEuQa[,2])[5]
MAPE_class_dec # MAPE of 27.80546 is a very good one

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeserEuQa, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeserEuQa)
autoarima
#Series: timeserEuQa 
#ARIMA(2,1,0) 

#Coefficients:
#  ar1      ar2
#-0.7359  -0.5879
#s.e.   0.1224   0.1185

#sigma^2 estimated as 21185:  log likelihood=-261.9
#AIC=529.8   AICc=530.44   BIC=534.94
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeserEuQa - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")

#Augmented Dickey-Fuller Test

#data:  resi_auto_arima
#Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
#alternative hypothesis: stationary

kpss.test(resi_auto_arima)

#KPSS Test for Level Stationarity

#data:  resi_auto_arima
#KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdataEuQa[,2])[5]
MAPE_auto_arima # 30.13319 (Classical Model gave a better MAPE)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeserEuQa, col = "black")
lines(auto_arima_pred, col = "red")

#----------4.3 Modeling code APAC Consumer Sales (lines 474 to 636)-----------

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

total_timeserApacSa <- ts(APAC_Con_Salesdf$Sales)
indataApacSa <- APAC_Con_Salesdf[1:42,]
timeserApacSa <- ts(indataApacSa$Sales)

ylab1 <- c("APAC_Con_Sales_TS")
xlab1 <- c("Months from Jan 2011")
title1 <- c("APAC_Con_Sales: Jan 2011 to Jul 2014")

plot(timeserApacSa, main=title1, xlab = xlab1, ylab = ylab1)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.3, 0.5,0.7)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedApacSa <- HoltWinters(timeserApacSa, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedApacSa)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#taking the value of alpha =0.5 as the final one and plotting it
plot(timeserApacSa, main=title1, xlab = xlab1, ylab = ylab1)
smoothedApacSa <- HoltWinters(timeserApacSa, alpha=0.5,
                              beta=FALSE, gamma=FALSE)
lines(fitted(smoothedApacSa)[,1], col="red", lwd=2)

#RHS smothing, adding the 42nd value, trying to add ordelete any element from a TS, gives an error
#converting to Matrix and back to DF to get rid of the TS
smoothedApacSa1 <- as.matrix(smoothedApacSa$fitted[,1])
smoothedApacSa <- as.data.frame(smoothedApacSa1)
smoothedApacSa[42,1] <-smoothedApacSa[41,1]

#------
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- c(1:nrow(smoothedApacSa))
smoothedApacSadf <- as.data.frame(cbind(timevals_in, smoothedApacSa))
colnames(smoothedApacSadf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.6*Month) * poly(Month,3)
            + Month, data=smoothedApacSadf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timevals_in, global_pred, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeserApacSa-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit) # incase you get an error figure margins too large
# use these three commands to get the graph graphics.off() par("mar") par(mar=c(1,1,1,1))
armafit # an ARIMA of (0,0,0) tells us that we have been do a good fit

#Series: local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 99032349:  log likelihood=-446.23
#AIC=894.45   AICc=894.55   BIC=896.19

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary") # since p-vaule is less than 0.05:"there is not 
#enough evidence to support Null Hypo, hence we go with Alt Hypo - Series is "Stationary"

#Augmented Dickey-Fuller Test

#data:  resi
#Dickey-Fuller = -5.0768, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(resi) # since pvalue is more than 0.05, hence series is stationary

#KPSS Test for Level Stationarity

#data:  resi
#KPSS Level = 0.026988, Truncation lag parameter = 1, p-value = 0.1

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdataApacSa <- APAC_Con_Salesdf[43:48,]
timevals_out <- outdataApacSa$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdataApacSa[,2])[5]
MAPE_class_dec # MAPE of 24.1905 is a very good one

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeserApacSa, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeserApacSa)
autoarima
#Series: timeserApacSa 
#ARIMA(0,1,1) 

#Coefficients:
#  ma1
#-0.7559
#s.e.   0.1381

#sigma^2 estimated as 174361555:  log likelihood=-447.11
#AIC=898.23   AICc=898.55   BIC=901.66
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeserApacSa - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")

#Augmented Dickey-Fuller Test

#data:  resi_auto_arima
#Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(resi_auto_arima)

#KPSS Test for Level Stationarity

#data:  resi_auto_arima
#KPSS Level = 0.042734, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdataApacSa[,2])[5]
MAPE_auto_arima # 27.68952 (Classical Model gave a better MAPE)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeserApacSa, col = "black")
lines(auto_arima_pred, col = "red")

#----------4.4 Modeling code APAC Consumer Quantity (lines 638 to 798)-----------

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

total_timeserApacQa <- ts(APAC_Con_Quantitydf$Quantity)
indataApacQa <- APAC_Con_Quantitydf[1:42,]
timeserApacQa <- ts(indataApacQa$Quantity)

ylab1 <- c("APAC_Con_Quantity_TS")
xlab1 <- c("Months from Jan 2011")
title1 <- c("APAC_Con_Quantity: Jan 2011 to Jul 2014")

plot(timeserApacQa, main=title1, xlab = xlab1, ylab = ylab1)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.3, 0.5,0.7)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedApacQa <- HoltWinters(timeserApacQa, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedApacQa)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#taking the value of alpha =0.5 as the final one and plotting it
plot(timeserApacQa, main=title1, xlab = xlab1, ylab = ylab1)
smoothedApacQa <- HoltWinters(timeserApacQa, alpha=0.5,
                              beta=FALSE, gamma=FALSE)
lines(fitted(smoothedApacQa)[,1], col="red", lwd=2)

#RHS smothing, adding the 42nd value, trying to add ordelete any element from a TS, gives an error
#converting to Matrix and back to DF to get rid of the TS
smoothedApacQa1 <- as.matrix(smoothedApacQa$fitted[,1])
smoothedApacQa <- as.data.frame(smoothedApacQa1)
smoothedApacQa[42,1] <-smoothedApacQa[41,1]

#------
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- c(1:nrow(smoothedApacQa))
smoothedApacQadf <- as.data.frame(cbind(timevals_in, smoothedApacQa))
colnames(smoothedApacQadf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smoothedApacQadf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timevals_in, global_pred, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeserApacQa-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit) # incase you get an error figure margins too large
# use these three commands to get the graph graphics.off() par("mar") par(mar=c(1,1,1,1))
armafit # an ARIMA of (0,0,0) tells us that we have been do a good fit

#Series: local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 12604:  log likelihood=-257.87
#AIC=517.74   AICc=517.84   BIC=519.48

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary") # since p-vaule is less than 0.05:"there is not 
#enough evidence to support Null Hypo, hence we go with Alt Hypo - Series is "Stationary"

#Augmented Dickey-Fuller Test

#data:  resi
#Dickey-Fuller = -5.6354, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary


kpss.test(resi) # since pvalue is more than 0.05, hence series is stationary

#KPSS Test for Level Stationarity

#data:  resi
#KPSS Level = 0.035126, Truncation lag parameter = 1, p-value = 0.1

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdataApacQa <- APAC_Con_Quantitydf[43:48,]
timevals_out <- outdataApacQa$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdataApacQa[,2])[5]
MAPE_class_dec # MAPE of 17.99585 is a very good one

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeserApacQa, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeserApacQa)
autoarima

#Series: timeserApacQa 
#ARIMA(0,1,0) 

#sigma^2 estimated as 25366:  log likelihood=-266.07
#AIC=534.14   AICc=534.24   BIC=535.85

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeserApacQa - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")

#Augmented Dickey-Fuller Test

#data:  resi_auto_arima
#Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(resi_auto_arima)

#KPSS Test for Level Stationarity

#data:  resi_auto_arima
#KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdataApacQa[,2])[5]
MAPE_auto_arima # 26.24458.13319 (Classical Model gave a better MAPE)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeserApacQa, col = "black")
lines(auto_arima_pred, col = "red")




