#Package installation
install.packages("quantmod")
install.packages("tseries")
install.packages("ggplot2")
install.packages("forecast")
install.packages("rmgarch")
install.packages("anytime")
install.packages("readxl")

library(quantmod)
library(tseries)
library(ggplot2)
library(forecast)
library(rmgarch)
library(anytime)
library(readxl)
### 1.1 BETA estimation using CAPM Model ###
#Daily data for ^NSEI and ROUTE.NS#
NSE_d<-getSymbols.yahoo("^NSEI",from="2020-11-02", to="2023-10-26",verbose=F, auto.assign=F, periodicity = "daily")
NSE_d<-na.omit(NSE_d)
route_d<-getSymbols.yahoo("ROUTE.NS",from="2020-11-02", to="2023-10-26",verbose=F, auto.assign=F, periodicity="daily")
head(NSE_d,5)
tail(NSE_d,5)
#T-Bills Data
T_Bills_d<- read_excel("C:\\Users\\joshi\\Desktop\\Courses\\Finance 4-1\\FRAM\\Assignment\\T-Bills_2023.xlsx",sheet="D")
T_Bills_d<-as.data.frame(T_Bills_d)
head(T_Bills_d)
T_Bills_d$Date<-anytime(T_Bills_d$Date)
rownames(T_Bills_d)<-as.Date(T_Bills_d[,1])
T_Bills_d_xts<-xts(T_Bills_d[,-1],order.by=T_Bills_d[,1])
head(T_Bills_d_xts,5)
##Estimating Beta using CAPM model

#Dataframe of closing prices
closing_d<-cbind(NSE_d$NSEI.Close,route_d$ROUTE.NS.Close)
head(closing_d,5)
#Calculating the returns
plot(closing_d$NSEI.Close,mar=c(6,4.1,4.1,2.1))
plot(closing_d$ROUTE.NS.Close,mar=c(6,4.1,4.1,2.1))

returns_d<- as.xts(tail(data.frame(closing_d),-1)/head(data.frame(closing_d),-1) - 1)
head(returns_d,5)
tail(returns_d,5)
d_ret_route<-returns_d$ROUTE.NS.Close
d_ret_route<-na.omit(d_ret_route)
plot(d_ret_route,mar=c(6,4.1,4.1,2.1))

#Uncomment to run
#mean(d_ret_route)
#var(d_ret_route)
d_ret_nifty<-returns_d$NSEI.Close
d_ret_nifty<-na.omit(d_ret_nifty)
plot(d_ret_nifty,mar=c(6,4.1,4.1,2.1))

#Calculating excess returns
exnifty_d<-returns_d$NSEI.Close-T_Bills_d_xts
head(exnifty_d)     

exroute_d<-returns_d$ROUTE.NS.Close-T_Bills_d_xts
head(exroute_d,5)

#Running the regression model
regression_d<-lm(exroute_d~exnifty_d)
#slope parameter is the beta in CAPM model
summary(regression_d)


###1.2 ARIMA Modelling ###

#Getting the data
route_ar_d<- getSymbols.yahoo("ROUTE.NS", from="2020-11-02", to="2023-10-26",verbose=F, auto.assign=F, periodicity = "daily")
head(route_ar_d,5)
#Calculating the returns
returns_route_ar_d<-as.xts(tail(data.frame(route_ar_d$ROUTE.NS.Close),-1)/head(data.frame(route_ar_d$ROUTE.NS.Close),-1) - 1, frequency=365)
colnames(returns_route_ar_d)<-"Returns"
head(returns_route_ar_d)

plot(route_ar_d$ROUTE.NS.Close) # can be seen that the data is not stationary
plot(returns_route_ar_d$Returns)

#Step-1 Identifying the model
adf.test(returns_route_ar_d$Returns, alternative=c("stationary"))
#p-value=0.01 which is less than 0.05- returns are stationary

#ACF and PACF plots for getting order of AR and MA terms
plot(acf(returns_route_ar_d$Returns,lag.max=10))#for AR
plot(pacf(returns_route_ar_d$Returns,lag.max=10))#for MA

arima_final_d<-arima(returns_route_ar_d$Returns, order=c(4,0,3)) #trying for different arima orders and choosing for one with the lowest aic value
arima_final_d

#Predicting using this model
predicted <- predict(arima_final_d,n.ahead = 10)
predicted
#Diagnosis of the model
tsdiag(arima_final_d)



### GARCH and EGARCH models
return_route_ge_d<-getSymbols("ROUTE.NS", from="2020-11-02", to="2023-10-26")#gives symbol stored as xts object
head(ROUTE.NS)
#Getting returns
r_route_ge_d<-dailyReturn(ROUTE.NS)
head(r_route_ge_d)

#Implementing univariate GARCH
ug_spec_d=ugarchspec()
ug_spec_d
#Implementing EGARCH
eg_spec_d=ugarchspec(variance.model=list(model="eGARCH"))
eg_spec_d
#Estimating the models
ugfit_d=ugarchfit(spec=ug_spec_d,data=r_route_ge_d)
ugfit_d #lower aic value models are better

#Forecasting
ugforecast_d=ugarchforecast(ugfit_d,n.ahead=10)
ugforecast_d
