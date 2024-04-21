#Loading the necessary packages
library(quantmod)
library(tseries)
library(ggplot2)
library(forecast)
library(rmgarch)
library(anytime)
library(readxl)
### 1.1 BETA estimation using CAPM Model ###
#Monthly data for ^NSE and ROUTE
NSE_m<-getSymbols.yahoo("^NSEI", from="2020-11-02", to="2023-10-26",verbose=F, auto.assign=F, periodicity="monthly")
head(NSE_m,5)
route_m<-getSymbols.yahoo("ROUTE.NS", from="2020-11-02", to="2023-10-26", verbose=F, auto.assign=F, periodicity = "monthly")
head(route_m,5)

#T-Bills data
T_Bills_m<-read_excel("T-Bills_2023.xlsx",sheet="M")
T_Bills_m<-as.data.frame(T_Bills_m)
head(T_Bills_m)
T_Bills_m$Month<-anytime(T_Bills_m$Month)
rownames(T_Bills_m)<-as.Date(T_Bills_m[,1])
T_Bills_m_xts<-xts(T_Bills_m[,-1],order.by=T_Bills_m[,1])
head(T_Bills_m_xts,5)

#Estimating BETA using CAPM model

#Data frame of closing prices
closing_m<-cbind(NSE_m$NSEI.Close,route_m$ROUTE.NS.Close)
head(closing_m)

#Calculating the returns

plot(closing_m$NSEI.Close,mar=c(6,4.1,4.1,2.1))
plot(closing_m$ROUTE.NS.Close,mar=c(6,4.1,4.1,2.1))

returns_m<-as.xts(tail(data.frame(closing_m),-1)/head(data.frame(closing_m),-1) - 1)
head(returns_m)

m_ret_route<-returns_m$ROUTE.NS.Close
m_ret_route<-na.omit(m_ret_route)
plot(m_ret_route)

#mean and variance
#mean(m_ret_route)
#var(m_ret_route)

m_ret_nifty<-returns_m$NSEI.Close
m_ret_nifty<-na.omit(m_ret_nifty)
plot(m_ret_nifty)

#Measuring excess returns

exnifty_m<-returns_m$NSEI.Close-T_Bills_m_xts
exnifty_m
exroute_m<-returns_m$ROUTE.NS.Close-T_Bills_m_xts









###1.2 ARIMA Modelling ###

#Getting the data
route_ar_m<-getSymbols.yahoo("ROUTE.NS",from="2020-11-02", to="2023-10-26",verbose=F, auto.assign=F, periodicity = "monthly")

#Calculating the returns
returns_route_ar_m<-as.xts(tail(data.frame(route_ar_m$ROUTE.NS.Close),-1)/head(data.frame(route_ar_m$ROUTE.NS.Close),-1) - 1, frequency=12)
colnames(returns_route_ar_m)<-"Returns_M"
head(returns_route_ar_m,5)

plot(route_ar_m$ROUTE.NS.Close)
plot(returns_route_ar_m$Returns_M)

#Identifying the model

adf.test(returns_route_ar_m,alternative=c("stationary"))
#returns are not stationary
diff_returns_route_ar_m<-diff(returns_route_ar_m,lag=1)
plot(diff_returns_route_ar_m$Returns_M)
diff_returns_route_ar_m<-na.omit(diff_returns_route_ar_m)
adf.test(diff_returns_route_ar_m,alternative=c("stationary"))
#p-value <0.05 series is stationary

#ACF and PACF plots for getting AR and MA orders
plot(acf(diff_returns_route_ar_m$Returns_M,lag.max = 10))
plot(pacf(diff_returns_route_ar_m$Returns_M,lag.max=10))

arima_final_m<-arima(diff_returns_route_ar_m$Returns_M, order=c(1,1,1))
arima_final_m

#Predicting using this model
predicted_m<-predict(arima_final_m,n.ahead=10)
predicted_m
#Diagnosis of the model
tsdiag(arima_final_m)


###1.3 GARCH and EGARCH models
route_ge_m<-getSymbols("ROUTE.NS",from="2020-11-02",to="2023-10-26")
r_route_ge_m<-monthlyReturn(ROUTE.NS)
head(r_route_ge_m,5)

#Implementing univariate GARCH
ug_spec_m=ugarchspec()
ug_spec_m

#Implementing EGARCH
eg_spec_m=ugarchspec(variance.model=list(model="eGARCH"))
eg_spec_m

#Estimating the models
ugfit_m=ugarchfit(spec=ug_spec_m, data=r_route_ge_m)
ugfit_w#lower aic value models are better
#Forecasting
ugforecast_m=ugarchforecast(ugfit_m,n.ahead = 10)
ugforecast_m
