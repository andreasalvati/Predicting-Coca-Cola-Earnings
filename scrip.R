library(fBasics)
library(forecast) 
library(ggplot2)

datos<-read.csv("coca_cola_earnings.csv",
                header=TRUE,sep=";",dec=",")
y<-datos[1:102,2] 

# achieving stationarity and identifying the model
ts.plot(y)  

par(mfrow=c(2,1))
acf(y)  
pacf(y)

s = 4       # seasonal parameter FOR THIS DATA SET
nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences?
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences?

nlags=90 

z<-diff(y)

ts.plot(z)  
par(mfrow=c(2,1))
acf(z,nlags)  
pacf(z,nlags)

ndiffs(z, alpha=0.05, test=c("adf"))

############ AR ############

#As the lags in the PACF display a cutoff value at lag 8, we can assume a AR model
#of order 8 to be appropriate to represent the data.

# estimating the model for the stationary data
fit_AR<-arima(y,order=c(8,1,0), method = "ML")    
fit_AR # we find the information about the estimated parameters

ts.plot(fit_AR$residuals)
par(mfrow=c(2,1))
acf(fit_AR$residuals,100)
pacf(fit_AR$residuals,100)    

Box.test(fit_AR$residuals,lag=15)

# testing for normality 
shapiro.test(fit_AR$residuals)  # 95% confidence intervals are robust for any kind of distribution

y.pred_AR<-predict(fit_AR,n.ahead=5)
y.pred_AR$pred # point predictions
y.pred_AR$se 
ts.plot(y.pred_AR$pred)

############ ARIMA ############

# estimating the model for the stationary data
fit_ARIMA <- arima(y,order=c(3,1,2), method = "ML")    
fit_ARIMA # we find the information about the estimated parameters

ts.plot(fit_ARIMA$residuals)
par(mfrow=c(2,1))
acf(fit_ARIMA$residuals,100)
pacf(fit_ARIMA$residuals,100)    

Box.test(fit_ARIMA$residuals,lag=15)

# testing for normality 
shapiro.test(fit_ARIMA$residuals)  # 95% confidence intervals are robust for any kind of distribution

y.pred_ARIMA<-predict(fit_ARIMA,n.ahead=5)
y.pred_ARIMA$pred # point predictions
y.pred_ARIMA$se 
ts.plot(y.pred_ARIMA$pred)

############ SAR ############
# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
fit_SAR<-arima(y,order=c(8,1,0),seasonal=list(order=c(2,1,0),period=s)) 
fit_SAR

ts.plot(fit_SAR$residuals)
par(mfrow=c(2,1))
acf(fit_SAR$residuals,nlags)
pacf(fit_SAR$residuals,nlags)    

ndiffs(fit_SAR$residuals, alpha=0.05, test=c("adf")) # regular differences?

Box.test(fit_SAR$residuals,lag=15)
shapiro.test(fit_SAR$residuals)

y.pred_SAR<-predict(fit_SAR,n.ahead=5)
y.pred_SAR$pred   # point predictions
y.pred_SAR$se    # standard errors
ts.plot(y.pred_SAR$pred)  # see how the model captures the seasonality

############ SAR ############
# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
fit_SAR<-arima(y,order=c(8,1,0),seasonal=list(order=c(2,1,0),period=s)) 
fit_SAR

ts.plot(fit_SAR$residuals)
par(mfrow=c(2,1))
acf(fit_SAR$residuals,nlags)
pacf(fit_SAR$residuals,nlags)    

ndiffs(fit_SAR$residuals, alpha=0.05, test=c("adf")) # regular differences?

Box.test(fit_SAR$residuals,lag=15)
shapiro.test(fit_SAR$residuals)

y.pred_SAR<-predict(fit_SAR,n.ahead=5)
y.pred_SAR$pred   # point predictions
y.pred_SAR$se    # standard errors
ts.plot(y.pred_SAR$pred)  # see how the model captures the seasonality

############ SARIMA ############
# estimate the SARIMA and analyze the estimated parameters. Compare with the Seasonal Difference
fit_SARIMA<-arima(y,order=c(3,1,2),seasonal=list(order=c(2,1,0),period=s)) 
fit_SARIMA

ts.plot(fit_SARIMA$residuals)
par(mfrow=c(2,1))
acf(fit_SARIMA$residuals,nlags)
pacf(fit_SARIMA$residuals,nlags)    

ndiffs(fit_SARIMA$residuals, alpha=0.05, test=c("adf")) # regular differences?

Box.test(fit_SARIMA$residuals,lag=15)
shapiro.test(fit_SARIMA$residuals)

y.pred_SARIMA<-predict(fit_SARIMA,n.ahead=5)
y.pred_SARIMA$pred   # point predictions
y.pred_SARIMA$se    # standard errors
ts.plot(y.pred_SARIMA$pred)  # see how the model captures the seasonality

############ EVALUATION ############

predictions <- data.frame(SARIMA = y.pred_SARIMA$pred, SAR = y.pred_SAR$pred, AR = y.pred_AR$pred, ARIMA = y.pred_ARIMA$pred)
standars_errors <- data.frame(SARIMA = y.pred_SARIMA$se, SAR = y.pred_SAR$se, AR = y.pred_AR$se, ARIMA = y.pred_ARIMA$se)
real <- datos[103:107,2]
  
error_SAR <- real - y.pred_SAR$pred
MSFE_SAR <- mean(error_SAR^2)
MAPE_SAR <- mean(abs(error_SAR/real)) *100

error_AR <- real - y.pred_AR$pred
MSFE_AR <- mean(error_AR^2)
MAPE_AR <- mean(abs(error_AR/real)) *100

error_ARIMA <- real - y.pred_ARIMA$pred
MSFE_ARIMA <- mean(error_ARIMA^2)
MAPE_ARIMA <- mean(abs(error_ARIMA/real)) *100

error_SARIMA <- real - y.pred_SARIMA$pred
MSFE_SARIMA <- mean(error_SARIMA^2)
MAPE_SARIMA <- mean(abs(error_SARIMA/real)) *100


df <- data.frame(MSFE=c(MSFE_SAR, MSFE_AR, MSFE_ARIMA, MSFE_SARIMA), MAPE = c(MAPE_SAR ,MAPE_AR, MAPE_ARIMA, MAPE_SARIMA))
rownames(df) <- c("SAR", "AR", "ARIMA","SARIMA")
df

plot(y.pred_SAR$pred)
lines(y.pred_AR$pred)
lines(y.pred_ARIMA$pred)
lines(datos[103:107,2])

