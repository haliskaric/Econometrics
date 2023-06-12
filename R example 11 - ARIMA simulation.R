rm(list = ls()) # clear all variables in workspace
graphics.off() # clear all plots

install.packages("devtools")
devtools::install_github("KevinKotze/tsm")
#devtools::install_github("cran/fArma")   # Not working

## Simulation of AR(1) model
set.seed(123)
x <- arima.sim(model = list(ar = 0.8), n = 1000)
plot.ts(x)

acf(x, max.lag = 18)
pacf(x, max.lag = 18)

# Box-Ljung test
Box.test(x, lag = 1, type = "Ljung-Box")
Box.res <- Box.test(x, lag = 1, type = "Ljung-Box")
Box.res

Box.test(x, lag = 1, type = "Ljung-Box")$statistic
Box.test(x, lag = 1, type = "Ljung-Box")$p.value

# Fit AR(1) model
arma10 <- arima(x, order = c(1, 0, 0), include.mean = FALSE) # uses ARIMA(p,d,q) specification
arma10

par(mfrow = c(1, 1))
plot(arma10$residuals)

acf(arma10$residuals, max.lag = 18)
pacf(arma10$residuals, max.lag = 18)

# MA(1) model
y <- arima.sim(model = list(ma = 0.7), n = 1000)
par(mfrow = c(1, 1))
plot.ts(y)

acf(y, max.lag = 18)
pacf(y, max.lag = 18)

arma01 <- arima(y, order = c(0, 0, 1)) # uses ARIMA(p,d,q) with constant
arma01
par(mfrow = c(1, 1))
plot(arma01$residuals)

# ARMA(1,1) model
x <- arima.sim(model = list(ar = 0.4), n = 200) # AR(1) process
acf(x, max.lag = 20)
pacf(x, max.lag = 20)

x <- arima.sim(model = list(ma = 0.5), n = 200) # MA(1) process
acf(x, max.lag = 20)
pacf(x, max.lag = 20)

x <- arima.sim(model = list(ar = 0.4, ma = 0.5), n = 200) ## ARMA(1,1)
acf(x, max.lag = 20)
pacf(x, max.lag = 20)

z <- arima.sim(model = list(ar = c(0.6, -0.2), ma = c(0.4)), n = 200)
acf(z, max.lag = 18)
pacf(z, max.lag = 18)

# The results from the ACF & PACF would suggest that we are at most dealing with an ARMA(3,2). 
# To estimate all models that may have an order that is equal to or less than order we could proceed as follows. 
# Store the AIC value in the object arma.res .
arma.res <- rep(0, 16)
arma.res[1] <- arima(z, order = c(3, 0, 2))$aic # fit arma(3,2) and save aic value
arma.res[2] <- arima(z, order = c(2, 0, 2))$aic
arma.res[3] <- arima(z, order = c(2, 0, 1))$aic
arma.res[4] <- arima(z, order = c(1, 0, 2))$aic
arma.res[5] <- arima(z, order = c(1, 0, 1))$aic
arma.res[6] <- arima(z, order = c(3, 0, 0))$aic
arma.res[7] <- arima(z, order = c(2, 0, 0))$aic
arma.res[8] <- arima(z, order = c(0, 0, 2))$aic
arma.res[9] <- arima(z, order = c(3, 0, 2), include.mean = FALSE)$aic
arma.res[10] <- arima(z, order = c(2, 0, 2), include.mean = FALSE)$aic
arma.res[11] <- arima(z, order = c(2, 0, 1), include.mean = FALSE)$aic
arma.res[12] <- arima(z, order = c(1, 0, 2), include.mean = FALSE)$aic
arma.res[13] <- arima(z, order = c(1, 0, 1), include.mean = FALSE)$aic
arma.res[14] <- arima(z, order = c(3, 0, 0), include.mean = FALSE)$aic
arma.res[15] <- arima(z, order = c(2, 0, 0), include.mean = FALSE)$aic
arma.res[16] <- arima(z, order = c(0, 0, 2), include.mean = FALSE)$aic
# To find the model that has the lowest value for the AIC statistic we could execute the code:
which(arma.res == min(arma.res))



## Simulating AR, MA, and ARMA Time Series
## R : Copyright 2003, The R Development Core Team Version 1.7.1 (2003-06-16)
ar.sim<-arima.sim(model=list(ar=c(.9,-.2)),n=100)
#ar.sim
ts.plot(ar.sim)
# Calculate the Sample Autocorrelation Function
acf(ar.sim,type="correlation",plot=T)
acf(ar.sim,type="partial",plot=T)   # A different but equivalent command for PACF

ma.sim<-arima.sim(model=list(ma=c(-.7,.1)),n=100)
ma.sim
ts.plot(ma.sim)
acf(ma.sim,type="correlation",plot=T)
acf(ma.sim,type="partial",plot=T)   # A different but equivalent command for PACF

arma.sim<-arima.sim(model=list(ar=c(.9,-.2),ma=c(-.7,.1)),n=100)
arma.sim
ts.plot(arma.sim)
acf(arma.sim,type="correlation",plot=T)
acf(arma.sim,type="partial",plot=T)

