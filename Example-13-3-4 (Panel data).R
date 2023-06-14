# Heiss example 13-4
# Crime rates and unemployment
rm(list=ls())

library(foreign); library(plm)

crime2 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime2.dta")

# define panel data frame
crime2.p <- pdata.frame(crime2, index=46)

# Panel dimensions
pdim(crime2.p)

# Observations 1-6
crime2.p[1:6, c("id", "time", "year", "pop", "crimes", "crmrte", "unem")]
