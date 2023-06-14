# Heiss example 13-3
# Effects of the construction of a new garbage incinerator on the value of nearby houses
rm(list=ls())

library(foreign)
kielmc <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/kielmc.dta")
 
# Separate regressions for 1978 and 1981: report coefficients only
coef( lm(rprice~nearinc, data=kielmc, subset=(year==1978)) ) # before the news of the Incinerator
coef( lm(rprice~nearinc, data=kielmc, subset=(year==1981)) ) # Construction began in 1981
# What is the effect of the new Incinerator on house price?

# Joint regression including an interaction term 
library(lmtest)
coeftest( lm(rprice~nearinc*y81, data=kielmc) )

# Log specification is preferred
# DiD specification
DiD      <- lm(log(rprice)~nearinc*y81, data=kielmc)
DiDcontr <- lm(log(rprice)~nearinc*y81+age+I(age^2)+log(intst)+
                 log(land)+log(area)+rooms+baths, data=kielmc)
library(stargazer)
stargazer(DiD,DiDcontr,type="text")

