# Heiss example 13-1
# Return to education and the Gender/Wage gap

rm(list = ls())
library(foreign)
cps <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/cps78_85.dta")

# Detailed OLS results including interaction terms
summary( lm(lwage ~ y85*(educ+female) +exper+ I((exper^2)/100) + union, data=cps) )
