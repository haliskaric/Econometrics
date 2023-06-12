y <- c(17.8, 39.0, 12.8, 24.2, 17.2)
x <- c(13.7, 23.2, 6.9, 16.8, 12.3)
bhat <- cov(x,y)/var(x)
bhat
ahat <- mean(y) - bhat*mean(x)
ahat

ex1 <- lm(y~x)
summary(ex1)
ex1
