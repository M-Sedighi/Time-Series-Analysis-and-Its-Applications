install.packages("dynlm")
par(mfrow = c(1,1))
library(astsa)

# 2.1 Classical Regression in the Time Series Context

# Example 2.1 Estimating a Linear Trend
summary(fit <- lm(chicken ~ time(chicken), na.action = NULL))
plot(chicken, ylab="cents per pound")
abline(fit) # add the fitted line

# Example 2.2 Pollution, Temperature and Mortality
par(mfrow=c(3,1)) # plot the data
plot(cmort, main="Cardiovascular MOrtality", xlab="", ylab="")
plot(tempr, main="Temperature", xlab="", ylab="")
plot(part, main="Particulate", xlab="", ylab="")
dev.new() # open a new graphic device
ts.plot(cmort, tempr, part, col=1:3) # all on same plot
dev.new() 
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))
temp = tempr - mean(tempr) # center temperature
temp2 = temp^2
trend = time(cmort)
fit = lm(cmort ~ trend + temp + temp2 + part, na.action = NULL)
summary(fit) # regression results
summary(aov(fit)) # ANOVA table 
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) #  Table 2.1
num = length(cmort) # sample size
AIC(fit)/num - log(2*pi) # AIC
BIC(fit)/num - log(2*pi) # BIC
(AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-5-2)) # AICc

# Example 2.3 Regression with Lagged Variables
fish = ts.intersect(rec, soiL6=lag(soi, -6), dframe=TRUE) # intersecting the lagged SOI with recruitement
summary(fit1 <- lm(rec~soiL6, data=fish, na.action = NULL))
library(dynlm)
summary(fit2 <- dynlm(rec~L(soi,6)))

# Exploratory Data Analysis

# Example 2.4 Detrending Chicken Prices
acf(1:100)

# Example 2.5 Differencing Chicken Prices
fit = lm(chicken~time(chicken), na.action = NULL) # regress chicken on time
par(mfrow=c(2,1))
plot(resid(fit), type="o", main = "detrended")
plot(diff(chicken), type="o", main = "first difference")
par(mfrow=c(3,1)) # plot ACFs
acf(chicken, 488, main="chicken")
acf(resid(fit), 48, main="detrended")
acf(diff(chicken), 48, main="fist difference")

# Example 2.6 Differencing Global Temperatures
par(mfrow=c(2,1))
plot(diff(globtemp), type="o")
mean(diff(globtemp))
acf(diff(globtemp), 48)

# Example 2.7 Paleoclimatic Glacial Varves
par(mfrow=c(2,1))
plot(varve, main="varve", ylab="")
plot(log(varve), main="log(varve)", ylab="")

# Example 2.8 Scatterplot Matrices, SOI and Recuitment
lag1.plot(soi, 12)
lag2.plot(soi, rec, 8)

# Example 2.9 Regression with Lagged Variables (cont)
dummy = ifelse(soi<0, 0, 1)
fish = ts.intersect(rec, soiL6=lag(soi, -6), dL6=lag(dummy, -6), dframe=TRUE)
summary(fit <- lm(rec~ soiL6*dL6, data=fish, na.action=NULL))
attach(fish)
plot(soiL6, rec)
lines(lowess(soiL6, rec), col=4, lwd=2)
points(soiL6, fitted(fit), pch='+', col=2)
plot(resid(fit))
acf(resid(fit)) # obviously not noise

# Example 2.10 Using Regression to discover a Signal in Noise
set.seed(90210)
x = 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)
z1 = cos(2*pi*1:500/50)
z2 = sin(2*pi*1:500/50)
summary(fit <- lm(x~0+z1+z2)) # zero to exclude the intercept
par(mfrow=c(2,1))
plot.ts(x)
plot.ts(x, col=8, ylab=expression(hat(x)))
lines(fitted(fit), col=2)

# 2.3 Smoothing in the Time Series Context

# Example 2.11 Moving Average Smoother
dev.new() 
wgts = c(.5, rep(1,11), .5)/12
soif = filter(soi, sides=2, filter=wgts)
plot(soi)
par(fig = c(.65, 1, .65, 1), new=TRUE) # the insert
nwgts = c(rep(0,20), wgts, rep(0,20))
plot(nwgts, type="l", ylim=c(-0.02, .1), xaxt='n', yaxt='n', ann=FALSE)

# Example 2.12 Kernel Smoothing
dev.new()
plot(soi)
lines(ksmooth(time(soi), soi, "normal", bandwidth = 1), lwd=2, col=4)
par(fig = c(.65, 1, .65, 1), new=TRUE) # the insert
gauss = function(x) {1/sqrt(2*pi)*exp(-(x^2)/2)}
x = seq(from = -3, to = 3, by = .001)
plot(x, gauss(x), type="l", ylim=c(-.02, .45), xaxt='n', yaxt='n', ann=FALSE)

# Example 2.13 Lowess
dev.new()
plot(soi)
lines(lowess(soi, f=.05), lwd=2, col=4) # El Nino cycle
lines(lowess(soi), lty=2, lwd=2, col=2) # Trend (with default cycle f=2/3)

# Example 2.14 Smoothing Splines
dev.new()
plot(soi)
lines(smooth.spline(time(soi), soi, spar=.5), lwd=2, col=4)
lines(smooth.spline(time(soi), soi, spar=1), lty=2, lwd=2, col=2)

# Example 2.15 Smoothing one Series as a Function of Another
plot(tempr, cmort, xlab="Temperature", ylab="Mortality")
lines(lowess(tempr,cmort))
