install.packages("astsa")
par(mfrow = c(1,1))
# 1.1 The Nature of Time Series Data

# Example 1.1 Johnson & Johnson Quarterly Earnings
library(astsa)
plot(jj, type="o", ylab="Quarterly Earnings per Share")

# Example 1.2 Global Warming
plot(globtemp, type="o", ylab="Global Temperature Deviations")

# Example 1.3 Speech Data
plot(speech)

# Example 1.4 Dow Jones Industrial Average
install.packages("TTR")
library(TTR)
djia = getYahooData("^DJI", start=20060420, end=20160420, freq="daily")
library(xts)
djiar = diff(log(djia$Close))[-1] #approximate returns
plot(djiar, main="DJIA Returns", type="n")
lines(djiar)

# Example 1.5 El Nino and Fish Population
par(mfrow = c(2,1)) # set up the graphics
plot(soi, ylab="", main="Southern Oscillation Index")
plot(rec, ylab="", main="Recruitment")

# Example 1.6 fMRI Imaging
par(mfrow = c(2,1))
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", main="Cortex")
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", main="Thalamus & Cerebellum")

# Example 1.7 Earthquakes and Explosions
par(mfrow = c(2,1))
plot(EQ5, main="Earthquake")
plot(EXP6, main="Explosion")


# 1.2 Time Series Statistical Models

# Example 1.9 Moving Averages and Filtering
w = rnorm(500,0,1) # 500 N(0,1) variates
v = filter(w, sides=2, filter=rep(1/3,3)) #moving average
par(mfrow = c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, ylim=c(-3,3), main="moving average")

# Example 1.10 Autoregressions
par(mfrow = c(1,1))
w = rnorm(550,0,1) # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)] # remove first 50
plot.ts(x,main="autoregression")

# Example 1.11 Random Walk with Drift
set.seed(154) # so you can reproduce the results
w = rnorm(200); x = cumsum(w) # two commands in one line
wd = w + .2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk", ylab='')
lines(x, col=4); abline(h=0, col=4, lty=2); abline(a=0, b=.2, lty=2)

# Example 1.12 Signal in Noise
cs = 2*cos(2*pi*1:500/50+.6*pi); w = rnorm(500,0,1)
par(mfrow = c(3,1))
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi)+N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi)+N(0,25)))

# 1.3 Measures of Dependence

# 1.4 Stationary Time Series

# Example 1.24 Prediction Using Cross-Correlation
x = rnorm(100)
y = lag(x, -5) + rnorm(100)
ccf(y, x, ylab='CCovF', type='covariance')

# 1.5 Estimation of Correlation

# Example 1.25 Sample ACF and Scatterplots
(r = round(acf(soi, 6, plot=FALSE)$acf[-1], 3)) # first 6 sample acf values
par(mfrow = c(1,2))
plot(lag(soi,-1), soi); legend('topleft', legend=r[1])
plot(lag(soi,-6), soi); legend('topleft', legend=r[6])

# Example 1.26 A Simulated Time Series
set.seed(101010)
x1 = 2*rbinom(11, 1, .5)-1 # simulated sequence of coin tosses
x2 = 2*rbinom(101, 1, .5)-1
y1 = 5 + filter(x1, sides = 1, filter = c(1, -.7))[-1]
y2 = 5 + filter(x2, sides = 1, filter = c(1, -.7))[-1]
plot.ts(y1, type='s'); plot.ts(y2, type='s') # plot both series
c(mean(y1),mean(y2))
acf(y1, lag.max=4, plot=FALSE) # 1/sqrt(10)= 0.32
acf(y2, lag.max=4, plot=FALSE) # 1/sqrt(100)= 0.1

# Example 1.27 ACF of a Speech Signal
acf(speech, 250)

# Example 1.28 SOI and Recruitement Correlation Analysis
par(mfrow=c(3,1))
acf(soi, 48, main="Southern Oscillation Index")
acf(rec, 48, main="Recruitment")
ccf(soi, rec, 48, main="SOI vs Recruitment", ylab="CCF")

# Example 1.29 Prewhitening and Cross Correlation Analysis
set.seed(1492)
num=120; t=1:num
X = ts(2*cos(2*pi*t/12) + rnorm(num), freq=12)
Y = ts(2*cos(2*pi*(t+5)/12) + rnorm(num), freq=12)
Yw = resid(lm(Y~cos(2*pi*t/12)+sin(2*pi*t/12), na.action = NULL))
par(mfrow=c(3,2), mgp=c(1.6,.6,0),mar=c(3,3,1,1))
plot(X)
plot(Y)
acf(X,48,ylab='ACF(X)')
acf(Y,48,ylab='ACF(Y)')
ccf(X,Y,24, ylab='CCF(X,Y)')
ccf(X,Yw,24, ylab='CCF(X,Yw)', ylim=c(-.6,.6))

# 1.6 Vector-Valued and Multidimensional Series

# Example 1.30 Soil Surface Temperatures
persp(1:64, 1:36, soiltemp, phi=25, theta=25, scale=FALSE, expand=4,
      ticktype="detailed", xlab="rows", ylab="cols", zlab="temperature")
plot.ts(rowMeans(soiltemp), xlab="row", ylab="Average Temperature")

# Example 1.31 Sample ACF of the Soil Temperature Series
fs = Mod(fft(soiltemp-mean(soiltemp)))^2/(64*36)
cs = Re(fft(fs, inverse=TRUE)/sqrt(64*36)) # ACovF
rs = cs/cs[1,1]
rs2 = cbind(rs[1:41,21:2], rs[1:41,1:21])
rs3 = rbind(rs2[41:2,], rs2)
par(mar=c(1,2.5,0,0)+.1)
persp(-40:40, -20:20, rs3, phi=30, theta=30, expand=30, scale=FALSE,
      ticktype="detailed", xlab="row lags", ylab="column lags",
      zlab="ACF")
