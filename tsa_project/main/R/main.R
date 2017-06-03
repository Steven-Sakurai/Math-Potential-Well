library(curl)
library(forecast)
library(TSA)
library(aTSA)
library(rugarch)

tmpf <- tempfile()
curl_download("http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.5.0.0.monthly_ns_avg.txt", tmpf)
gtemp <- read.table(tmpf)[, 1:2]
temp = gtemp$V2[1:2004]
#de-seasonal
myTS = ts(as.numeric(temp), start = c(1850, 1), frequency = 12)
myTS.additive = decompose(myTS)
# myTS.multiple = decompose(myTS, type = "multiplicative")
myTS.adjusted = as.numeric(myTS.additive$x - myTS.additive$seasonal)
#res = myTS.additive$random

auto.arima(myTS.adjusted)
# (2, 0, 1) or (2, 1, 4)
arima1 = Arima(myTS.adjusted, c(2, 0, 1))
auto.arima(arima1$residuals)
arima2 = Arima(myTS.adjusted, order = c(2, 1, 4))
auto.arima(arima2$residuals)
acf(arima1$residuals)
acf(arima2$residuals)
pacf(arima1$residuals)
pacf(arima2$residuals)

plot(forecast.Arima(arima2, h = 240))

sarima = Arima(myTS.adjusted[1:1800], c(2, 1, 4))
plot(forecast.Arima(sarima, h = 203))
lines(myTS.adjusted)



# Route 1

dtemp = diff(myTS.adjusted)
adf.test(dtemp)
auto.arima(dtemp)
arma21.dtemp = arima(dtemp, c(2, 0, 1))
arma24.dtemp = arima(dtemp, c(2, 0, 4))
auto.arima(arma21.dtemp$residuals)
auto.arima(arma24.dtemp$residuals)
my.arma = arma24.dtemp
my.arma
res = my.arma$residuals
Box.test(res, type = 'Ljung-Box')
arch.test(my.arma)

fit1 = my_sGARCH_test(1, 1, 2, 3, dtemp)
z = as.numeric(residuals(fit1) / sigma(fit1))
plot.ts(z)
mean(z)
var(z)
length(z)
plot.ts(rnorm(2003, 0.03181525, 1.013866))
plot(fit1)
acf(z)

fore1 = ugarchforecast(fit1, n.ahead = 24)
fore.diff = as.numeric(fore1@forecast$seriesFor)
fore.sigma = as.numeric(fore1@forecast$sigmaFor)
ts.predict = temp[length(temp)] + cumsum(fore.diff)
ts.predict = ts.predict + myTS.additive$figure
ts.sigma = sqrt(cumsum(fore.sigma^2))
tsup.sigma = ts.predict + ts.sigma
tsdown.sigma = ts.predict - ts.sigma
plot(1:24, ts.predict, ylim=c(0,1.5), type = 'l', col = 'blue', xlab = "months", ylab = "temperature predict")
lines(1:24, tsup.sigma, type = 'l', col = 'red')
lines(1:24, tsdown.sigma, type = 'l', col = 'red')


my_sGARCH_test <- function(p, q, m, n, ts.data)
{
    myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)), mean.model = list(armaOrder = c(m, n), include.mean = FALSE), distribution.model = "norm")
    myfit=ugarchfit(myspec,data=ts.data, solver="solnp")
    # extracting from fit result
    return(myfit)  
}



my_eGARCH_test <- function(p, q, m, n, ts.data)
{
    myspec=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(p, q)), mean.model = list(armaOrder = c(m, n), include.mean = FALSE), distribution.model = "norm")
    myfit=ugarchfit(myspec,data=ts.data, solver="solnp")
    # extracting from fit result
    return(myfit)  
}

std_sGARCH_test <- function(p, q, m, n, ts.data)
{
    myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)), mean.model = list(armaOrder = c(m, n), include.mean = FALSE), distribution.model = "std")
    
    myfit=ugarchfit(myspec,data=ts.data, solver="solnp")
    # extracting from fit result
    return(myfit)  
}
stdmodel = std_eGARCH_test(1, 1, 2, 3, dtemp)

# branches
efit = my_eGARCH_test(2, 0, 2, 3, dtemp)

atemp = temp[480:2004]
t = 480:2004
trend.model = lm(atemp ~ t + I(t^2))
det = atemp + 2.280e-04 * t - 3.370e-07 * t^2







# Route 2

library(fracdiff)
d = fdGPH(myTS.adjusted)
diff.temp = diffseries(myTS.adjusted, d = 0.8925434)
plot.ts(diff.temp)
auto.arima(diff.temp)
arma10.m = arima(diff.temp, c(1, 0, 0))
arma22.m = arima(diff.temp, c(2, 0, 2))
auto.arima(arma10.m$residuals)
auto.arima(arma22.m$residuals)

fit2 = my_sGARCH_test(2, 2, 2, 2, diff.temp)
z2 = fit2@fit$z
acf(z2)

# Route 3
t = as.numeric(1:2004)
md = lm(temp ~ t + I(t^2))
t2 = temp - md$coefficients[2] * t - md$coefficients[3] * t^2
plot.ts(t2)
t2 = (t2 - mean(t2))/sqrt(var(t2))
plot.ts(t2)
d = fdGPH(t2)
tmp = diffseries(t2, d$d)
diff.temp = diffseries(tmp, fdGPH(tmp)$d)
plot.ts(diff.temp)
acf(diff.temp)

# Route 4
# in folder 'another'


# convenient functions

my_checkResNormal <- function(p, q, m, n, ts.data = res)
{
    f = my_sGARCH_test(p, q, m, n, ts.data = res)
    shapiro.test(residuals(f)/sigma(f))
}

setpar <- function(i1, i2)
{
    par(mfrow=c(i1, i2))
}

my_sGARCH_test <- function(p, q, m, n, ts.data)
{
    myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)), mean.model = list(armaOrder = c(m, n), include.mean = FALSE), distribution.model = "norm")
    myfit=ugarchfit(myspec,data=ts.data, solver="solnp")
    # extracting from fit result
    return(myfit)  
}

my_eGARCH_test <- function(p, q, m, n, ts.data)
{
    myspec=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(p, q)), mean.model = list(armaOrder = c(m, n), include.mean = FALSE), distribution.model = "norm")
    myfit=ugarchfit(myspec,data=ts.data, solver="solnp")
    # extracting from fit result
    return(myfit)  
}





























my.temp = as.numeric(myTS.adjusted)
dtemp = diff(my.temp)
stddtemp = (dtemp - mean(dtemp))/var(dtemp)
s = stddtemp[500:length(stddtemp)]
myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)), mean.model = list(armaOrder = c(1, 0), include.mean = FALSE, arfima = TRUE), distribution.model = "std")
myfit=ugarchfit(myspec,data=s, fit.control=list(scale=TRUE))
acf(myfit@fit$residuals)

















# fracdiff
memory.long <- fracdiff.sim(1500, d = 0.3)
fdGPH(memory.long$series)

memory.long <- fracdiff.sim(80, d = 0.3)
mGPH <- fdGPH(memory.long$series)
r <- diffseries(memory.long$series, d = mGPH$d)
acf(memory.long$series)
acf(r)
