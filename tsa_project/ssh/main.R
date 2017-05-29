library(curl)
library(forecast)
library(TSA)
library(aTSA)
library(rugarch)
my_sGARCH_test <- function(p, q, m, n, ts.data = res)
{
  myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)), mean.model = list(armaOrder = c(m, n), include.mean = FALSE), distribution.model = "norm")
  myfit=ugarchfit(myspec,data=ts.data, solver="solnp")
  # extracting from fit result
  return(myfit)  
}

my_arch_test <- function(p, q, ts.data)
{
  std.gres = my_sGARCH_test(p, q, ts.data)@fit$z
  return(arch.test(arima(std.gres, c(0, 0, 0))))
}

tmpf <- tempfile()
curl_download("http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.5.0.0.monthly_ns_avg.txt", tmpf)
gtemp <- read.table(tmpf)[, 1:2]
temp = gtemp$V2[1:2004]
#de-seasonal
myTS = ts(as.numeric(temp), start = c(1850, 1), frequency = 12)
myTS.additive = decompose(myTS)
myTS.multiple = decompose(myTS, type = "multiplicative")
# seems that inside decompose return value:
# x(original) = seasonal(figure) + trend + random(residuals) 
res = myTS$random[7:1998]

fit = my_sGARCH_test(1, 1, 1, 2, res)
fore = ugarchforecast(fit)
plot(fore)










armaModel = auto.arima(res)
arma_residual = armaModel$residual
# ARMA(1, 2) all significant
Box.test(arma_residual, type = 'Ljung-Box')
# p = 0.2491
adf.test(arma_residual)
#Augmented Dickey-Fuller Test

#data:  ar_residual[-(1:6)]
#Dickey-Fuller = -16.875, Lag order = 12, p-value = 0.01
#alternative hypothesis: stationary
# stationary

gfit = my_sGARCH_test(c(1, 1), c(0, 0), ar_residual)



my_sGARCH_test <- function(garchorder, armaorder, ts.data)
{
  myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = garchorder), mean.model = list(armaOrder = armaorder, include.mean = TRUE), distribution.model = "norm")
  myfit=ugarchfit(myspec,data=ts.data, solver="solnp")
  # extracting from fit result
  return(myfit)  
}
#gtest = my_sGARCH_test(armaorder = c(1, 1), garchorder = c(5, 0), ts.data = temp)


my_checkResNormal <- function(p, q, m, n, ts.data = res)
{
  f = my_sGARCH_test(p, q, m, n, ts.data = res)
  shapiro.test(residuals(f)/sigma(f))
}

setpar <- function(i1, i2)
{
  par(mfrow=c(i1, i2))
}










