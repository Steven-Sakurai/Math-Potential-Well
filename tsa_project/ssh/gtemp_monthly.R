library(curl)
library(forecast)
library(TSA)
library(aTSA)
library(rugarch)
my_sGARCH_test <- function(p, q, m, n, ts.data = res)
{
    myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)), mean.model = list(armaOrder = c(m, n), include.mean = TRUE), distribution.model = "norm")
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
myTS = decompose(ts(as.numeric(temp), frequency = 12))
# seems that inside decompose return value:
# x(original) = seasonal(figure) + trend + random(residuals) 
res = myTS$random[7:1998]

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


gres = gfit@fit$residuals





temp.diff = diff(gtemp$V2)
adf.test(temp.diff)
# small p-value suggest stationarity
# 1850 ~ 2017 + 4month - 1:  2007 observ.
auto.arima(temp.diff)


my_sGARCH_test <- function(garchorder, armaorder, ts.data)
{
    myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = garchorder), mean.model = list(armaOrder = armaorder, include.mean = TRUE), distribution.model = "norm")
    myfit=ugarchfit(myspec,data=ts.data, solver="solnp")
    # extracting from fit result
    return(myfit)  
}
#gtest = my_sGARCH_test(armaorder = c(1, 1), garchorder = c(5, 0), ts.data = temp)


my_checkResNormal <- function(gorder, aorder, ts.data)
{
    f = my_sGARCH_test(gorder, aorder, ts.data)
    shapiro.test(f$residuals)
}


myspec=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(3, 3)), distribution.model = "norm")
myfit=ugarchfit(myspec,data=temp.diff,solver="solnp")
# extracting from fit result
res = myfit@fit$residuals
Box.test(res, type = 'Ljung-Box')
plot.ts(res)

setpar <- function(i1, i2)
{
    par(mfrow=c(i1, i2))
}

