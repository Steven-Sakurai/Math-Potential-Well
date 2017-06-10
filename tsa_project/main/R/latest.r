library(curl)
library(forecast)
library(TSA)
library(rugarch)

tmpf <- tempfile()
curl_download("http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.5.0.0.monthly_ns_avg.txt", tmpf)
gtemp <- read.table(tmpf)[, 1:2]

gtemp = as.numeric(gtemp$V2)
mytemp =  gtemp[600:1980] 
plot.ts(mytemp)
acf(mytemp)
pacf(mytemp)
m1 = auto.arima(mytemp, seasonal = TRUE)
m2 = arima(mytemp, order = c(3, 1, 1), seasonal = list(order = c(1, 0, 1), period = 24))
res2 = residuals(m2)
acf(res2)

m3 = arfima(mytemp)
res3 = residuals(m3)
res4 = residuals(arima(res3, seasonal = list(order = c(1, 0, 1), period = 24)))


# seasonal adjustment
atemp = diff(mytemp, lag = 24)
m5 = auto.arima(atemp)
res5 = residuals(m5)
acf(res5)
pacf(res5)
