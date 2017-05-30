library(curl)
tmpf <- tempfile()
curl_download("http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.5.0.0.monthly_ns_avg.txt", tmpf)
gtemp <- read.table(tmpf)[, 1:2]
temp = gtemp$V2[1:2004]
#de-seasonal
myTS = ts(as.numeric(temp), start = c(1850, 1), frequency = 12)
myTS.additive = decompose(myTS)
myTS.adjusted = myTS.additive$x - myTS.additive$seasonal

library(fracdiff)
fdGPH(myTS.adjusted)
dtemp = diffseries(myTS.adjusted, 0.8925434)
adf.test(dtemp)
kpss.test(dtemp)
plot.ts(dtemp)
acf(dtemp)
ar1 = arima(dtemp, c(1, 0, 0))
# check arima(1, 0, 0)
auto.arima(ar1$residuals)
arma2.2 = arima(dtemp, c(2, 0, 2))
# check arima(2, 0, 2)
auto.arima(arma2.2$residuals)

lmodel = arfima(dtemp)

library(rugarch)

