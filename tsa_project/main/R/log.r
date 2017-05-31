library(curl)
tmpf <- tempfile()
curl_download("http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.5.0.0.monthly_ns_avg.txt", tmpf)
gtemp <- read.table(tmpf)[, 1:2]
temp = gtemp$V2[1:2004]
#de-seasonal
myTS = ts(as.numeric(temp), start = c(1850, 1), frequency = 12)
myTS.additive = decompose(myTS)
myTS.adjusted = myTS.additive$x - myTS.additive$seasonal

st = diff(myTS.adjusted)
arfima(st)
# large order bad

y = as.numeric(log(myTS.adjusted + 1 - min(myTS.adjusted)))[600:2004]
ty = 1:length(y)
trend = lm(y ~ ty)
tr2 = lm(y ~ ty + I(cos((ty - 208)*pi/792)))

# R^2 > 0.7
# adding I(ty^2) or log(ty^2) wouldn't increase R^2
summary(trend)
summary(tr2)
random = y - 3.309e-04*ty - 4.259e-01
lmodel = arfima(random)
summary(lmodel)

library(FinTS)