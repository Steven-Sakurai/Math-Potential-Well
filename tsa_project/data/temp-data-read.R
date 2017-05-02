
library(zoo)  # for na.approx

GISS_read <- function (file)
{
  lines <- readLines (file)
  lines <- grep ("^[0-9]{4}[ \t]", lines, value=TRUE)
  lines <- gsub (
   "^(.................................................................).*",
   "\\1", lines)
  lines <- gsub (" \\*\\*\\*\\*","   NA",lines)
  lines <- gsub ("\\*\\*\\*\\*\\*","   NA",lines)
  read.table (textConnection(lines), head=FALSE, row.names=1)
}

GISS_GLB <- GISS_read("data/GLB.Ts+dSST.txt")

GISS_NH  <- GISS_read("data/NH.Ts+dSST.txt")
GISS_SH  <- GISS_read("data/SH.Ts+dSST.txt")

GISS_GLB_Land <- GISS_read("data/GLB.Ts.txt")

GISS_NH_Land <- GISS_read("data/NH.Ts.txt")
GISS_SH_Land <- GISS_read("data/SH.Ts.txt")

GISS_GLB_old <- GISS_read("data/GLB.Ts+dSST-old.txt")

GISS_GLB_Land_old <- GISS_read("data/GLB.Ts-old.txt")

GISS_NH_Land_old <- GISS_read("data/NH.Ts-old.txt")
GISS_SH_Land_old <- GISS_read("data/SH.Ts-old.txt")

GISS_GLB_old2 <- GISS_read("data/GLB.Ts+dSST-old2.txt")
GISS_GLB_old3 <- GISS_read("data/GLB.Ts+dSST-old3.txt")

GISS_as_vector <- function (d) as.vector(t(as.matrix(d))) / 100


# Read UAH 6.0beta4 temperature data.

UAH <- read.table (
          textConnection(readLines("data/uahncdc_lt_6.0beta4.txt")[1:443]),
          head=TRUE)


# Read multivariate ENSO index data.

lines <- readLines ("data/MEI.html")
lines <- grep ("^[0-9]{4}[ \t]", lines[-1], value=TRUE)
lines[length(lines)] <- paste (lines[length(lines)], "NA NA")

MEI <- read.table (textConnection(lines), head=FALSE)
colnames(MEI)[1] <- "YEAR"


# Read Atlantic Multidecadal Oscillation index data.

lines <- readLines ("data/amon.us.data")
lines <- grep ("^ [0-9]{4} ", lines, value=TRUE)

AMO <- read.table (textConnection(lines), head=FALSE)
colnames(AMO)[1] <- "YEAR"


# Read atmospheric transmission data.

lines <- readLines ("data/mlo_transmission.dat")
lines <- c (lines[-(1:2)],
  # Values manually taken from mlo_transmission.gif.  Due to the low 
  # resolution of that plot, some guesswork is involved.
  "AUG-2012   2012.6250      0.933",
  "SEP-2012   2012.7084      0.930",
  "OCT-2012   2012.7916      0.934",
  "NOV-2012   2012.8750      0.933",
  "DEC-2012   2012.9584      0.933",
  "JAN-2013   2013.0416      0.933",
  "FEB-2013   2013.1250      0.929",
  "MAR-2013   2013.2084      0.921",
  "APR-2013   2013.2916      0.926",
  "MAY-2013   2013.3750      0.930",
  "JUN-2013   2013.4584      0.930",
  "JUL-2013   2013.5416      0.934",
  "AUG-2013   2013.6250      0.933",
  "SEP-2013   2013.7084      0.930",
  "OCT-2013   2013.7916      0.929",
  "NOV-2013   2013.8750      0.931",
  "DEC-2013   2013.9584      0.931",
  "JAN-2014   2014.0416      0.931",
  "FEB-2014   2014.1250      0.923",
  "MAR-2014   2014.2084      0.924",
  "APR-2014   2014.2916      0.928",
  "MAY-2014   2014.3750      0.932",
  "JUN-2014   2014.4584      0.932",
  "JUL-2014   2014.5416      0.930",
  "AUG-2014   2014.6250      0.929",
  "SEP-2014   2014.7084      0.930",
  "OCT-2014   2014.7916      0.930",
  "NOV-2014   2014.8750      0.934",
  "DEC-2014   2014.9584      0.933")

TRANS <- read.table (textConnection(lines), head=FALSE)


# Read Total Solar Irradiance data.

TSI <- read.table (
         textConnection (readLines("data/TSI_TIM_Reconstruction.txt")[-(1:7)]),
         head=FALSE)


# Read sunspot numbers.

lines <- readLines("data/SN_m_tot_V2.0.txt")
lines <- gsub ("\\*"," ",lines)

SN <- read.table (textConnection(lines), head=FALSE)


# Read CO2 data.  Fill in two missing data points in Samoa data, and
# 23 missing data points in South Pole data.

co2_brw <- read.table ("data/co2_brw_surface-flask_1_ccgg_month.txt")

co2_mlo <- read.table ("data/co2_mlo_surface-flask_1_ccgg_month.txt")

co2_smo <- read.table ("data/co2_smo_surface-flask_1_ccgg_month.txt")
co2_smo[496,1] <- "SMO"
co2_smo[496,2] <- 2010
co2_smo[496,3] <- 4
co2_smo[496,4] <- (387.34*2/3 + 387.79/3)
co2_smo[497,1] <- "SMO"
co2_smo[497,2] <- 2010
co2_smo[497,3] <- 5
co2_smo[497,4] <- (387.34/3 + 387.79*2/3)
o <- order(co2_smo[,2]+co2_smo[,3]/12)
co2_smo <- co2_smo[o,]

co2_spo <- read.table ("data/co2_spo_surface-flask_1_ccgg_month.txt")
co2_spo$sw <- rep(0.5,nrow(co2_spo))
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 1
co2_spo$sw[nrow(co2_spo)] <- 1/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 2
co2_spo$sw[nrow(co2_spo)] <- 2/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 3
co2_spo$sw[nrow(co2_spo)] <- 3/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 4
co2_spo$sw[nrow(co2_spo)] <- 4/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 5
co2_spo$sw[nrow(co2_spo)] <- 5/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 6
co2_spo$sw[nrow(co2_spo)] <- 6/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 7
co2_spo$sw[nrow(co2_spo)] <- 7/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 8
co2_spo$sw[nrow(co2_spo)] <- 8/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 9
co2_spo$sw[nrow(co2_spo)] <- 9/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 10
co2_spo$sw[nrow(co2_spo)] <- 10/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 11
co2_spo$sw[nrow(co2_spo)] <- 11/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1980
co2_spo[nrow(co2_spo),3] <- 12
co2_spo$sw[nrow(co2_spo)] <- 12/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1981
co2_spo[nrow(co2_spo),3] <- 1
co2_spo$sw[nrow(co2_spo)] <- 13/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1981
co2_spo[nrow(co2_spo),3] <- 2
co2_spo$sw[nrow(co2_spo)] <- 14/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1981
co2_spo[nrow(co2_spo),3] <- 3
co2_spo$sw[nrow(co2_spo)] <- 15/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1981
co2_spo[nrow(co2_spo),3] <- 4
co2_spo$sw[nrow(co2_spo)] <- 16/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1981
co2_spo[nrow(co2_spo),3] <- 5
co2_spo$sw[nrow(co2_spo)] <- 17/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 1981
co2_spo[nrow(co2_spo),3] <- 6
co2_spo$sw[nrow(co2_spo)] <- 18/19
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 2001
co2_spo[nrow(co2_spo),3] <- 2
co2_spo$sw[nrow(co2_spo)] <- 1/6
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 2001
co2_spo[nrow(co2_spo),3] <- 3
co2_spo$sw[nrow(co2_spo)] <- 2/6
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 2001
co2_spo[nrow(co2_spo),3] <- 4
co2_spo$sw[nrow(co2_spo)] <- 3/6
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 2001
co2_spo[nrow(co2_spo),3] <- 5
co2_spo$sw[nrow(co2_spo)] <- 4/6
co2_spo[nrow(co2_spo)+1,1] <- "SPO"
co2_spo[nrow(co2_spo),2] <- 2001
co2_spo[nrow(co2_spo),3] <- 6
co2_spo$sw[nrow(co2_spo)] <- 5/6
o <- order(co2_spo[,2]+co2_spo[,3]/12)
co2_spo <- co2_spo[o,]
co2_spo_if <- co2_spo
for (i in 1:nrow(co2_spo))
{ if (is.na(co2_spo[i,4]))
  {  co2_spo_if[i,4] <- co2_spo_if[i-1,4]+co2_spo_if[i-12,4]-co2_spo_if[i-13,4]
  }
}
co2_spo_ib <- co2_spo
for (i in nrow(co2_spo):1)
{ if (is.na(co2_spo[i,4]))
  {  co2_spo_ib[i,4] <- co2_spo_ib[i+1,4]+co2_spo_ib[i+12,4]-co2_spo_ib[i+13,4]
  }
}
co2_spo$na <- is.na(co2_spo[,4])
co2_spo[,4] <- (1-co2_spo$sw)*co2_spo_if[,4] + co2_spo$sw*co2_spo_ib[,4]

# Read historical CO2 data (back to 1800).  Fill in a number for 2014 based
# on change from 2013 seen in co2_mlo.  Create an interpolated monthly version.

co2_historical <- rbind(read.csv("co2-historical.csv",head=TRUE),
                        c (2014, 397.5, NA, NA))

co2_interp <- rep (NA, 12*(2014-1800+1))
for (i in 1:nrow(co2_historical))
  co2_interp[12*(co2_historical[i,1]-1800)+6] <- co2_historical[i,2]

co2_interp[1] <- co2_interp[6]
co2_interp[length(co2_interp)] <- co2_interp[length(co2_interp)-6]

co2_interp <- zoo::na.approx(co2_interp)  # linear interpolation


# Combine some data from January 1880 to December 2014 into one data frame, H.
# Interplate CO2 data when necessary.

ybgn <- 1880
yend <- 2014

H <- list()

H$year <- rep(ybgn:yend,each=12)
H$month <- rep(1:12,times=yend-ybgn+1)
H$date <- H$year + (H$month-0.5)/12

w <- as.character(ybgn:yend)
H$GISS_GLB <- GISS_as_vector(GISS_GLB[w,])
H$GISS_NH  <- GISS_as_vector(GISS_NH[w,])
H$GISS_SH  <- GISS_as_vector(GISS_SH[w,])
H$GISS_NH_Land  <- GISS_as_vector(GISS_NH_Land[w,])
H$GISS_SH_Land  <- GISS_as_vector(GISS_SH_Land[w,])
H$GISS_GLB_old <- GISS_as_vector(GISS_GLB_old[w,])
# H$GISS_NH_old  <- GISS_as_vector(GISS_NH_old[w,])
# H$GISS_SH_old  <- GISS_as_vector(GISS_SH_old[w,])
H$GISS_GLB_old2 <- GISS_as_vector(rbind(GISS_GLB_old2,`2015`=NA)[w,])
H$GISS_GLB_old3 <- GISS_as_vector(rbind(GISS_GLB_old3,`2015`=NA)[w,])

H$CO2 <- co2_interp [(12*(ybgn-1800)+1) : (12*(yend-1800)+12)]

H <- data.frame(H)


# Combine some data from January 1959 to December 2014 into one data frame, D.

ybgn <- 1959
yend <- 2014

D <- list()

D$year <- rep(ybgn:yend,each=12)
D$month <- rep(1:12,times=yend-ybgn+1)
D$date <- D$year + (D$month-0.5)/12

D$sinm  <- sin(2*pi*D$month/12)
D$cosm  <- cos(2*pi*D$month/12)
D$sinm2 <- sin(2*2*pi*D$month/12)
D$cosm2 <- cos(2*2*pi*D$month/12)
D$sinm3 <- sin(3*2*pi*D$month/12)
D$cosm3 <- cos(3*2*pi*D$month/12)

w <- as.character(ybgn:yend)
D$GISS_GLB <- GISS_as_vector(GISS_GLB[w,])
D$GISS_NH <- GISS_as_vector(GISS_NH[w,])
D$GISS_SH <- GISS_as_vector(GISS_SH[w,])
D$GISS_GLB_old <- GISS_as_vector(GISS_GLB_old[w,])
# D$GISS_NH_old  <- GISS_as_vector(GISS_NH_old[w,])
# D$GISS_SH_old  <- GISS_as_vector(GISS_SH_old[w,])

w <- MEI$YEAR>=ybgn & MEI$YEAR<=yend
D$MEI <- as.vector(t(as.matrix(MEI[w,-1])))
w <- MEI$YEAR>=ybgn-1 & MEI$YEAR<=yend
D$MEI_MA <- as.vector(t(as.matrix(MEI[w,-1])))
D$MEI_MA <- filter(D$MEI_MA,rep(1/12,12))
D$MEI_MA <- D$MEI_MA[!is.na(D$MEI_MA)][-1]

w <- AMO$YEAR>=ybgn & AMO$YEAR<=yend
D$AMO <- as.vector(t(as.matrix(AMO[w,-1])))
w <- AMO$YEAR>=ybgn-4 & AMO$YEAR<=yend
D$AMO_MA <- as.vector(t(as.matrix(AMO[w,-1])))
D$AMO_MA <- filter(D$AMO_MA,rep(1/48,48))
D$AMO_MA <- D$AMO_MA[!is.na(D$AMO_MA)][-1]

w <- TRANS[,2]>=ybgn & TRANS[,2]<yend+1
D$TRANS <- TRANS[w,3]
w <- TRANS[,2]>=ybgn-1 & TRANS[,2]<yend+1
D$TRANS_MA <- TRANS[w,3]
D$TRANS_MA <- filter(D$TRANS_MA,rep(1/12,12))
D$TRANS_MA <- D$TRANS_MA[!is.na(D$TRANS_MA)][-1]
w <- TRANS[,2]>=ybgn-2 & TRANS[,2]<yend+1
D$TRANS_MA2 <- TRANS[w,3]
D$TRANS_MA2 <- filter(D$TRANS_MA2,c(rep(0,12),rep(1/12,12)))
D$TRANS_MA2 <- c(D$TRANS_MA[1:11],D$TRANS_MA2[!is.na(D$TRANS_MA2)])

w <- TSI[,1]>=ybgn-1 & TSI[,1]<yend+1
D$TSI <- rep(TSI[w,2],each=12)
D$TSI <- filter(D$TSI,rep(1/12,12))
D$TSI <- D$TSI[!is.na(D$TSI)][-1]

w <- SN[,1]>=ybgn & SN[,1]<=yend
D$SN <- SN[w,4]
w <- SN[,1]>=ybgn-1 & SN[,1]<=yend
D$SN_MA <- SN[w,4]
D$SN_MA <- filter(D$SN_MA,rep(1/12,12))
D$SN_MA <- D$SN_MA[!is.na(D$SN_MA)][-1]
w <- SN[,1]>=ybgn-3 & SN[,1]<yend+1
D$SN_MA2 <- SN[w,4]
D$SN_MA2 <- filter(D$SN_MA2,c(rep(0,12),rep(1/24,24)))
D$SN_MA2 <- D$SN_MA2[!is.na(D$SN_MA2)][-1]

D <- data.frame(D)


# Combine all data from January 1979 to December 2014 into one data frame, d.

ybgn <- 1979
yend <- 2014

d <- list()

d$year <- rep(ybgn:yend,each=12)
d$month <- rep(1:12,times=yend-ybgn+1)
d$date <- d$year + (d$month-0.5)/12

d$sinm  <- sin(2*pi*d$month/12)
d$cosm  <- cos(2*pi*d$month/12)
d$sinm2 <- sin(2*2*pi*d$month/12)
d$cosm2 <- cos(2*2*pi*d$month/12)
d$sinm3 <- sin(3*2*pi*d$month/12)
d$cosm3 <- cos(3*2*pi*d$month/12)

w <- UAH$Year>=ybgn & UAH$Year<=yend

d$UAH_GLB <- UAH$Globe[w]
d$UAH_NH <- UAH$NH[w]
d$UAH_SH <- UAH$SH[w]
d$UAH_Trpcs <- UAH$Trpcs[w]
d$UAH_NH_Ocean <- UAH$Ocean.1[w]
d$UAH_SH_Ocean <- UAH$Ocean.2[w]
d$UAH_NH_Land <- UAH$Land.1[w]
d$UAH_SH_Land <- UAH$Land.2[w]
d$UAH_Trpcs_Ocean <- UAH$Ocean.3[w]
d$UAH_NoPol <- UAH$NoPol[w]
d$UAH_SoPol <- UAH$SoPol[w]

w <- as.character(ybgn:yend)

d$GISS_GLB <- GISS_as_vector(GISS_GLB[w,])
d$GISS_NH <- GISS_as_vector(GISS_NH[w,])
d$GISS_SH <- GISS_as_vector(GISS_SH[w,])
d$GISS_GLB_Land <- GISS_as_vector(GISS_GLB_Land[w,])
d$GISS_NH_Land <- GISS_as_vector(GISS_NH_Land[w,])
d$GISS_SH_Land <- GISS_as_vector(GISS_SH_Land[w,])
d$GISS_GLB_old <- GISS_as_vector(GISS_GLB_old[w,])
d$GISS_GLB_old2 <- GISS_as_vector(GISS_GLB_old2[w,])
d$GISS_GLB_old3 <- GISS_as_vector(GISS_GLB_old3[w,])
# d$GISS_NH_old  <- GISS_as_vector(GISS_NH_old[w,])
# d$GISS_SH_old  <- GISS_as_vector(GISS_SH_old[w,])
d$GISS_GLB_Land_old <- GISS_as_vector(GISS_GLB_Land_old[w,])
d$GISS_NH_Land_old <- GISS_as_vector(GISS_NH_Land_old[w,])
d$GISS_SH_Land_old <- GISS_as_vector(GISS_SH_Land_old[w,])

w <- MEI$YEAR>=ybgn & MEI$YEAR<=yend
d$MEI <- as.vector(t(as.matrix(MEI[w,-1])))
w <- MEI$YEAR>=ybgn-1 & MEI$YEAR<=yend
d$MEI_MA <- as.vector(t(as.matrix(MEI[w,-1])))
d$MEI_MA <- filter(d$MEI_MA,rep(1/12,12))
d$MEI_MA <- d$MEI_MA[!is.na(d$MEI_MA)][-1]

w <- AMO$YEAR>=ybgn & AMO$YEAR<=yend
d$AMO <- as.vector(t(as.matrix(AMO[w,-1])))
w <- AMO$YEAR>=ybgn-4 & AMO$YEAR<=yend
d$AMO_MA <- as.vector(t(as.matrix(AMO[w,-1])))
d$AMO_MA <- filter(d$AMO_MA,rep(1/48,48))
d$AMO_MA <- d$AMO_MA[!is.na(d$AMO_MA)][-1]

w <- TRANS[,2]>=ybgn & TRANS[,2]<yend+1
d$TRANS <- TRANS[w,3]
w <- TRANS[,2]>=ybgn-1 & TRANS[,2]<yend+1
d$TRANS_MA <- TRANS[w,3]
d$TRANS_MA <- filter(d$TRANS_MA,rep(1/12,12))
d$TRANS_MA <- d$TRANS_MA[!is.na(d$TRANS_MA)][-1]
w <- TRANS[,2]>=ybgn-2 & TRANS[,2]<yend+1
d$TRANS_MA2 <- TRANS[w,3]
d$TRANS_MA2 <- filter(d$TRANS_MA2,c(rep(0,12),rep(1/12,12)))
d$TRANS_MA2 <- d$TRANS_MA2[!is.na(d$TRANS_MA2)][-1]

w <- TSI[,1]>=ybgn-1 & TSI[,1]<yend+1
d$TSI <- rep(TSI[w,2],each=12)
d$TSI <- filter(d$TSI,rep(1/12,12))
d$TSI <- d$TSI[!is.na(d$TSI)][-1]

w <- SN[,1]>=ybgn & SN[,1]<=yend
d$SN <- SN[w,4]
w <- SN[,1]>=ybgn-1 & SN[,1]<=yend
d$SN_MA <- SN[w,4]
d$SN_MA <- filter(d$SN_MA,rep(1/12,12))
d$SN_MA <- d$SN_MA[!is.na(d$SN_MA)][-1]
w <- SN[,1]>=ybgn-3 & SN[,1]<yend+1
d$SN_MA2 <- SN[w,4]
d$SN_MA2 <- filter(d$SN_MA2,c(rep(0,12),rep(1/24,24)))
d$SN_MA2 <- d$SN_MA2[!is.na(d$SN_MA2)][-1]

d <- data.frame(d)
