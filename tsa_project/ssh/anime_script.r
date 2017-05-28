list.of.packages <- c("ggplot2", "dplyr", "tidyr","animation","ggvis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
library(tidyr)
library(ggplot2)
library(animation)
read_cru_hemi <- function(filename) {
    # read in whole file as table
    tab <- read.table(filename,fill=TRUE)
    nrows <- nrow(tab)
    # create frame
    hemi <- data.frame(
        year=tab[seq(1,nrows,2),1],
        annual=tab[seq(1,nrows,2),14],
        month=array(tab[seq(1,nrows,2),2:13]),
        cover=array(tab[seq(2,nrows,2),2:13])
    )
    # mask out months with 0 coverage
    hemi$month.1 [which(hemi$cover.1 ==0)] <- NA
    hemi$month.2 [which(hemi$cover.2 ==0)] <- NA
    hemi$month.3 [which(hemi$cover.3 ==0)] <- NA
    hemi$month.4 [which(hemi$cover.4 ==0)] <- NA
    hemi$month.5 [which(hemi$cover.5 ==0)] <- NA
    hemi$month.6 [which(hemi$cover.6 ==0)] <- NA
    hemi$month.7 [which(hemi$cover.7 ==0)] <- NA
    hemi$month.8 [which(hemi$cover.8 ==0)] <- NA
    hemi$month.9 [which(hemi$cover.9 ==0)] <- NA
    hemi$month.10[which(hemi$cover.10==0)] <- NA
    hemi$month.11[which(hemi$cover.11==0)] <- NA
    hemi$month.12[which(hemi$cover.12==0)] <- NA
    #
    return(hemi)
}
#Data from https://crudata.uea.ac.uk/cru/data/temperature/
#As well as data read in script

url_dat <- "https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT4-gl.dat"
temp_dat <- read_cru_hemi(url_dat)

#temp_dat <- read_cru_hemi("./HadCRUT4-gl.dat")

#remove cover
temp_dat_monthly <- temp_dat %>%
  select(-starts_with("cover")) %>%
  select(-starts_with("annual")) %>%
  gather(month, anomaly, -year) %>%
  mutate(month = gsub("month\\.", "", month)) %>%
  mutate(month = as.numeric(month)) %>%
  filter(year !=2016)

mo <- months(seq(as.Date("1910/1/1"), as.Date("1911/1/1"), "months"))
mo <- gsub("(^...).*", "\\1", mo)

saveGIF({
  
#  for(i in 1850:2015){
  for(i in 1850:2016){
    print(ggplot(temp_dat_monthly %>% filter(year <= i), 
           aes(x=month, y=anomaly, color=year, group=year)) +
        geom_line() +
          #scale_color_gradient(low="blue", high="red", limits=c(1850, 2015), guide="none") +
        scale_color_gradient(low="blue", high="red", limits=c(1850, 2016), guide="none") +
        geom_hline(yintercept=1.5, color="black", lty=2) +
        geom_hline(yintercept=2, color="black", lty=2) +
        coord_polar() +
        annotate(x=1, y=-1.5, geom="text", label=i) +
        annotate(x=1, y=1.5, geom="label", label="1.5C", fill="white", label.size=0) +
        annotate(x=1, y=2, geom="label", label="2.0C", fill="white", label.size=0) +
          ggtitle(expression(atop("Global Temperature Change 1850-2016, East Anglia's HadCRUT4-gl.dat", atop(italic("by Steven Shen"), "")))) +
    #    ggtitle("Global Temperature Change 1850-2016 using University of East Anglia's HadCRUT4-gl.dat") +
        scale_x_continuous(labels=mo, breaks=1:13) +
        scale_y_continuous(labels=NULL, breaks=NULL) +
         ylab("") + xlab("")
       
  )}
}, interval=0.1)
