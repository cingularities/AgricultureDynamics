gc()
removeTmpFiles()
rm(list=ls())


library(raster)
library(rgdal)
library(ggplot2)
library(maptools)

setwd('U:/Land Cover Change/Senior Water Rights/FANTA')
#Load Reference Classification

#Phoenix Fanta
phxnlcd2001ac<- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2001NLCD"), full.names = TRUE, pattern = ".tif$")
phxnlcd2006ac<- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2006NLCD"), full.names = TRUE, pattern = ".tif$")
phxnlcd2011ac<- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2011NLCD"), full.names = TRUE, pattern = ".tif$")

phxstack2001ac <- stack(phxnlcd2001ac)
phxstack2006ac <- stack(phxnlcd2006ac)
phxstack2011ac <- stack(phxnlcd2011ac)


#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2011
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX

fantaextent <- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2001NLCD/phx_2001_SD3_1_26_19.tif")


#Extent and CRS
croplandFANTAPHX01AC <- projectRaster(phxstack2001ac, NLCD2001agPHX)
croplandFANTAPHX06AC <- projectRaster(phxstack2006ac, NLCD2006agPHX)
croplandFANTAPHX11AC <- projectRaster(phxstack2011ac, NLCD2011agPHX)

#crop
croplandFANTAPHX01AC <- crop(croplandFANTAPHX01AC, NLCD2001agPHX)
croplandFANTAPHX06AC <- crop(croplandFANTAPHX06AC, NLCD2006agPHX)
croplandFANTAPHX11AC <- crop(croplandFANTAPHX11AC, NLCD2011agPHX)

plot(croplandFANTAPHX01AC)

#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#mask
croplandFANTAPHX01AC <- mask(croplandFANTAPHX01AC, NLCD2001agPHX)
croplandFANTAPHX06AC <- mask(croplandFANTAPHX06AC, NLCD2006agPHX)
croplandFANTAPHX11AC <- mask(croplandFANTAPHX11AC, NLCD2011agPHX)


agextentNLCD01PHX <- cellStats(croplandFANTAPHX01AC, sum)
agextentNLCD06PHX <- cellStats(croplandFANTAPHX06AC, sum)
agextentNLCD11PHX <- cellStats(croplandFANTAPHX11AC, sum)
write.csv(agextentNLCD01PHX, file = "agextentNLCD01PHX")
write.csv(agextentNLCD06PHX, file = "agextentNLCD06PHX")
write.csv(agextentNLCD11PHX, file = "agextentNLCD11PHX")



croplandFANTAPHX01FA[croplandFANTAPHX01FA != 1] = NA
croplandFANTAPHX01Fallow <- croplandFANTAPHX01FA
plot(croplandFANTAPHX01Fallow)
phxfallow01 <- cellStats(croplandFANTAPHX01Fallow, sum)
dev.off()
plot(phxfallow01)

croplandFANTAPHX06FA[croplandFANTAPHX06FA != 1] = NA
croplandFANTAPHX06Fallow <- croplandFANTAPHX06FA
phxfallow06 <- cellStats(croplandFANTAPHX06Fallow, sum)
plot(phxfallow06)
plot(croplandFANTAPHX06Fallow)

croplandFANTAPHX11FA[croplandFANTAPHX11FA != 1] = NA
croplandFANTAPHX11Fallow <- croplandFANTAPHX11FA
phxfallow11 <- cellStats(croplandFANTAPHX11Fallow, sum)
plot(phxfallow11)
plot(croplandFANTAPHX11Fallow)

write.csv(phxfallow01, file = "phxfallow01update926")
write.csv(phxfallow06, file = "phxfallow06update926")
write.csv(phxfallow11, file = "phxfallow11update926")



croplandFANTAPHX01AC[croplandFANTAPHX01AC != 2] = NA
croplandFANTAPHX01Active <- croplandFANTAPHX01AC
PHXActive01 <- cellStats(croplandFANTAPHX01Active, sum)
plot(PHXActive01)

croplandFANTAPHX06AC[croplandFANTAPHX06AC != 2] = NA
croplandFANTAPHX06Active <- croplandFANTAPHX06AC
PHXActive06 <- cellStats(croplandFANTAPHX06Active, sum)
plot(PHXActive06)
plot(croplandFANTAPHX06Active)

croplandFANTAPHX11AC[croplandFANTAPHX11AC != 2] = NA
croplandFANTAPHX11Active <- croplandFANTAPHX11AC
PHXActive11 <- cellStats(croplandFANTAPHX11Active, sum)
plot(PHXActive11)
plot(croplandFANTAPHX11Active)

write.csv(PHXActive01, file = "PHXActive01update926")
write.csv(PHXActive06, file = "PHXActive06update926")
write.csv(PHXActive11, file = "PHXActive11update926")


#Yuma Fanta
yumanlcd2001ac<- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019/fanta_yuma_2001NLCD"), full.names = TRUE, pattern = ".tif$")
yumanlcd2006ac<- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019/fanta_yuma_2006NLCD"), full.names = TRUE, pattern = ".tif$")
yumanlcd2011ac<- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019/fanta_yuma_2011NLCD"), full.names = TRUE, pattern = ".tif$")

yumastack2001ac <- stack(yumanlcd2001ac)
yumastack2006ac <- stack(yumanlcd2006ac)
yumastack2011ac <- stack(yumanlcd2011ac)


#for crop
yumaforcrop <-raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019/fanta_yuma_2001NLCD/yuma_2001_SD3_1_26_19.tif")


#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2011
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA


#projectRaster
croplandFANTAYUMA01AC <- projectRaster(yumastack2001ac, NLCD2001agYUMA)
croplandFANTAYUMA06AC <- projectRaster(yumastack2006ac, NLCD2006agYUMA)
croplandFANTAYUMA11AC <- projectRaster(yumastack2011ac, NLCD2011agYUMA)



#crop
croplandFANTAYUMA01AC <- crop(croplandFANTAYUMA01AC, NLCD2001agYUMA)
croplandFANTAYUMA06AC <- crop(croplandFANTAYUMA06AC, NLCD2006agYUMA)
croplandFANTAYUMA11AC <- crop(croplandFANTAYUMA11AC, NLCD2011agYUMA)

#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)

NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)




#mask
croplandFANTAYUMA01AC <- mask(croplandFANTAYUMA01AC, NLCD2001agYUMA)
croplandFANTAYUMA06AC <- mask(croplandFANTAYUMA06AC, NLCD2006agYUMA)
croplandFANTAYUMA11AC <- mask(croplandFANTAYUMA11AC, NLCD2011agYUMA)


agextentNLCD01YUMA <- cellStats(croplandFANTAYUMA01AC, sum)
agextentNLCD06YUMA <- cellStats(croplandFANTAYUMA06AC, sum)
agextentNLCD11YUMA <- cellStats(croplandFANTAYUMA11AC, sum)
write.csv(agextentNLCD01YUMA, file = "agextentNLCD01YUMA")
write.csv(agextentNLCD06YUMA, file = "agextentNLCD06YUMA")
write.csv(agextentNLCD11YUMA, file = "agextentNLCD11YUMA")

croplandFANTAYUMA01FA[croplandFANTAYUMA01FA != 1] = NA
croplandFANTAYUMA01Fallow <- croplandFANTAYUMA01FA
yumafallow01 <- cellStats(croplandFANTAYUMA01Fallow, sum)
plot(yumafallow01)

croplandFANTAYUMA06FA[croplandFANTAYUMA06FA != 1] = NA
croplandFANTAYUMA06Fallow <- croplandFANTAYUMA06FA
yumafallow06 <- cellStats(croplandFANTAYUMA06Fallow, sum)
plot(yumafallow06)
plot(croplandFANTAYUMA06Fallow)

croplandFANTAYUMA11FA[croplandFANTAYUMA11FA != 1] = NA
croplandFANTAYUMA11Fallow <- croplandFANTAYUMA11FA
yumafallow11 <- cellStats(croplandFANTAYUMA11Fallow, sum)
plot(yumafallow11)
plot(croplandFANTAYUMA11Fallow)

write.csv(yumafallow01, file = "yumafallow01update926")
write.csv(yumafallow06, file = "yumafallow06update926")
write.csv(yumafallow11, file = "yumafallow11update926")


croplandFANTAYUMA01AC[croplandFANTAYUMA01AC != 2] = NA
croplandFANTAYUMA01Active <- croplandFANTAYUMA01AC
yumaActive01 <- cellStats(croplandFANTAYUMA01Active, sum)
plot(yumaActive01)

croplandFANTAYUMA06AC[croplandFANTAYUMA06AC != 2] = NA
croplandFANTAYUMA06Active <- croplandFANTAYUMA06AC
yumaActive06 <- cellStats(croplandFANTAYUMA06Active, sum)
plot(yumaActive06)
plot(croplandFANTAYUMA06Active)

croplandFANTAYUMA11AC[croplandFANTAYUMA11AC != 2] = NA
croplandFANTAYUMA11Active <- croplandFANTAYUMA11AC
yumaActive11 <- cellStats(croplandFANTAYUMA11Active, sum)
plot(yumaActive11)
plot(croplandFANTAYUMA11Active)

write.csv(yumaActive01, file = "yumaActive01update926")
write.csv(yumaActive06, file = "yumaActive06update926")
write.csv(yumaActive11, file = "yumaActive11update926")

#MERGE

multMerge = function(mypath){
  filenames = list.files(path = mypath, full.names = TRUE)
  datalist = lapply(filenames, 
                    function(x){read.csv(file = x,
                                         header = TRUE,
                                         stringsAsFactors = FALSE)})
  Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)
}



phx2001_2017fallowarea <- multMerge("U:/Land Cover Change/Senior Water Rights/FANTA/AREA/phxfallow")
yuma2001_2017fallowarea <- multMerge("U:/Land Cover Change/Senior Water Rights/FANTA/AREA/yumafallow")

par(mfrow=c(2,1))
plot.default(phx2001_2016fallowarea$x, main = "PHOENIX", ylab= "Area Extent (Pixel Count",xlab="",  type= "h", col = "blue", lwd = 2,xaxt='n')
lines(yuma2001_2016fallowarea$x,type= "h", col = 'green', lwd =2,xaxt='n', ylab= "Area Extent (Pixel Count)",main = "YUMA", xlab="16 Day Interval (2001-2016)")


write.csv(phx2001_2017fallowarea, file = "phx2001_2017fallowareaupdate926")
write.csv(yuma2001_2017fallowarea, file = "yuma2001_2017fallowareaupdate926")



phx2001_2017activearea <- multMerge("U:/Land Cover Change/Senior Water Rights/FANTA/AREA/phxactive")
yuma2001_2017activearea <- multMerge("U:/Land Cover Change/Senior Water Rights/FANTA/AREA/yumaactive")


par(mfrow=c(2,1))
plot.default(phx2001_2016activearea$x, main = "PHOENIX", ylab= "Area Extent (Pixel Count",xlab="",  type= "h", col = "blue", lwd = 2,xaxt='n')
plot(yuma2001_2016activearea$x,type= "h", col = 'green', lwd =2,xaxt='n', ylab= "Area Extent (Pixel Count)",main = "YUMA", xlab="16 Day Interval (2001-2016)")


write.csv(phx2001_2017activearea, file = "phx2001_2017activeareaupdate926")
write.csv(yuma2001_2017activearea, file = "yuma2001_2017activeareaupdate926")





##GRAPHS
#Area
phxactive <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/AreaExtent/PHX_mha_percent.csv")
yumaactive <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/AreaExtent/YUMA_mha_percent.csv")


#Climate Variables
phxclimate <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/PRISM/PHX_yearly_climate.csv")
yumaclimate <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/PRISM/YUMA_yearly_climate.csv")


par(mfrow=c(2,1))
plot.default(phxactive$percent.active, main = "PHOENIX", ylab= "Area Extent %",xlab="16 Day Interval (2001-2017)",  type= "l", col = "blue", lwd = 3, ylim = c(0.75, 0.92))
lines(yumaactive$percent.active,type= "l", col = 'green', lwd =3, ylab= "Area Extent %",main = "YUMA", xlab="16 Day Interval (2001-2017)")


#Yuma
##Same Plots
par(mfrow=c(2,2))
par(oma=c(0,2,2,0))
#precipitation
plot(yumaactive$percent.active, yumaclimate$ppt, main = "", xlab="", ylab= "Precipitation Average (mm)",  col="blue", cex.lab = 1.5, cex.main=1.5, bty ="o", pch=16, yaxt = "none")
abline(lm(yumaclimate$ppt ~ yumaactive$percent.active), lwd=3, lty=2)
axis(2,las=2, font=4)
axis(1, font=3)
legend('bottomleft', legend=c("Abline"),col=c("black"), lty=2, cex=0.8, box.lty=0)


mod1 = lm(yumaclimate$ppt ~ yumaactive$percent.active)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)



#Temperature Avg Avg
plot(yumaactive$percent.active, yumaclimate$tavg, main = "", xlab="", ylab= "Average Yearly Temp",  col="blue", cex.lab = 1.5, cex.main=1.5, bty ="o", pch=16, yaxt = "none")
abline(lm(yumaclimate$tavg ~ yumaactive$percent.active), lwd=3, lty=2)
axis(2,las=2, font=4)
axis(1, font=3)
legend('bottomleft', legend=c("Abline"),col=c("black"), lty=2, cex=0.8, box.lty=0)


mod1 = lm(yumaclimate$tavg ~ yumaactive$percent.active)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


#PET
plot(yumaactive$percent.active, yumaclimate$pet, main = "", xlab="% Active (mha)", ylab= "PET",  col="blue", cex.lab = 1.5, cex.main=1.5, bty ="o", pch=16, yaxt = "none")
abline(lm(yumaclimate$pet ~ yumaactive$percent.active), lwd=3, lty=2)
axis(2,las=2, font=4)
axis(1, font=3)
legend('bottomleft', legend=c("Abline"),col=c("black"), lty=2, cex=0.8, box.lty=0)


mod1 = lm(yumaclimate$pet ~ yumaactive$percent.active)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


#AI
plot(yumaactive$percent.active, yumaclimate$ai, main = "", xlab="% Active (mha)", ylab= "Aridity Index (P/PET)",  col="blue", cex.lab = 1.5, cex.main=1.5, bty ="o", pch=16, yaxt = "none")
abline(lm(yumaclimate$ai ~ yumaactive$percent.active), lwd=3, lty=2)
axis(2,las=2, font=4)
axis(1, font=3)
legend('bottomleft', legend=c("Abline"),col=c("black"), lty=2, cex=0.8, box.lty=0)


mod1 = lm(yumaclimate$ai ~ yumaactive$percent.active)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


title("Yuma Active Extent vs Climate", outer=TRUE, cex.main = 3)


#phx
##Same Plots
par(mfrow=c(2,2))
par(oma=c(0,2,2,0))
#precipitation
plot(phxactive$percent.active, phxclimate$ppt, main = "", xlab="", ylab= "Precipitation Average (mm)",  col="blue", cex.lab = 1.5, cex.main=1.5, bty ="o", pch=16, yaxt = "none")
abline(lm(phxclimate$ppt ~ phxactive$percent.active), lwd=3, lty=2)
axis(2,las=2, font=4)
axis(1, font=3)
legend('bottomleft', legend=c("Abline"),col=c("black"), lty=2, cex=0.8, box.lty=0)

mod1 = lm(phxclimate$ppt ~ phxactive$percent.active)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


#Temperature Avg Avg
plot(phxactive$percent.active, phxclimate$tavg, main = "", xlab="", ylab= "Average Yearly Temp",  col="blue", cex.lab = 1.5, cex.main=1.5, bty ="o", pch=16, yaxt = "none")
abline(lm(phxclimate$tavg ~ phxactive$percent.active), lwd=3, lty=2)
axis(2,las=2, font=4)
axis(1, font=3)
legend('bottomleft', legend=c("Abline"),col=c("black"), lty=2, cex=0.8, box.lty=0)

mod1 = lm(phxclimate$tavg ~ phxactive$percent.active)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


#PET
plot(phxactive$percent.active, phxclimate$pet, main = "", xlab="% Active (mha)", ylab= "PET",  col="blue", cex.lab = 1.5, cex.main=1.5, bty ="o", pch=16, yaxt = "none")
abline(lm(phxclimate$pet ~ phxactive$percent.active), lwd=3, lty=2)
axis(2,las=2, font=4)
axis(1, font=3)
legend('bottomleft', legend=c("Abline"),col=c("black"), lty=2, cex=0.8, box.lty=0)

mod1 = lm(phxclimate$pet ~ phxactive$percent.active)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)



#AI
plot(phxactive$percent.active, phxclimate$ai, main = "", xlab="% Active (mha)", ylab= "Aridity Index (P/PET)",  col="blue", cex.lab = 1.5, cex.main=1.5, bty ="o", pch=16, yaxt = "none")
abline(lm(phxclimate$ai ~ phxactive$percent.active), lwd=3, lty=2)
axis(2,las=2, font=4)
axis(1, font=3)
legend('bottomleft', legend=c("Abline"),col=c("black"), lty=2, cex=0.8, box.lty=0)


mod1 = lm(phxclimate$ai ~ phxactive$percent.active)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


title("Phoenix Active Extent vs Climate", outer=TRUE, cex.main = 3)

