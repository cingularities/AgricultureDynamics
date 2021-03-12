#Fanta Paper Figure

library(raster)
library(rgdal)
library(ggplot2)
library(maptools)


setwd('U:/Land Cover Change/Senior Water Rights/Figures/Updated_Figures')


gc()
removeTmpFiles()
rm(list=ls())

##FANTA and NLCD
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


#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

breakpoints <- c(0,1,2)
colors <- c("red","green")

#2001 AG mask, matrix, mean, extract, 
#2001
fantaPHX01 <- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/phx/fanta_phx_2001NLCD/"), full.names = TRUE, pattern = ".tif$")
fantaPHX01
fantaPHX01 <- stack(fantaPHX01, bands=1)

fantaPHX01 <-  projectRaster(fantaPHX01, NLCD2001agPHX)

fantacropPHX01 <- mask(fantaPHX01, NLCD2001agPHX)
plot(fantacropPHX01[[1]],breaks=breakpoints,col=colors)


#2006
fantaPHX06 <- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/phx/fanta_phx_2006NLCD/"), full.names = TRUE, pattern = ".tif$")
fantaPHX06
fantaPHX06 <- stack(fantaPHX06, bands=1)

fantaPHX06 <-  projectRaster(fantaPHX06, NLCD2006agPHX)

fantacropPHX06 <- mask(fantaPHX06, NLCD2006agPHX)
plot(fantacropPHX06[[1]],breaks=breakpoints,col=colors)

#2011
fantaPHX11 <- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/phx/fanta_phx_2011NLCD/"), full.names = TRUE, pattern = ".tif$")
fantaPHX11
fantaPHX11 <- stack(fantaPHX11, bands=1)

fantaPHX11 <-  projectRaster(fantaPHX11, NLCD2011agPHX)

fantacropPHX11 <- mask(fantaPHX11, NLCD2011agPHX)
plot(fantacropPHX11[[1]],breaks=breakpoints,col=colors)



#YUMA Fanta
#NLCD
#NLCD 2001
NLCD2001agyuma <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agyuma

#NLCD 2006
NLCD2006agyuma <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agyuma

#NLCD 2011
NLCD2011agyuma <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agyuma

#Selecting Field
NLCD2001agyuma[NLCD2001agyuma != 82] = NA
plot(NLCD2001agyuma)


NLCD2006agyuma[NLCD2006agyuma != 82] = NA
plot(NLCD2006agyuma)

NLCD2011agyuma[NLCD2011agyuma != 82] = NA
plot(NLCD2011agyuma)


#2001 AG mask, matrix, mean, extract, 
#2001
fantayuma01 <- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/yuma/fanta_yuma_2001NLCD/"), full.names = TRUE, pattern = ".tif$")
fantayuma01
fantayuma01 <- stack(fantayuma01, bands=1)

fantayuma01 <-  projectRaster(fantayuma01, NLCD2001agyuma)

fantacropyuma01 <- mask(fantayuma01, NLCD2001agyuma)
plot(fantacropyuma01[[1]])


#2006
fantayuma06 <- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/yuma/fanta_yuma_2006NLCD/"), full.names = TRUE, pattern = ".tif$")
fantayuma06
fantayuma06 <- stack(fantayuma06, bands=1)

fantayuma06 <-  projectRaster(fantayuma06, NLCD2006agyuma)

fantacropyuma06 <- mask(fantayuma06, NLCD2006agyuma)
plot(fantacropyuma06[[1]])

#2011
fantayuma11 <- list.files(("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/yuma/fanta_yuma_2011NLCD/"), full.names = TRUE, pattern = ".tif$")
fantayuma11
fantayuma11 <- stack(fantayuma11, bands=1)

fantayuma11 <-  projectRaster(fantayuma11, NLCD2011agyuma)

fantacropyuma11 <- mask(fantayuma11, NLCD2011agyuma)
plot(fantacropyuma11[[1]])



##Integrated NDVI
#Phoenix
breakpoints2 <- c(0,5,10,15,20)
colors2 <- c("red","green", "green",'green','green')



ndviPHX01 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2001"), full.names = TRUE, pattern = ".tif$")
ndviPHX01
ndvistackPHX01 <- stack(ndviPHX01, bands=1)
ndvisumPHX01 <- sum(ndvistackPHX01)
ndvisumPHX01 <-  projectRaster(ndvisumPHX01, NLCD2001agPHX)
ndvinlcdPHX01 <- mask(ndvisumPHX01, NLCD2001agPHX)
plot(ndvinlcdPHX01[[1]],breaks=breakpoints2,legend = FALSE,col=colors2)

ndviPHX02 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2002"), full.names = TRUE, pattern = ".tif$")
ndviPHX02
ndvistackPHX02 <- stack(ndviPHX02, bands=1)
ndvisumPHX02 <- sum(ndvistackPHX02)
ndvisumPHX02 <-  projectRaster(ndvisumPHX02, NLCD2001agPHX)
ndvinlcdPHX02 <- mask(ndvisumPHX02, NLCD2001agPHX)

ndviPHX03 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2003"), full.names = TRUE, pattern = ".tif$")
ndviPHX03
ndvistackPHX03 <- stack(ndviPHX03, bands=1)
ndvisumPHX03 <- sum(ndvistackPHX03)
ndvisumPHX03 <-  projectRaster(ndvisumPHX03, NLCD2001agPHX)
ndvinlcdPHX03 <- mask(ndvisumPHX03, NLCD2001agPHX)

ndviPHX04 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2004"), full.names = TRUE, pattern = ".tif$")
ndviPHX04
ndvistackPHX04 <- stack(ndviPHX04, bands=1)
ndvisumPHX04 <- sum(ndvistackPHX04)
ndvisumPHX04 <-  projectRaster(ndvisumPHX04, NLCD2001agPHX)
ndvinlcdPHX04 <- mask(ndvisumPHX04, NLCD2001agPHX)

ndviPHX05 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2005"), full.names = TRUE, pattern = ".tif$")
ndviPHX05
ndvistackPHX05 <- stack(ndviPHX05, bands=1)
ndvisumPHX05 <- sum(ndvistackPHX05)
ndvisumPHX05 <-  projectRaster(ndvisumPHX05, NLCD2001agPHX)
ndvinlcdPHX05 <- mask(ndvisumPHX05, NLCD2001agPHX)


ndviPHX06 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2006"), full.names = TRUE, pattern = ".tif$")
ndviPHX06
ndvistackPHX06 <- stack(ndviPHX06, bands=1)
ndvisumPHX06 <- sum(ndvistackPHX06)
ndvisumPHX06 <-  projectRaster(ndvisumPHX06, NLCD2006agPHX)
ndvinlcdPHX06 <- mask(ndvisumPHX06, NLCD2006agPHX)

ndviPHX07 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2007"), full.names = TRUE, pattern = ".tif$")
ndviPHX07
ndvistackPHX07 <- stack(ndviPHX07, bands=1)
ndvisumPHX07 <- sum(ndvistackPHX07)
ndvisumPHX07 <-  projectRaster(ndvisumPHX07, NLCD2006agPHX)
ndvinlcdPHX07 <- mask(ndvisumPHX07, NLCD2006agPHX)


ndviPHX08 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2008"), full.names = TRUE, pattern = ".tif$")
ndviPHX08
ndvistackPHX08 <- stack(ndviPHX08, bands=1)
ndvisumPHX08 <- sum(ndvistackPHX08)
ndvisumPHX08 <-  projectRaster(ndvisumPHX08, NLCD2006agPHX)
ndvinlcdPHX08 <- mask(ndvisumPHX08, NLCD2006agPHX)


ndviPHX09 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2009"), full.names = TRUE, pattern = ".tif$")
ndviPHX09
ndvistackPHX09 <- stack(ndviPHX09, bands=1)
ndvisumPHX09 <- sum(ndvistackPHX09)
ndvisumPHX09 <-  projectRaster(ndvisumPHX09, NLCD2006agPHX)
ndvinlcdPHX09 <- mask(ndvisumPHX09, NLCD2006agPHX)


ndviPHX10 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2010"), full.names = TRUE, pattern = ".tif$")
ndviPHX10
ndvistackPHX10 <- stack(ndviPHX10, bands=1)
ndvisumPHX10 <- sum(ndvistackPHX10)
ndvisumPHX10 <-  projectRaster(ndvisumPHX10, NLCD2006agPHX)
ndvinlcdPHX10 <- mask(ndvisumPHX10, NLCD2006agPHX)


ndviPHX11 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2011"), full.names = TRUE, pattern = ".tif$")
ndviPHX11
ndvistackPHX11 <- stack(ndviPHX11, bands=1)
ndvisumPHX11 <- sum(ndvistackPHX11)
ndvisumPHX11 <-  projectRaster(ndvisumPHX11, NLCD2011agPHX)
ndvinlcdPHX11 <- mask(ndvisumPHX11, NLCD2011agPHX)


ndviPHX12 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2012"), full.names = TRUE, pattern = ".tif$")
ndviPHX12
ndvistackPHX12 <- stack(ndviPHX12, bands=1)
ndvisumPHX12 <- sum(ndvistackPHX12)
ndvisumPHX12 <-  projectRaster(ndvisumPHX12,  NLCD2011agPHX)
ndvinlcdPHX12 <- mask(ndvisumPHX12,  NLCD2011agPHX)


ndviPHX13 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2013"), full.names = TRUE, pattern = ".tif$")
ndviPHX13
ndvistackPHX13 <- stack(ndviPHX13, bands=1)
ndvisumPHX13 <- sum(ndvistackPHX13)
ndvisumPHX13 <-  projectRaster(ndvisumPHX13,  NLCD2011agPHX)
ndvinlcdPHX13 <- mask(ndvisumPHX13,  NLCD2011agPHX)


ndviPHX14 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2014"), full.names = TRUE, pattern = ".tif$")
ndviPHX14
ndvistackPHX14 <- stack(ndviPHX14, bands=1)
ndvisumPHX14 <- sum(ndvistackPHX14)
ndvisumPHX14 <-  projectRaster(ndvisumPHX14,  NLCD2011agPHX)
ndvinlcdPHX14 <- mask(ndvisumPHX14,  NLCD2011agPHX)


ndviPHX15 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2015"), full.names = TRUE, pattern = ".tif$")
ndviPHX15
ndvistackPHX15 <- stack(ndviPHX15, bands=1)
ndvisumPHX15 <- sum(ndvistackPHX15)
ndvisumPHX15 <-  projectRaster(ndvisumPHX15,  NLCD2011agPHX)
ndvinlcdPHX15 <- mask(ndvisumPHX15,  NLCD2011agPHX)


ndviPHX16 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2016"), full.names = TRUE, pattern = ".tif$")
ndviPHX16
ndvistackPHX16 <- stack(ndviPHX16, bands=1)
ndvisumPHX16 <- sum(ndvistackPHX16)
ndvisumPHX16 <-  projectRaster(ndvisumPHX16,  NLCD2011agPHX)
ndvinlcdPHX16 <- mask(ndvisumPHX16,  NLCD2011agPHX)

ndviPHX17 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2017"), full.names = TRUE, pattern = ".tif$")
ndviPHX17
ndvistackPHX17 <- stack(ndviPHX17, bands=1)
ndvisumPHX17 <- sum(ndvistackPHX17)
ndvisumPHX17 <-  projectRaster(ndvisumPHX17,  NLCD2011agPHX)
ndvinlcdPHX17 <- mask(ndvisumPHX17,  NLCD2011agPHX)



#YUMA
ndviYUMA01 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2001"), full.names = TRUE, pattern = ".tif$")
ndviYUMA01
ndvistackYUMA01 <- stack(ndviYUMA01, bands=1)
ndvisumYUMA01 <- sum(ndvistackYUMA01)
ndvisumYUMA01 <-  projectRaster(ndvisumYUMA01, NLCD2001agyuma)
ndvinlcdYUMA01 <- mask(ndvisumYUMA01, NLCD2001agyuma)
plot(ndvinlcdYUMA01[[1]])

ndviYUMA02 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2002"), full.names = TRUE, pattern = ".tif$")
ndviYUMA02
ndvistackYUMA02 <- stack(ndviYUMA02, bands=1)
ndvisumYUMA02 <- sum(ndvistackYUMA02)
ndvisumYUMA02 <-  projectRaster(ndvisumYUMA02, NLCD2001agyuma)
ndvinlcdYUMA02 <- mask(ndvisumYUMA02, NLCD2001agyuma)

ndviYUMA03 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2003"), full.names = TRUE, pattern = ".tif$")
ndviYUMA03
ndvistackYUMA03 <- stack(ndviYUMA03, bands=1)
ndvisumYUMA03 <- sum(ndvistackYUMA03)
ndvisumYUMA03 <-  projectRaster(ndvisumYUMA03, NLCD2001agyuma)
ndvinlcdYUMA03 <- mask(ndvisumYUMA03, NLCD2001agyuma)

ndviYUMA04 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2004"), full.names = TRUE, pattern = ".tif$")
ndviYUMA04
ndvistackYUMA04 <- stack(ndviYUMA04, bands=1)
ndvisumYUMA04 <- sum(ndvistackYUMA04)
ndvisumYUMA04 <-  projectRaster(ndvisumYUMA04, NLCD2001agyuma)
ndvinlcdYUMA04 <- mask(ndvisumYUMA04, NLCD2001agyuma)

ndviYUMA05 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2005"), full.names = TRUE, pattern = ".tif$")
ndviYUMA05
ndvistackYUMA05 <- stack(ndviYUMA05, bands=1)
ndvisumYUMA05 <- sum(ndvistackYUMA05)
ndvisumYUMA05 <-  projectRaster(ndvisumYUMA05, NLCD2001agyuma)
ndvinlcdYUMA05 <- mask(ndvisumYUMA05, NLCD2001agyuma)


ndviYUMA06 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2006"), full.names = TRUE, pattern = ".tif$")
ndviYUMA06
ndvistackYUMA06 <- stack(ndviYUMA06, bands=1)
ndvisumYUMA06 <- sum(ndvistackYUMA06)
ndvisumYUMA06 <-  projectRaster(ndvisumYUMA06, NLCD2006agyuma)
ndvinlcdYUMA06 <- mask(ndvisumYUMA06, NLCD2006agyuma)

ndviYUMA07 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2007"), full.names = TRUE, pattern = ".tif$")
ndviYUMA07
ndvistackYUMA07 <- stack(ndviYUMA07, bands=1)
ndvisumYUMA07 <- sum(ndvistackYUMA07)
ndvisumYUMA07 <-  projectRaster(ndvisumYUMA07, NLCD2006agyuma)
ndvinlcdYUMA07 <- mask(ndvisumYUMA07, NLCD2006agyuma)


ndviYUMA08 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2008"), full.names = TRUE, pattern = ".tif$")
ndviYUMA08
ndvistackYUMA08 <- stack(ndviYUMA08, bands=1)
ndvisumYUMA08 <- sum(ndvistackYUMA08)
ndvisumYUMA08 <-  projectRaster(ndvisumYUMA08, NLCD2006agyuma)
ndvinlcdYUMA08 <- mask(ndvisumYUMA08, NLCD2006agyuma)


ndviYUMA09 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2009"), full.names = TRUE, pattern = ".tif$")
ndviYUMA09
ndvistackYUMA09 <- stack(ndviYUMA09, bands=1)
ndvisumYUMA09 <- sum(ndvistackYUMA09)
ndvisumYUMA09 <-  projectRaster(ndvisumYUMA09, NLCD2006agyuma)
ndvinlcdYUMA09 <- mask(ndvisumYUMA09, NLCD2006agyuma)


ndviYUMA10 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2010"), full.names = TRUE, pattern = ".tif$")
ndviYUMA10
ndvistackYUMA10 <- stack(ndviYUMA10, bands=1)
ndvisumYUMA10 <- sum(ndvistackYUMA10)
ndvisumYUMA10 <-  projectRaster(ndvisumYUMA10, NLCD2006agyuma)
ndvinlcdYUMA10 <- mask(ndvisumYUMA10, NLCD2006agyuma)


ndviYUMA11 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2011"), full.names = TRUE, pattern = ".tif$")
ndviYUMA11
ndvistackYUMA11 <- stack(ndviYUMA11, bands=1)
ndvisumYUMA11 <- sum(ndvistackYUMA11)
ndvisumYUMA11 <-  projectRaster(ndvisumYUMA11, NLCD2011agyuma)
ndvinlcdYUMA11 <- mask(ndvisumYUMA11, NLCD2011agyuma)


ndviYUMA12 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2012"), full.names = TRUE, pattern = ".tif$")
ndviYUMA12
ndvistackYUMA12 <- stack(ndviYUMA12, bands=1)
ndvisumYUMA12 <- sum(ndvistackYUMA12)
ndvisumYUMA12 <-  projectRaster(ndvisumYUMA12,  NLCD2011agyuma)
ndvinlcdYUMA12 <- mask(ndvisumYUMA12,  NLCD2011agyuma)


ndviYUMA13 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2013"), full.names = TRUE, pattern = ".tif$")
ndviYUMA13
ndvistackYUMA13 <- stack(ndviYUMA13, bands=1)
ndvisumYUMA13 <- sum(ndvistackYUMA13)
ndvisumYUMA13 <-  projectRaster(ndvisumYUMA13,  NLCD2011agyuma)
ndvinlcdYUMA13 <- mask(ndvisumYUMA13,  NLCD2011agyuma)


ndviYUMA14 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2014"), full.names = TRUE, pattern = ".tif$")
ndviYUMA14
ndvistackYUMA14 <- stack(ndviYUMA14, bands=1)
ndvisumYUMA14 <- sum(ndvistackYUMA14)
ndvisumYUMA14 <-  projectRaster(ndvisumYUMA14,  NLCD2011agyuma)
ndvinlcdYUMA14 <- mask(ndvisumYUMA14,  NLCD2011agyuma)


ndviYUMA15 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2015"), full.names = TRUE, pattern = ".tif$")
ndviYUMA15
ndvistackYUMA15 <- stack(ndviYUMA15, bands=1)
ndvisumYUMA15 <- sum(ndvistackYUMA15)
ndvisumYUMA15 <-  projectRaster(ndvisumYUMA15,  NLCD2011agyuma)
ndvinlcdYUMA15 <- mask(ndvisumYUMA15,  NLCD2011agyuma)


ndviYUMA16 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2016"), full.names = TRUE, pattern = ".tif$")
ndviYUMA16
ndvistackYUMA16 <- stack(ndviYUMA16, bands=1)
ndvisumYUMA16 <- sum(ndvistackYUMA16)
ndvisumYUMA16 <-  projectRaster(ndvisumYUMA16,  NLCD2011agyuma)
ndvinlcdYUMA16 <- mask(ndvisumYUMA16,  NLCD2011agyuma)



ndviYUMA17 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2017"), full.names = TRUE, pattern = ".tif$")
ndviYUMA17
ndvistackYUMA17 <- stack(ndviYUMA17, bands=1)
ndvisumYUMA17 <- sum(ndvistackYUMA17)
ndvisumYUMA17 <-  projectRaster(ndvisumYUMA17,  NLCD2011agyuma)
ndvinlcdYUMA17 <- mask(ndvisumYUMA17,  NLCD2011agyuma)


###PLOT
#PHX
tiff("PHX2001.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 1, 0.2, 0.1))
plot(NLCD2001agPHX, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX01[[1]],legend = FALSE, breaks=breakpoints, col=colors, main = "2001 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX01, main = "2001 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3,  cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2002.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2001agPHX, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX01[[2]],legend = FALSE, breaks=breakpoints, col=colors, main = "2002 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX02, main = "2002 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2003.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2001agPHX, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX01[[3]],legend = FALSE, breaks=breakpoints, col=colors, main = "2003 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX03, main = "2003 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2004.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2001agPHX, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX01[[4]],legend = FALSE, breaks=breakpoints, col=colors, main = "2004 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX04, main = "2004 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2005.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2001agPHX, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX01[[5]],legend = FALSE, breaks=breakpoints, col=colors, main = "2005 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX05, main = "2005 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2006.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agPHX, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX06[[1]],legend = FALSE, breaks=breakpoints, col=colors, main = "2006 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX06, main = "2006 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2007.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agPHX, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX06[[2]],legend = FALSE, breaks=breakpoints, col=colors, main = "2007 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX07, main = "2007 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2008.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agPHX, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX06[[3]],legend = FALSE, breaks=breakpoints, col=colors, main = "2008 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX08, main = "2008 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2009.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agPHX, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX06[[4]],legend = FALSE, breaks=breakpoints, col=colors, main = "2009 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX09, main = "2009 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2010.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agPHX, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX06[[5]],legend = FALSE, breaks=breakpoints, col=colors, main = "2010 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX10, main = "2010 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2011.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agPHX, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX11[[1]],legend = FALSE, breaks=breakpoints, col=colors, main = "2011 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX11, main = "2011 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2012.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agPHX, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX11[[2]],legend = FALSE, breaks=breakpoints, col=colors, main = "2012 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX12, main = "2012 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2013.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agPHX, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX11[[3]],legend = FALSE, breaks=breakpoints, col=colors, main = "2013 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX13, main = "2013 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2014.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agPHX, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX11[[4]],legend = FALSE, breaks=breakpoints, col=colors, main = "2014 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX14, main = "2014 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2015.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agPHX, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX11[[5]],legend = FALSE, breaks=breakpoints, col=colors, main = "2015 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX15, main = "2015 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2016.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agPHX, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX11[[6]],legend = FALSE, breaks=breakpoints, col=colors, main = "2016 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX16, main = "2016 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("PHX2017.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agPHX, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropPHX11[[7]],legend = FALSE, breaks=breakpoints, col=colors, main = "2017 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdPHX17, main = "2017 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()


#YUMA
tiff("YUMA2001.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2001agyuma, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma01[[1]],legend = FALSE, breaks=breakpoints, col=colors, main = "2001 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA01, main = "2001 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2002.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2001agyuma, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma01[[2]],legend = FALSE, breaks=breakpoints, col=colors, main = "2002 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA02, main = "2002 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2003.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2001agyuma, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma01[[3]],legend = FALSE, breaks=breakpoints, col=colors, main = "2003 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA03, main = "2003 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2004.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2001agyuma, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma01[[4]],legend = FALSE, breaks=breakpoints, col=colors, main = "2004 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA04, main = "2004 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2005.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2001agyuma, legend = FALSE, col="orange", main = "2001 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma01[[5]],legend = FALSE, breaks=breakpoints, col=colors, main = "2005 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA05, main = "2005 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2006.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agyuma, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma06[[1]],legend = FALSE, breaks=breakpoints, col=colors, main = "2006 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA06, main = "2006 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2007.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agyuma, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma06[[2]],legend = FALSE, breaks=breakpoints, col=colors, main = "2007 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA07, main = "2007 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2008.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agyuma, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma06[[3]],legend = FALSE, breaks=breakpoints, col=colors, main = "2008 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA08, main = "2008 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2009.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agyuma, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma06[[4]],legend = FALSE, breaks=breakpoints, col=colors, main = "2009 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA09, main = "2009 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2010.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2006agyuma, legend = FALSE, col="orange", main = "2006 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma06[[5]],legend = FALSE, breaks=breakpoints, col=colors, main = "2010 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA10, main = "2010 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2011.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agyuma, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma11[[1]],legend = FALSE, breaks=breakpoints, col=colors, main = "2011 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA11, main = "2011 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2012.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agyuma, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma11[[2]],legend = FALSE, breaks=breakpoints, col=colors, main = "2012 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA12, main = "2012 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2013.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agyuma, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma11[[3]],legend = FALSE, breaks=breakpoints, col=colors, main = "2013 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA13, main = "2013 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2014.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agyuma, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma11[[4]],legend = FALSE, breaks=breakpoints, col=colors, main = "2014 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA14, main = "2014 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2015.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agyuma, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma11[[5]],legend = FALSE, breaks=breakpoints, col=colors, main = "2015 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA15, main = "2015 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2016.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agyuma, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma11[[6]],legend = FALSE, breaks=breakpoints, col=colors, main = "2016 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA16, main = "2016 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

tiff("YUMA2017.jpg",  res=2000, compression = "lzw", height=10, width=5, units="in")
par(mfrow=c(3,1), mai = c(0.1, 01, 0.2, 0.1))
plot(NLCD2011agyuma, legend = FALSE, col="orange", main = "2011 NLCD", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Agriculture Extent"), fill = "orange",cex=1.3, pt.cex = 1,box.lty = 0)
plot(fantacropyuma11[[7]],legend = FALSE, breaks=breakpoints, col=colors, main = "2017 FANTA", yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright", legend = c("Fallow", "Active"), fill = colors, cex=1.3, pt.cex = 1,box.lty = 0)
plot(ndvinlcdYUMA17, main = "2017 NDVI",breaks=breakpoints2,legend = FALSE, col=colors2, yaxt = "none", xaxt ="none",cex=1.3, cex.main=1.5)
legend("topright",legend = c("NDVI < 5", "NDVI > 5") , fill = colors2, cex=1.3, pt.cex = 1,box.lty = 0)
dev.off()

#Write Raster
writeRaster(fantacropPHX01[[1]], "phxfanta01", format = "GTiff")
writeRaster(fantacropPHX01[[2]], "phxfanta02", format = "GTiff")
writeRaster(fantacropPHX01[[3]], "phxfanta03", format = "GTiff")
writeRaster(fantacropPHX01[[4]], "phxfanta04", format = "GTiff")
writeRaster(fantacropPHX01[[5]], "phxfanta05", format = "GTiff")
writeRaster(fantacropPHX06[[1]], "phxfanta06", format = "GTiff")
writeRaster(fantacropPHX06[[2]], "phxfanta07", format = "GTiff")
writeRaster(fantacropPHX06[[3]], "phxfanta08", format = "GTiff")
writeRaster(fantacropPHX06[[4]], "phxfanta09", format = "GTiff")
writeRaster(fantacropPHX06[[5]], "phxfanta10", format = "GTiff")
writeRaster(fantacropPHX11[[1]], "phxfanta11", format = "GTiff")
writeRaster(fantacropPHX11[[2]], "phxfanta12", format = "GTiff")
writeRaster(fantacropPHX11[[3]], "phxfanta13", format = "GTiff")
writeRaster(fantacropPHX11[[4]], "phxfanta14", format = "GTiff")
writeRaster(fantacropPHX11[[5]], "phxfanta15", format = "GTiff")
writeRaster(fantacropPHX11[[6]], "phxfanta16", format = "GTiff")
writeRaster(fantacropPHX11[[7]], "phxfanta17", format = "GTiff")


writeRaster(fantacropyuma01[[1]], "yumafanta01", format = "GTiff")
writeRaster(fantacropyuma01[[2]], "yumafanta02", format = "GTiff")
writeRaster(fantacropyuma01[[3]], "yumafanta03", format = "GTiff")
writeRaster(fantacropyuma01[[4]], "yumafanta04", format = "GTiff")
writeRaster(fantacropyuma01[[5]], "yumafanta05", format = "GTiff")
writeRaster(fantacropyuma06[[1]], "yumafanta06", format = "GTiff")
writeRaster(fantacropyuma06[[2]], "yumafanta07", format = "GTiff")
writeRaster(fantacropyuma06[[3]], "yumafanta08", format = "GTiff")
writeRaster(fantacropyuma06[[4]], "yumafanta09", format = "GTiff")
writeRaster(fantacropyuma06[[5]], "yumafanta10", format = "GTiff")
writeRaster(fantacropyuma11[[1]], "yumafanta11", format = "GTiff")
writeRaster(fantacropyuma11[[2]], "yumafanta12", format = "GTiff")
writeRaster(fantacropyuma11[[3]], "yumafanta13", format = "GTiff")
writeRaster(fantacropyuma11[[4]], "yumafanta14", format = "GTiff")
writeRaster(fantacropyuma11[[5]], "yumafanta15", format = "GTiff")
writeRaster(fantacropyuma11[[6]], "yumafanta16", format = "GTiff")
writeRaster(fantacropyuma11[[7]], "yumafanta17", format = "GTiff")


#Write Raster
writeRaster(ndvinlcdPHX01, "phxndvisum01", format = "GTiff")
writeRaster(ndvinlcdPHX02, "phxndvisum02", format = "GTiff")
writeRaster(ndvinlcdPHX03, "phxndvisum03", format = "GTiff")
writeRaster(ndvinlcdPHX04, "phxndvisum04", format = "GTiff")
writeRaster(ndvinlcdPHX05, "phxndvisum05", format = "GTiff")
writeRaster(ndvinlcdPHX06, "phxndvisum06", format = "GTiff")
writeRaster(ndvinlcdPHX07, "phxndvisum07", format = "GTiff")
writeRaster(ndvinlcdPHX08, "phxndvisum08", format = "GTiff")
writeRaster(ndvinlcdPHX09, "phxndvisum09", format = "GTiff")
writeRaster(ndvinlcdPHX10, "phxndvisum10", format = "GTiff")
writeRaster(ndvinlcdPHX11, "phxndvisum11", format = "GTiff")
writeRaster(ndvinlcdPHX12, "phxndvisum12", format = "GTiff")
writeRaster(ndvinlcdPHX13, "phxndvisum13", format = "GTiff")
writeRaster(ndvinlcdPHX14, "phxndvisum14", format = "GTiff")
writeRaster(ndvinlcdPHX15, "phxndvisum15", format = "GTiff")
writeRaster(ndvinlcdPHX16, "phxndvisum16", format = "GTiff")
writeRaster(ndvinlcdPHX17, "phxndvisum17", format = "GTiff")


writeRaster(ndvinlcdYUMA01, "YUMAndvisum01", format = "GTiff")
writeRaster(ndvinlcdYUMA02, "YUMAndvisum02", format = "GTiff")
writeRaster(ndvinlcdYUMA03, "YUMAndvisum03", format = "GTiff")
writeRaster(ndvinlcdYUMA04, "YUMAndvisum04", format = "GTiff")
writeRaster(ndvinlcdYUMA05, "YUMAndvisum05", format = "GTiff")
writeRaster(ndvinlcdYUMA06, "YUMAndvisum06", format = "GTiff")
writeRaster(ndvinlcdYUMA07, "YUMAndvisum07", format = "GTiff")
writeRaster(ndvinlcdYUMA08, "YUMAndvisum08", format = "GTiff")
writeRaster(ndvinlcdYUMA09, "YUMAndvisum09", format = "GTiff")
writeRaster(ndvinlcdYUMA10, "YUMAndvisum10", format = "GTiff")
writeRaster(ndvinlcdYUMA11, "YUMAndvisum11", format = "GTiff")
writeRaster(ndvinlcdYUMA12, "YUMAndvisum12", format = "GTiff")
writeRaster(ndvinlcdYUMA13, "YUMAndvisum13", format = "GTiff")
writeRaster(ndvinlcdYUMA14, "YUMAndvisum14", format = "GTiff")
writeRaster(ndvinlcdYUMA15, "YUMAndvisum15", format = "GTiff")
writeRaster(ndvinlcdYUMA16, "YUMAndvisum16", format = "GTiff")
writeRaster(ndvinlcdYUMA17, "YUMAndvisum17", format = "GTiff")