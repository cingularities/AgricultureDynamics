
gc()
removeTmpFiles()
rm(list=ls())

install.packages("raster")
library(raster)
library(rgdal)
library(ggplot2)
library(maptools)


setwd('U:/Land Cover Change/Senior Water Rights/rcsv/updatedNDVI2019')


gc()
removeTmpFiles()
rm(list=ls())


#YUMA Fanta
phxfanta2001<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2001NLCD/phx_2001_SD3_1_26_19.tif")
phxndvi2001<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2001/PHOENIX_NDVI2001305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2001
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2001agPHX <-  projectRaster(NLCD2001agPHX, phxndvi2001)
phxfanta2001 <-  projectRaster(phxfanta2001, phxndvi2001)


#mask
croplandFANTAPHX01AC <- mask(phxfanta2001, NLCD2001agPHX)
plot(croplandFANTAPHX01AC)

#Crop
croplandFANTAPHX01AC[croplandFANTAPHX01AC != 2] = NA
croplandFANTAPHX01Active <- croplandFANTAPHX01AC
plot(croplandFANTAPHX01Active)


#2001 AG mask, matrix, mean, extract, 
#2001
ndviPHX01 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2001"), full.names = TRUE, pattern = ".tif$")
ndviPHX01
ndvistackPHX01 <- stack(ndviPHX01, bands=1)
croplandPHX01 <- mask(ndvistackPHX01, croplandFANTAPHX01Active)
writeRaster(croplandPHX01, "croplandPHX01", format = "GTiff")

cropdataPHX01 <- as.matrix(croplandPHX01, mode="list")
ncell(! is.na(cropdataPHX01[]))
naomitcropdataPHX01 <- na.omit(cropdataPHX01)
meancropPHX01 <- colMeans(naomitcropdataPHX01, na.rm = FALSE, dims = 1)



write.csv(meancropPHX01, file =  "updatemeanActivecropPHX01")



gc()
removeTmpFiles()
rm(list=ls())



#Phoenix Fanta
phxfanta2002<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2001NLCD/phx_2002_SD3_1_26_19.tif.tif")
phxndvi2002<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2002/PHOENIX_NDVI2002305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2002
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2001agPHX <-  projectRaster(NLCD2001agPHX, phxndvi2002)
phxfanta2002 <-  projectRaster(phxfanta2002, phxndvi2002)


#mask
croplandFANTAPHX02AC <- mask(phxfanta2002, NLCD2001agPHX)
plot(croplandFANTAPHX02AC)


#Crop
croplandFANTAPHX02AC[croplandFANTAPHX02AC != 2] = NA
croplandFANTAPHX02Active <- croplandFANTAPHX02AC



#2002 AG mask, matrix, mean, extract, 
#2002
ndviPHX02 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2002"), full.names = TRUE, pattern = ".tif$")
ndviPHX02
ndvistackPHX02 <- stack(ndviPHX02, bands=1)
croplandPHX02 <- mask(ndvistackPHX02, croplandFANTAPHX02Active)
writeRaster(croplandPHX02, "croplandPHX02", format = "GTiff")


cropdataPHX02 <- as.matrix(croplandPHX02, mode="list")
ncell(! is.na(cropdataPHX02[]))
naomitcropdataPHX02 <- na.omit(cropdataPHX02)
meancropPHX02 <- colMeans(naomitcropdataPHX02, na.rm = FALSE, dims = 1)

write.csv(meancropPHX02, file =  "updatemeanActivecropPHX02")




gc()
removeTmpFiles()
rm(list=ls())


#Phoenix Fanta
phxfanta2003<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2001NLCD/phx_2003_SD3_1_26_19.tif.tif")
phxndvi2003<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2003/PHOENIX_NDVI2003305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2003
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2001agPHX <-  projectRaster(NLCD2001agPHX, phxndvi2003)
phxfanta2003 <-  projectRaster(phxfanta2003, phxndvi2003)


#mask
croplandFANTAPHX03AC <- mask(phxfanta2003, NLCD2001agPHX)
croplandFANTAPHX03FA <- mask(phxfanta2003, NLCD2001agPHX)
plot(croplandFANTAPHX03AC)


#Crop
croplandFANTAPHX03AC[croplandFANTAPHX03AC != 2] = NA
croplandFANTAPHX03Active <- croplandFANTAPHX03AC



#2003 AG mask, matrix, mean, extract, 
#2003
ndviPHX03 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2003"), full.names = TRUE, pattern = ".tif$")
ndviPHX03
ndvistackPHX03 <- stack(ndviPHX03, bands=1)
croplandPHX03 <- mask(ndvistackPHX03, croplandFANTAPHX03Active)
writeRaster(croplandPHX03, "croplandPHX03", format = "GTiff")

cropdataPHX03 <- as.matrix(croplandPHX03, mode="list")
ncell(! is.na(cropdataPHX03[]))
naomitcropdataPHX03 <- na.omit(cropdataPHX03)
meancropPHX03 <- colMeans(naomitcropdataPHX03, na.rm = FALSE, dims = 1)

write.csv(meancropPHX03, file =  "updatemeanActivecropPHX03")




gc()
removeTmpFiles()
rm(list=ls())

#Phoenix Fanta
phxfanta2004<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2001NLCD/phx_2004_SD3_1_26_19.tif")
phxndvi2004<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2004/PHOENIX_NDVI2004305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2004
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2001agPHX <-  projectRaster(NLCD2001agPHX, phxndvi2004)
phxfanta2004 <-  projectRaster(phxfanta2004, phxndvi2004)


#mask
croplandFANTAPHX04AC <- mask(phxfanta2004, NLCD2001agPHX)
croplandFANTAPHX04FA <- mask(phxfanta2004, NLCD2001agPHX)
plot(croplandFANTAPHX04AC)


#Crop
croplandFANTAPHX04AC[croplandFANTAPHX04AC != 2] = NA
croplandFANTAPHX04Active <- croplandFANTAPHX04AC



#2004 AG mask, matrix, mean, extract, 
#2004
ndviPHX04 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2004"), full.names = TRUE, pattern = ".tif$")
ndviPHX04
ndvistackPHX04 <- stack(ndviPHX04, bands=1)
croplandPHX04 <- mask(ndvistackPHX04, croplandFANTAPHX04Active)
writeRaster(croplandPHX04, "croplandPHX04", format = "GTiff")

cropdataPHX04 <- as.matrix(croplandPHX04, mode="list")
ncell(! is.na(cropdataPHX04[]))
naomitcropdataPHX04 <- na.omit(cropdataPHX04)
meancropPHX04 <- colMeans(naomitcropdataPHX04, na.rm = FALSE, dims = 1)

write.csv(meancropPHX04, file =  "updatemeanActivecropPHX04")




gc()
removeTmpFiles()
rm(list=ls())



#Phoenix Fanta
phxfanta2005<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2001NLCD/phx_2005_SD3_1_26_19.tif")
phxndvi2005<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2005/PHOENIX_NDVI2005305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2005
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2001agPHX <-  projectRaster(NLCD2001agPHX, phxndvi2005)
phxfanta2005 <-  projectRaster(phxfanta2005, phxndvi2005)


#mask
croplandFANTAPHX05AC <- mask(phxfanta2005, NLCD2001agPHX)
croplandFANTAPHX05FA <- mask(phxfanta2005, NLCD2001agPHX)
plot(croplandFANTAPHX05AC)


#Crop
croplandFANTAPHX05AC[croplandFANTAPHX05AC != 2] = NA
croplandFANTAPHX05Active <- croplandFANTAPHX05AC



#2005 AG mask, matrix, mean, extract, 
#2005
ndviPHX05 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2005"), full.names = TRUE, pattern = ".tif$")
ndviPHX05
ndvistackPHX05 <- stack(ndviPHX05, bands=1)
croplandPHX05 <- mask(ndvistackPHX05, croplandFANTAPHX05Active)
writeRaster(croplandPHX05, "croplandPHX05", format = "GTiff")

cropdataPHX05 <- as.matrix(croplandPHX05, mode="list")
ncell(! is.na(cropdataPHX05[]))
naomitcropdataPHX05 <- na.omit(cropdataPHX05)
meancropPHX05 <- colMeans(naomitcropdataPHX05, na.rm = FALSE, dims = 1)

write.csv(meancropPHX05, file =  "updatemeanActivecropPHX05")




gc()
removeTmpFiles()
rm(list=ls())


#Phoenix Fanta
phxfanta2006<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2006NLCD/phx_2006_SD3_1_26_19.tif")
phxndvi2006<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2006/PHOENIX_NDVI2006305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2006
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2006agPHX <-  projectRaster(NLCD2006agPHX, phxndvi2006)
phxfanta2006 <-  projectRaster(phxfanta2006, phxndvi2006)


#mask
croplandFANTAPHX06AC <- mask(phxfanta2006, NLCD2006agPHX)
croplandFANTAPHX06FA <- mask(phxfanta2006, NLCD2006agPHX)
plot(croplandFANTAPHX06AC)


#Crop
croplandFANTAPHX06AC[croplandFANTAPHX06AC != 2] = NA
croplandFANTAPHX06Active <- croplandFANTAPHX06AC



#2006 AG mask, matrix, mean, extract, 
#2006
ndviPHX06 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2006"), full.names = TRUE, pattern = ".tif$")
ndviPHX06
ndvistackPHX06 <- stack(ndviPHX06, bands=1)
croplandPHX06 <- mask(ndvistackPHX06, croplandFANTAPHX06Active)
writeRaster(croplandPHX06, "croplandPHX06", format = "GTiff")


cropdataPHX06 <- as.matrix(croplandPHX06, mode="list")
ncell(! is.na(cropdataPHX06[]))
naomitcropdataPHX06 <- na.omit(cropdataPHX06)
meancropPHX06 <- colMeans(naomitcropdataPHX06, na.rm = FALSE, dims = 1)

write.csv(meancropPHX06, file =  "updatemeanActivecropPHX06")





gc()
removeTmpFiles()
rm(list=ls())





#Phoenix Fanta
phxfanta2007<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2006NLCD/phx_2007_SD3_1_26_19.tif")
phxndvi2007<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2007/PHOENIX_NDVI2007305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2007
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2006agPHX <-  projectRaster(NLCD2006agPHX, phxndvi2007)
phxfanta2007 <-  projectRaster(phxfanta2007, phxndvi2007)


#mask
croplandFANTAPHX07AC <- mask(phxfanta2007, NLCD2006agPHX)
croplandFANTAPHX07FA <- mask(phxfanta2007, NLCD2006agPHX)
plot(croplandFANTAPHX07AC)



#Crop
croplandFANTAPHX07AC[croplandFANTAPHX07AC != 2] = NA
croplandFANTAPHX07Active <- croplandFANTAPHX07AC



#2007 AG mask, matrix, mean, extract, 
#2007
ndviPHX07 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2007"), full.names = TRUE, pattern = ".tif$")
ndviPHX07
ndvistackPHX07 <- stack(ndviPHX07, bands=1)
croplandPHX07 <- mask(ndvistackPHX07, croplandFANTAPHX07Active)
writeRaster(croplandPHX07, "croplandPHX07", format = "GTiff")

cropdataPHX07 <- as.matrix(croplandPHX07, mode="list")
ncell(! is.na(cropdataPHX07[]))
naomitcropdataPHX07 <- na.omit(cropdataPHX07)
meancropPHX07 <- colMeans(naomitcropdataPHX07, na.rm = FALSE, dims = 1)

write.csv(meancropPHX07, file =  "updatemeanActivecropPHX07")





gc()
removeTmpFiles()
rm(list=ls())


#Phoenix Fanta
phxfanta2008<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2006NLCD/phx_2008_SD3_1_26_19.tif")
phxndvi2008<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2008/PHOENIX_NDVI2008305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2008
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2006agPHX <-  projectRaster(NLCD2006agPHX, phxndvi2008)
phxfanta2008 <-  projectRaster(phxfanta2008, phxndvi2008)


#mask
croplandFANTAPHX08AC <- mask(phxfanta2008, NLCD2006agPHX)
croplandFANTAPHX08FA <- mask(phxfanta2008, NLCD2006agPHX)
plot(croplandFANTAPHX08AC)



#Crop
croplandFANTAPHX08AC[croplandFANTAPHX08AC != 2] = NA
croplandFANTAPHX08Active <- croplandFANTAPHX08AC



#2008 AG mask, matrix, mean, extract, 
#2008
ndviPHX08 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2008"), full.names = TRUE, pattern = ".tif$")
ndviPHX08
ndvistackPHX08 <- stack(ndviPHX08, bands=1)
croplandPHX08 <- mask(ndvistackPHX08, croplandFANTAPHX08Active)
writeRaster(croplandPHX08, "croplandPHX08", format = "GTiff")

cropdataPHX08 <- as.matrix(croplandPHX08, mode="list")
ncell(! is.na(cropdataPHX08[]))
naomitcropdataPHX08 <- na.omit(cropdataPHX08)
meancropPHX08 <- colMeans(naomitcropdataPHX08, na.rm = FALSE, dims = 1)

write.csv(meancropPHX08, file =  "updatemeanActivecropPHX08")





gc()
removeTmpFiles()
rm(list=ls())


#Phoenix Fanta
phxfanta2009<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2006NLCD/phx_2009_SD3_1_26_19.tif")
phxndvi2009<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2009/PHOENIX_NDVI2009305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2009
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2006agPHX <-  projectRaster(NLCD2006agPHX, phxndvi2009)
phxfanta2009 <-  projectRaster(phxfanta2009, phxndvi2009)


#mask
croplandFANTAPHX09AC <- mask(phxfanta2009, NLCD2006agPHX)
croplandFANTAPHX09FA <- mask(phxfanta2009, NLCD2006agPHX)
plot(croplandFANTAPHX09AC)



#Crop
croplandFANTAPHX09AC[croplandFANTAPHX09AC != 2] = NA
croplandFANTAPHX09Active <- croplandFANTAPHX09AC



#2009 AG mask, matrix, mean, extract, 
#2009
ndviPHX09 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2009"), full.names = TRUE, pattern = ".tif$")
ndviPHX09
ndvistackPHX09 <- stack(ndviPHX09, bands=1)
croplandPHX09 <- mask(ndvistackPHX09, croplandFANTAPHX09Active)
writeRaster(croplandPHX09, "croplandPHX09", format = "GTiff")


cropdataPHX09 <- as.matrix(croplandPHX09, mode="list")
ncell(! is.na(cropdataPHX09[]))
naomitcropdataPHX09 <- na.omit(cropdataPHX09)
meancropPHX09 <- colMeans(naomitcropdataPHX09, na.rm = FALSE, dims = 1)

write.csv(meancropPHX09, file =  "updatemeanActivecropPHX09")





gc()
removeTmpFiles()
rm(list=ls())


#Phoenix Fanta
phxfanta2010<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019/fanta_phx_2006NLCD/phx_2010_SD3_1_26_19.tif")
phxndvi2010<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2010/PHOENIX_NDVI2010305.tif")

#NLCD
#NLCD 2001
NLCD2001agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2001.tif")
NLCD2001agPHX

#NLCD 2006
NLCD2006agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2006.tif")
NLCD2006agPHX

#NLCD 2010
NLCD2011agPHX <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/PHX_NLCD_2011.tif")
NLCD2011agPHX



#Selecting Field
NLCD2001agPHX[NLCD2001agPHX != 82] = NA
plot(NLCD2001agPHX)


NLCD2006agPHX[NLCD2006agPHX != 82] = NA
plot(NLCD2006agPHX)

NLCD2011agPHX[NLCD2011agPHX != 82] = NA
plot(NLCD2011agPHX)

#ProjectRaster
NLCD2006agPHX <-  projectRaster(NLCD2006agPHX, phxndvi2010)
phxfanta2010 <-  projectRaster(phxfanta2010, phxndvi2010)


#mask
croplandFANTAPHX10AC <- mask(phxfanta2010, NLCD2006agPHX)
croplandFANTAPHX10FA <- mask(phxfanta2010, NLCD2006agPHX)
plot(croplandFANTAPHX10AC)



#Crop
croplandFANTAPHX10AC[croplandFANTAPHX10AC != 2] = NA
croplandFANTAPHX10Active <- croplandFANTAPHX10AC



#2010 AG mask, matrix, mean, extract, 
#2010
ndviPHX10 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2010"), full.names = TRUE, pattern = ".tif$")
ndviPHX10
ndvistackPHX10 <- stack(ndviPHX10, bands=1)
croplandPHX10 <- mask(ndvistackPHX10, croplandFANTAPHX10Active)
writeRaster(croplandPHX10, "croplandPHX10", format = "GTiff")

cropdataPHX10 <- as.matrix(croplandPHX10, mode="list")
ncell(! is.na(cropdataPHX10[]))
naomitcropdataPHX10 <- na.omit(cropdataPHX10)
meancropPHX10 <- colMeans(naomitcropdataPHX10, na.rm = FALSE, dims = 1)

write.csv(meancropPHX10, file =  "updatemeanActivecropPHX10")




gc()
removeTmpFiles()
rm(list=ls())

#Phoenix Fanta
phxfanta2011<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019//fanta_phx_2011NLCD/phx_2011_SD3_1_26_19.tif")
phxndvi2011<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2011/PHOENIX_NDVI2011305.tif")

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

#ProjectRaster
NLCD2011agPHX <-  projectRaster(NLCD2011agPHX, phxndvi2011)
phxfanta2011 <-  projectRaster(phxfanta2011, phxndvi2011)


#mask
croplandFANTAPHX11AC <- mask(phxfanta2011, NLCD2011agPHX)
croplandFANTAPHX11FA <- mask(phxfanta2011, NLCD2011agPHX)
plot(croplandFANTAPHX11AC)



#Crop
croplandFANTAPHX11AC[croplandFANTAPHX11AC != 2] = NA
croplandFANTAPHX11Active <- croplandFANTAPHX11AC



#2011 AG mask, matrix, mean, extract, 
#2011
ndviPHX11 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2011"), full.names = TRUE, pattern = ".tif$")
ndviPHX11
ndvistackPHX11 <- stack(ndviPHX11, bands=1)
croplandPHX11 <- mask(ndvistackPHX11, croplandFANTAPHX11Active)
writeRaster(croplandPHX11, "croplandPHX11", format = "GTiff")


cropdataPHX11 <- as.matrix(croplandPHX11, mode="list")
ncell(! is.na(cropdataPHX11[]))
naomitcropdataPHX11 <- na.omit(cropdataPHX11)
meancropPHX11 <- colMeans(naomitcropdataPHX11, na.rm = FALSE, dims = 1)

write.csv(meancropPHX11, file =  "updatemeanActivecropPHX11")


gc()
removeTmpFiles()
rm(list=ls())
#Phoenix Fanta
phxfanta2012<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019//fanta_phx_2011NLCD/phx_2012_SD3_1_26_19.tif")
phxndvi2012<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2012/PHOENIX_NDVI2012305.tif")

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

#ProjectRaster
NLCD2011agPHX <-  projectRaster(NLCD2011agPHX, phxndvi2012)
phxfanta2012 <-  projectRaster(phxfanta2012, phxndvi2012)


#mask
croplandFANTAPHX12AC <- mask(phxfanta2012, NLCD2011agPHX)
croplandFANTAPHX12FA <- mask(phxfanta2012, NLCD2011agPHX)
plot(croplandFANTAPHX12AC)



#Crop
croplandFANTAPHX12AC[croplandFANTAPHX12AC != 2] = NA
croplandFANTAPHX12Active <- croplandFANTAPHX12AC



#2012 AG mask, matrix, mean, extract, 
#2012
ndviPHX12 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2012"), full.names = TRUE, pattern = ".tif$")
ndviPHX12
ndvistackPHX12 <- stack(ndviPHX12, bands=1)
croplandPHX12 <- mask(ndvistackPHX12, croplandFANTAPHX12Active)
writeRaster(croplandPHX12, "croplandPHX12", format = "GTiff")

cropdataPHX12 <- as.matrix(croplandPHX12, mode="list")
ncell(! is.na(cropdataPHX12[]))
naomitcropdataPHX12 <- na.omit(cropdataPHX12)
meancropPHX12 <- colMeans(naomitcropdataPHX12, na.rm = FALSE, dims = 1)

write.csv(meancropPHX12, file =  "updatemeanActivecropPHX12")

gc()
removeTmpFiles()
rm(list=ls())

#Phoenix Fanta
phxfanta2013<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019//fanta_phx_2011NLCD/phx_2013_SD3_1_26_19.tif")
phxndvi2013<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2013/PHOENIX_NDVI2013305.tif")

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

#ProjectRaster
NLCD2011agPHX <-  projectRaster(NLCD2011agPHX, phxndvi2013)
phxfanta2013 <-  projectRaster(phxfanta2013, phxndvi2013)


#mask
croplandFANTAPHX13AC <- mask(phxfanta2013, NLCD2011agPHX)
croplandFANTAPHX13FA <- mask(phxfanta2013, NLCD2011agPHX)
plot(croplandFANTAPHX13AC)



#Crop
croplandFANTAPHX13AC[croplandFANTAPHX13AC != 2] = NA
croplandFANTAPHX13Active <- croplandFANTAPHX13AC



#2013 AG mask, matrix, mean, extract, 
#2013
ndviPHX13 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2013"), full.names = TRUE, pattern = ".tif$")
ndviPHX13
ndvistackPHX13 <- stack(ndviPHX13, bands=1)
croplandPHX13 <- mask(ndvistackPHX13, croplandFANTAPHX13Active)
writeRaster(croplandPHX13, "croplandPHX13", format = "GTiff")


cropdataPHX13 <- as.matrix(croplandPHX13, mode="list")
ncell(! is.na(cropdataPHX13[]))
naomitcropdataPHX13 <- na.omit(cropdataPHX13)
meancropPHX13 <- colMeans(naomitcropdataPHX13, na.rm = FALSE, dims = 1)

write.csv(meancropPHX13, file =  "updatemeanActivecropPHX13")


gc()
removeTmpFiles()
rm(list=ls())

#Phoenix Fanta
phxfanta2014<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019//fanta_phx_2011NLCD/phx_2014_SD3_1_26_19.tif")
phxndvi2014<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2014/PHOENIX_NDVI2014305.tif")

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

#ProjectRaster
NLCD2011agPHX <-  projectRaster(NLCD2011agPHX, phxndvi2014)
phxfanta2014 <-  projectRaster(phxfanta2014, phxndvi2014)


#mask
croplandFANTAPHX14AC <- mask(phxfanta2014, NLCD2011agPHX)
croplandFANTAPHX14FA <- mask(phxfanta2014, NLCD2011agPHX)
plot(croplandFANTAPHX14AC)



#Crop
croplandFANTAPHX14AC[croplandFANTAPHX14AC != 2] = NA
croplandFANTAPHX14Active <- croplandFANTAPHX14AC



#2014 AG mask, matrix, mean, extract, 
#2014
ndviPHX14 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2014"), full.names = TRUE, pattern = ".tif$")
ndviPHX14
ndvistackPHX14 <- stack(ndviPHX14, bands=1)
croplandPHX14 <- mask(ndvistackPHX14, croplandFANTAPHX14Active)
writeRaster(croplandPHX14, "croplandPHX14", format = "GTiff")


cropdataPHX14 <- as.matrix(croplandPHX14, mode="list")
ncell(! is.na(cropdataPHX14[]))
naomitcropdataPHX14 <- na.omit(cropdataPHX14)
meancropPHX14 <- colMeans(naomitcropdataPHX14, na.rm = FALSE, dims = 1)

write.csv(meancropPHX14, file =  "updatemeanActivecropPHX14")


gc()
removeTmpFiles()
rm(list=ls())

#Phoenix Fanta
phxfanta2015<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019//fanta_phx_2011NLCD/phx_2015_SD3_1_26_19.tif")
phxndvi2015<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2015/PHOENIX_NDVI2015305.tif")

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

#ProjectRaster
NLCD2011agPHX <-  projectRaster(NLCD2011agPHX, phxndvi2015)
phxfanta2015 <-  projectRaster(phxfanta2015, phxndvi2015)


#mask
croplandFANTAPHX15AC <- mask(phxfanta2015, NLCD2011agPHX)
croplandFANTAPHX15FA <- mask(phxfanta2015, NLCD2011agPHX)
plot(croplandFANTAPHX15AC)



#Crop
croplandFANTAPHX15AC[croplandFANTAPHX15AC != 2] = NA
croplandFANTAPHX15Active <- croplandFANTAPHX15AC



#2015 AG mask, matrix, mean, extract, 
#2015
ndviPHX15 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2015"), full.names = TRUE, pattern = ".tif$")
ndviPHX15
ndvistackPHX15 <- stack(ndviPHX15, bands=1)
croplandPHX15 <- mask(ndvistackPHX15, croplandFANTAPHX15Active)
writeRaster(croplandPHX15, "croplandPHX15", format = "GTiff")


cropdataPHX15 <- as.matrix(croplandPHX15, mode="list")
ncell(! is.na(cropdataPHX15[]))
naomitcropdataPHX15 <- na.omit(cropdataPHX15)
meancropPHX15 <- colMeans(naomitcropdataPHX15, na.rm = FALSE, dims = 1)

write.csv(meancropPHX15, file =  "updatemeanActivecropPHX15")




gc()
removeTmpFiles()
rm(list=ls())

#Phoenix Fanta
phxfanta2016<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019//fanta_phx_2011NLCD/phx_2016_SD3_9_28.tif")
phxndvi2016<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2016/PHOENIX_NDVI2016305.tif")

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

#ProjectRaster
NLCD2011agPHX <-  projectRaster(NLCD2011agPHX, phxndvi2016)
phxfanta2016 <-  projectRaster(phxfanta2016, phxndvi2016)


#mask
croplandFANTAPHX16AC <- mask(phxfanta2016, NLCD2011agPHX)
croplandFANTAPHX16FA <- mask(phxfanta2016, NLCD2011agPHX)
plot(croplandFANTAPHX16AC)



#Crop
croplandFANTAPHX16AC[croplandFANTAPHX16AC != 2] = NA
croplandFANTAPHX16Active <- croplandFANTAPHX16AC



#2016 AG mask, matrix, mean, extract, 
#2016
ndviPHX16 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2016"), full.names = TRUE, pattern = ".tif$")
ndviPHX16
ndvistackPHX16 <- stack(ndviPHX16, bands=1)
croplandPHX16 <- mask(ndvistackPHX16, croplandFANTAPHX16Active)
writeRaster(croplandPHX16, "croplandPHX16", format = "GTiff")

cropdataPHX16 <- as.matrix(croplandPHX16, mode="list")
ncell(! is.na(cropdataPHX16[]))
naomitcropdataPHX16 <- na.omit(cropdataPHX16)
meancropPHX16 <- colMeans(naomitcropdataPHX16, na.rm = FALSE, dims = 1)

write.csv(meancropPHX16, file =  "updatemeanActivecropPHX16")



gc()
removeTmpFiles()
rm(list=ls())


#Phoenix Fanta
phxfanta2017<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/PHX_FANTA_SD3_2019//fanta_phx_2011NLCD/phx_2017_SD3_9_26.tif")
phxndvi2017<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2017/PHOENIX_NDVI2017305.tif")

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

#ProjectRaster
NLCD2011agPHX <-  projectRaster(NLCD2011agPHX, phxndvi2017)
phxfanta2017 <-  projectRaster(phxfanta2017, phxndvi2017)


#mask
croplandFANTAPHX17AC <- mask(phxfanta2017, NLCD2011agPHX)
croplandFANTAPHX17FA <- mask(phxfanta2017, NLCD2011agPHX)
plot(croplandFANTAPHX17AC)



#Crop
croplandFANTAPHX17AC[croplandFANTAPHX17AC != 2] = NA
croplandFANTAPHX17Active <- croplandFANTAPHX17AC



#2017 AG mask, matrix, mean, extract, 
#2017
ndviPHX17 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/PHOENIX_NDVI/2017"), full.names = TRUE, pattern = ".tif$")
ndviPHX17
ndvistackPHX17 <- stack(ndviPHX17, bands=1)
croplandPHX17 <- mask(ndvistackPHX17, croplandFANTAPHX17Active)
writeRaster(croplandPHX17, "croplandPHX17", format = "GTiff")


cropdataPHX17 <- as.matrix(croplandPHX17, mode="list")
ncell(! is.na(cropdataPHX17[]))
naomitcropdataPHX17 <- na.omit(cropdataPHX17)
meancropPHX17 <- colMeans(naomitcropdataPHX17, na.rm = FALSE, dims = 1)

write.csv(meancropPHX17, file =  "updatemeanActivecropPHX17")



gc()
removeTmpFiles()
rm(list=ls())


#YUMA Fanta
YUMAfanta2001<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019/_FANTA_SD3_2019//fanta_yuma_2001NLCD/YUMA_2001_SD3_1_26_19.tif")
YUMAndvi2001<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2001/YUMA_NDVI2001305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2001
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2001agYUMA <-  projectRaster(NLCD2001agYUMA, YUMAndvi2001)
YUMAfanta2001 <-  projectRaster(YUMAfanta2001, YUMAndvi2001)


#mask
croplandFANTAYUMA01AC <- mask(YUMAfanta2001, NLCD2001agYUMA)
croplandFANTAYUMA01FA <- mask(YUMAfanta2001, NLCD2001agYUMA)
plot(croplandFANTAYUMA01AC)


#Crop
croplandFANTAYUMA01AC[croplandFANTAYUMA01AC != 2] = NA
croplandFANTAYUMA01Active <- croplandFANTAYUMA01AC



#2001 AG mask, matrix, mean, extract, 
#2001
ndviYUMA01 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2001"), full.names = TRUE, pattern = ".tif$")
ndviYUMA01
ndvistackYUMA01 <- stack(ndviYUMA01, bands=1)
croplandYUMA01 <- mask(ndvistackYUMA01, croplandFANTAYUMA01Active)
writeRaster(croplandYUMA01, "croplandYUMA01", format = "GTiff")

cropdataYUMA01 <- as.matrix(croplandYUMA01, mode="list")
ncell(! is.na(cropdataYUMA01[]))
naomitcropdataYUMA01 <- na.omit(cropdataYUMA01)
meancropYUMA01 <- colMeans(naomitcropdataYUMA01, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA01, file =  "updatemeanActivecropYUMA01")



gc()
removeTmpFiles()
rm(list=ls())



#YUMA Fanta
YUMAfanta2002<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2001NLCD/YUMA_2002_SD3_1_26_19.tif")
YUMAndvi2002<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2002/YUMA_NDVI2002305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2002
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2001agYUMA <-  projectRaster(NLCD2001agYUMA, YUMAndvi2002)
YUMAfanta2002 <-  projectRaster(YUMAfanta2002, YUMAndvi2002)


#mask
croplandFANTAYUMA02AC <- mask(YUMAfanta2002, NLCD2001agYUMA)
croplandFANTAYUMA02FA <- mask(YUMAfanta2002, NLCD2001agYUMA)
plot(croplandFANTAYUMA02AC)



#Crop
croplandFANTAYUMA02AC[croplandFANTAYUMA02AC != 2] = NA
croplandFANTAYUMA02Active <- croplandFANTAYUMA02AC



#2002 AG mask, matrix, mean, extract, 
#2002
ndviYUMA02 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2002"), full.names = TRUE, pattern = ".tif$")
ndviYUMA02
ndvistackYUMA02 <- stack(ndviYUMA02, bands=1)
croplandYUMA02 <- mask(ndvistackYUMA02, croplandFANTAYUMA02Active)
writeRaster(croplandYUMA02, "croplandYUMA02", format = "GTiff")


cropdataYUMA02 <- as.matrix(croplandYUMA02, mode="list")
ncell(! is.na(cropdataYUMA02[]))
naomitcropdataYUMA02 <- na.omit(cropdataYUMA02)
meancropYUMA02 <- colMeans(naomitcropdataYUMA02, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA02, file =  "updatemeanActivecropYUMA02")




gc()
removeTmpFiles()
rm(list=ls())


#YUMA Fanta
YUMAfanta2003<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2001NLCD/YUMA_2003_SD3_1_26_19.tif")
YUMAndvi2003<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2003/YUMA_NDVI2003305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2003
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2001agYUMA <-  projectRaster(NLCD2001agYUMA, YUMAndvi2003)
YUMAfanta2003 <-  projectRaster(YUMAfanta2003, YUMAndvi2003)


#mask
croplandFANTAYUMA03AC <- mask(YUMAfanta2003, NLCD2001agYUMA)
croplandFANTAYUMA03FA <- mask(YUMAfanta2003, NLCD2001agYUMA)
plot(croplandFANTAYUMA03AC)



#Crop
croplandFANTAYUMA03AC[croplandFANTAYUMA03AC != 2] = NA
croplandFANTAYUMA03Active <- croplandFANTAYUMA03AC



#2003 AG mask, matrix, mean, extract, 
#2003
ndviYUMA03 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2003"), full.names = TRUE, pattern = ".tif$")
ndviYUMA03
ndvistackYUMA03 <- stack(ndviYUMA03, bands=1)
croplandYUMA03 <- mask(ndvistackYUMA03, croplandFANTAYUMA03Active)
writeRaster(croplandYUMA03, "croplandYUMA03", format = "GTiff")

cropdataYUMA03 <- as.matrix(croplandYUMA03, mode="list")
ncell(! is.na(cropdataYUMA03[]))
naomitcropdataYUMA03 <- na.omit(cropdataYUMA03)
meancropYUMA03 <- colMeans(naomitcropdataYUMA03, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA03, file =  "updatemeanActivecropYUMA03")




gc()
removeTmpFiles()
rm(list=ls())

#YUMA Fanta
YUMAfanta2004<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2001NLCD/YUMA_2004_SD3_1_26_19.tif")
YUMAndvi2004<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2004/YUMA_NDVI2004305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2004
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2001agYUMA <-  projectRaster(NLCD2001agYUMA, YUMAndvi2004)
YUMAfanta2004 <-  projectRaster(YUMAfanta2004, YUMAndvi2004)


#mask
croplandFANTAYUMA04AC <- mask(YUMAfanta2004, NLCD2001agYUMA)
croplandFANTAYUMA04FA <- mask(YUMAfanta2004, NLCD2001agYUMA)
plot(croplandFANTAYUMA04AC)



#Crop
croplandFANTAYUMA04AC[croplandFANTAYUMA04AC != 2] = NA
croplandFANTAYUMA04Active <- croplandFANTAYUMA04AC



#2004 AG mask, matrix, mean, extract, 
#2004
ndviYUMA04 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2004"), full.names = TRUE, pattern = ".tif$")
ndviYUMA04
ndvistackYUMA04 <- stack(ndviYUMA04, bands=1)
croplandYUMA04 <- mask(ndvistackYUMA04, croplandFANTAYUMA04Active)
writeRaster(croplandYUMA04, "croplandYUMA04", format = "GTiff")

cropdataYUMA04 <- as.matrix(croplandYUMA04, mode="list")
ncell(! is.na(cropdataYUMA04[]))
naomitcropdataYUMA04 <- na.omit(cropdataYUMA04)
meancropYUMA04 <- colMeans(naomitcropdataYUMA04, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA04, file =  "updatemeanActivecropYUMA04")




gc()
removeTmpFiles()
rm(list=ls())



#YUMA Fanta
YUMAfanta2005<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2001NLCD/YUMA_2005_SD3_1_26_19.tif")
YUMAndvi2005<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2005/YUMA_NDVI2005305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2005
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2001agYUMA <-  projectRaster(NLCD2001agYUMA, YUMAndvi2005)
YUMAfanta2005 <-  projectRaster(YUMAfanta2005, YUMAndvi2005)


#mask
croplandFANTAYUMA05AC <- mask(YUMAfanta2005, NLCD2001agYUMA)
croplandFANTAYUMA05FA <- mask(YUMAfanta2005, NLCD2001agYUMA)
plot(croplandFANTAYUMA05AC)



#Crop
croplandFANTAYUMA05AC[croplandFANTAYUMA05AC != 2] = NA
croplandFANTAYUMA05Active <- croplandFANTAYUMA05AC



#2005 AG mask, matrix, mean, extract, 
#2005
ndviYUMA05 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2005"), full.names = TRUE, pattern = ".tif$")
ndviYUMA05
ndvistackYUMA05 <- stack(ndviYUMA05, bands=1)
croplandYUMA05 <- mask(ndvistackYUMA05, croplandFANTAYUMA05Active)
writeRaster(croplandYUMA05, "croplandYUMA05", format = "GTiff")


cropdataYUMA05 <- as.matrix(croplandYUMA05, mode="list")
ncell(! is.na(cropdataYUMA05[]))
naomitcropdataYUMA05 <- na.omit(cropdataYUMA05)
meancropYUMA05 <- colMeans(naomitcropdataYUMA05, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA05, file =  "updatemeanActivecropYUMA05")




gc()
removeTmpFiles()
rm(list=ls())


#YUMA Fanta
YUMAfanta2006<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2006NLCD/YUMA_2006_SD3_1_26_19.tif")
YUMAndvi2006<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2006/YUMA_NDVI2006305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2006
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2006agYUMA <-  projectRaster(NLCD2006agYUMA, YUMAndvi2006)
YUMAfanta2006 <-  projectRaster(YUMAfanta2006, YUMAndvi2006)


#mask
croplandFANTAYUMA06AC <- mask(YUMAfanta2006, NLCD2006agYUMA)
croplandFANTAYUMA06FA <- mask(YUMAfanta2006, NLCD2006agYUMA)
plot(croplandFANTAYUMA06AC)



#Crop
croplandFANTAYUMA06AC[croplandFANTAYUMA06AC != 2] = NA
croplandFANTAYUMA06Active <- croplandFANTAYUMA06AC



#2006 AG mask, matrix, mean, extract, 
#2006
ndviYUMA06 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2006"), full.names = TRUE, pattern = ".tif$")
ndviYUMA06
ndvistackYUMA06 <- stack(ndviYUMA06, bands=1)
croplandYUMA06 <- mask(ndvistackYUMA06, croplandFANTAYUMA06Active)
writeRaster(croplandYUMA06, "croplandYUMA06", format = "GTiff")

cropdataYUMA06 <- as.matrix(croplandYUMA06, mode="list")
ncell(! is.na(cropdataYUMA06[]))
naomitcropdataYUMA06 <- na.omit(cropdataYUMA06)
meancropYUMA06 <- colMeans(naomitcropdataYUMA06, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA06, file =  "updatemeanActivecropYUMA06")





gc()
removeTmpFiles()
rm(list=ls())





#YUMA Fanta
YUMAfanta2007<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2006NLCD/YUMA_2007_SD3_1_26_19.tif")
YUMAndvi2007<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2007/YUMA_NDVI2007305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2007
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2006agYUMA <-  projectRaster(NLCD2006agYUMA, YUMAndvi2007)
YUMAfanta2007 <-  projectRaster(YUMAfanta2007, YUMAndvi2007)


#mask
croplandFANTAYUMA07AC <- mask(YUMAfanta2007, NLCD2006agYUMA)
croplandFANTAYUMA07FA <- mask(YUMAfanta2007, NLCD2006agYUMA)
plot(croplandFANTAYUMA07AC)


#Crop
croplandFANTAYUMA07AC[croplandFANTAYUMA07AC != 2] = NA
croplandFANTAYUMA07Active <- croplandFANTAYUMA07AC



#2007 AG mask, matrix, mean, extract, 
#2007
ndviYUMA07 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2007"), full.names = TRUE, pattern = ".tif$")
ndviYUMA07
ndvistackYUMA07 <- stack(ndviYUMA07, bands=1)
croplandYUMA07 <- mask(ndvistackYUMA07, croplandFANTAYUMA07Active)
writeRaster(croplandYUMA07, "croplandYUMA07", format = "GTiff")



cropdataYUMA07 <- as.matrix(croplandYUMA07, mode="list")
ncell(! is.na(cropdataYUMA07[]))
naomitcropdataYUMA07 <- na.omit(cropdataYUMA07)
meancropYUMA07 <- colMeans(naomitcropdataYUMA07, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA07, file =  "updatemeanActivecropYUMA07")





gc()
removeTmpFiles()
rm(list=ls())


#YUMA Fanta
YUMAfanta2008<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2006NLCD/YUMA_2008_SD3_1_26_19.tif")
YUMAndvi2008<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2008/YUMA_NDVI2008305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2008
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2006agYUMA <-  projectRaster(NLCD2006agYUMA, YUMAndvi2008)
YUMAfanta2008 <-  projectRaster(YUMAfanta2008, YUMAndvi2008)


#mask
croplandFANTAYUMA08AC <- mask(YUMAfanta2008, NLCD2006agYUMA)
croplandFANTAYUMA08FA <- mask(YUMAfanta2008, NLCD2006agYUMA)
plot(croplandFANTAYUMA08AC)


#Crop
croplandFANTAYUMA08AC[croplandFANTAYUMA08AC != 2] = NA
croplandFANTAYUMA08Active <- croplandFANTAYUMA08AC



#2008 AG mask, matrix, mean, extract, 
#2008
ndviYUMA08 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2008"), full.names = TRUE, pattern = ".tif$")
ndviYUMA08
ndvistackYUMA08 <- stack(ndviYUMA08, bands=1)
croplandYUMA08 <- mask(ndvistackYUMA08, croplandFANTAYUMA08Active)
writeRaster(croplandYUMA08, "croplandYUMA08", format = "GTiff")

cropdataYUMA08 <- as.matrix(croplandYUMA08, mode="list")
ncell(! is.na(cropdataYUMA08[]))
naomitcropdataYUMA08 <- na.omit(cropdataYUMA08)
meancropYUMA08 <- colMeans(naomitcropdataYUMA08, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA08, file =  "updatemeanActivecropYUMA08")





gc()
removeTmpFiles()
rm(list=ls())


#YUMA Fanta
YUMAfanta2009<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2006NLCD/YUMA_2009_SD3_1_26_19.tif")
YUMAndvi2009<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2009/YUMA_NDVI2009305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2009
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2006agYUMA <-  projectRaster(NLCD2006agYUMA, YUMAndvi2009)
YUMAfanta2009 <-  projectRaster(YUMAfanta2009, YUMAndvi2009)


#mask
croplandFANTAYUMA09AC <- mask(YUMAfanta2009, NLCD2006agYUMA)
croplandFANTAYUMA09FA <- mask(YUMAfanta2009, NLCD2006agYUMA)
plot(croplandFANTAYUMA09AC)



#Crop
croplandFANTAYUMA09AC[croplandFANTAYUMA09AC != 2] = NA
croplandFANTAYUMA09Active <- croplandFANTAYUMA09AC



#2009 AG mask, matrix, mean, extract, 
#2009
ndviYUMA09 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2009"), full.names = TRUE, pattern = ".tif$")
ndviYUMA09
ndvistackYUMA09 <- stack(ndviYUMA09, bands=1)
croplandYUMA09 <- mask(ndvistackYUMA09, croplandFANTAYUMA09Active)
writeRaster(croplandYUMA09, "croplandYUMA09", format = "GTiff")

cropdataYUMA09 <- as.matrix(croplandYUMA09, mode="list")
ncell(! is.na(cropdataYUMA09[]))
naomitcropdataYUMA09 <- na.omit(cropdataYUMA09)
meancropYUMA09 <- colMeans(naomitcropdataYUMA09, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA09, file =  "updatemeanActivecropYUMA09")





gc()
removeTmpFiles()
rm(list=ls())


#YUMA Fanta
YUMAfanta2010<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2006NLCD/YUMA_2010_SD3_1_26_19.tif")
YUMAndvi2010<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2010/YUMA_NDVI2010305.tif")

#NLCD
#NLCD 2001
NLCD2001agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2001.tif")
NLCD2001agYUMA

#NLCD 2006
NLCD2006agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2006.tif")
NLCD2006agYUMA

#NLCD 2010
NLCD2011agYUMA <-raster("U:/Land Cover Change/Senior Water Rights/NLCD/YUMA_NLCD_2011.tif")
NLCD2011agYUMA



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2006agYUMA <-  projectRaster(NLCD2006agYUMA, YUMAndvi2010)
YUMAfanta2010 <-  projectRaster(YUMAfanta2010, YUMAndvi2010)


#mask
croplandFANTAYUMA10AC <- mask(YUMAfanta2010, NLCD2006agYUMA)
croplandFANTAYUMA10FA <- mask(YUMAfanta2010, NLCD2006agYUMA)
plot(croplandFANTAYUMA10AC)



#Crop
croplandFANTAYUMA10AC[croplandFANTAYUMA10AC != 2] = NA
croplandFANTAYUMA10Active <- croplandFANTAYUMA10AC



#2010 AG mask, matrix, mean, extract, 
#2010
ndviYUMA10 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2010"), full.names = TRUE, pattern = ".tif$")
ndviYUMA10
ndvistackYUMA10 <- stack(ndviYUMA10, bands=1)
croplandYUMA10 <- mask(ndvistackYUMA10, croplandFANTAYUMA10Active)
writeRaster(croplandYUMA10, "croplandYUMA10", format = "GTiff")

cropdataYUMA10 <- as.matrix(croplandYUMA10, mode="list")
ncell(! is.na(cropdataYUMA10[]))
naomitcropdataYUMA10 <- na.omit(cropdataYUMA10)
meancropYUMA10 <- colMeans(naomitcropdataYUMA10, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA10, file =  "updatemeanActivecropYUMA10")




gc()
removeTmpFiles()
rm(list=ls())

#YUMA Fanta
YUMAfanta2011<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2011NLCD/YUMA_2011_SD3_1_26_19.tif")
YUMAndvi2011<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2011/YUMA_NDVI2011305.tif")

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



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2011agYUMA <-  projectRaster(NLCD2011agYUMA, YUMAndvi2011)
YUMAfanta2011 <-  projectRaster(YUMAfanta2011, YUMAndvi2011)


#mask
croplandFANTAYUMA11AC <- mask(YUMAfanta2011, NLCD2011agYUMA)
croplandFANTAYUMA11FA <- mask(YUMAfanta2011, NLCD2011agYUMA)
plot(croplandFANTAYUMA11AC)



#Crop
croplandFANTAYUMA11AC[croplandFANTAYUMA11AC != 2] = NA
croplandFANTAYUMA11Active <- croplandFANTAYUMA11AC



#2011 AG mask, matrix, mean, extract, 
#2011
ndviYUMA11 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2011"), full.names = TRUE, pattern = ".tif$")
ndviYUMA11
ndvistackYUMA11 <- stack(ndviYUMA11, bands=1)
croplandYUMA11 <- mask(ndvistackYUMA11, croplandFANTAYUMA11Active)
writeRaster(croplandYUMA11, "croplandYUMA11", format = "GTiff")


cropdataYUMA11 <- as.matrix(croplandYUMA11, mode="list")
ncell(! is.na(cropdataYUMA11[]))
naomitcropdataYUMA11 <- na.omit(cropdataYUMA11)
meancropYUMA11 <- colMeans(naomitcropdataYUMA11, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA11, file =  "updatemeanActivecropYUMA11")


gc()
removeTmpFiles()
rm(list=ls())



#YUMA Fanta
YUMAfanta2012<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2011NLCD/YUMA_2012_SD3_9_26.tif")
YUMAndvi2012<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2012/YUMA_NDVI2012305.tif")

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



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2011agYUMA <-  projectRaster(NLCD2011agYUMA, YUMAndvi2012)
YUMAfanta2012 <-  projectRaster(YUMAfanta2012, YUMAndvi2012)


#mask
croplandFANTAYUMA12AC <- mask(YUMAfanta2012, NLCD2011agYUMA)
croplandFANTAYUMA12FA <- mask(YUMAfanta2012, NLCD2011agYUMA)
plot(croplandFANTAYUMA12AC)



#Crop
croplandFANTAYUMA12AC[croplandFANTAYUMA12AC != 2] = NA
croplandFANTAYUMA12Active <- croplandFANTAYUMA12AC



#2012 AG mask, matrix, mean, extract, 
#2012
ndviYUMA12 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2012"), full.names = TRUE, pattern = ".tif$")
ndviYUMA12
ndvistackYUMA12 <- stack(ndviYUMA12, bands=1)
croplandYUMA12 <- mask(ndvistackYUMA12, croplandFANTAYUMA12Active)
writeRaster(croplandYUMA12, "croplandYUMA12", format = "GTiff")


cropdataYUMA12 <- as.matrix(croplandYUMA12, mode="list")
ncell(! is.na(cropdataYUMA12[]))
naomitcropdataYUMA12 <- na.omit(cropdataYUMA12)
meancropYUMA12 <- colMeans(naomitcropdataYUMA12, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA12, file =  "updatemeanActivecropYUMA12")

gc()
removeTmpFiles()
rm(list=ls())

#YUMA Fanta
YUMAfanta2013<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2011NLCD/YUMA_2013_SD3_1_26_19.tif")
YUMAndvi2013<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2013/YUMA_NDVI2013305.tif")

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



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2011agYUMA <-  projectRaster(NLCD2011agYUMA, YUMAndvi2013)
YUMAfanta2013 <-  projectRaster(YUMAfanta2013, YUMAndvi2013)


#mask
croplandFANTAYUMA13AC <- mask(YUMAfanta2013, NLCD2011agYUMA)
croplandFANTAYUMA13FA <- mask(YUMAfanta2013, NLCD2011agYUMA)
plot(croplandFANTAYUMA13AC)



#Crop
croplandFANTAYUMA13AC[croplandFANTAYUMA13AC != 2] = NA
croplandFANTAYUMA13Active <- croplandFANTAYUMA13AC



#2013 AG mask, matrix, mean, extract, 
#2013
ndviYUMA13 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2013"), full.names = TRUE, pattern = ".tif$")
ndviYUMA13
ndvistackYUMA13 <- stack(ndviYUMA13, bands=1)
croplandYUMA13 <- mask(ndvistackYUMA13, croplandFANTAYUMA13Active)
writeRaster(croplandYUMA13, "croplandYUMA13", format = "GTiff")


cropdataYUMA13 <- as.matrix(croplandYUMA13, mode="list")
ncell(! is.na(cropdataYUMA13[]))
naomitcropdataYUMA13 <- na.omit(cropdataYUMA13)
meancropYUMA13 <- colMeans(naomitcropdataYUMA13, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA13, file =  "updatemeanActivecropYUMA13")


gc()
removeTmpFiles()
rm(list=ls())

#YUMA Fanta
YUMAfanta2014<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2011NLCD/YUMA_2014_SD3_1_26_19.tif")
YUMAndvi2014<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2014/YUMA_NDVI2014305.tif")

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



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2011agYUMA <-  projectRaster(NLCD2011agYUMA, YUMAndvi2014)
YUMAfanta2014 <-  projectRaster(YUMAfanta2014, YUMAndvi2014)


#mask
croplandFANTAYUMA14AC <- mask(YUMAfanta2014, NLCD2011agYUMA)
croplandFANTAYUMA14FA <- mask(YUMAfanta2014, NLCD2011agYUMA)
plot(croplandFANTAYUMA14AC)



#Crop
croplandFANTAYUMA14AC[croplandFANTAYUMA14AC != 2] = NA
croplandFANTAYUMA14Active <- croplandFANTAYUMA14AC



#2014 AG mask, matrix, mean, extract, 
#2014
ndviYUMA14 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2014"), full.names = TRUE, pattern = ".tif$")
ndviYUMA14
ndvistackYUMA14 <- stack(ndviYUMA14, bands=1)
croplandYUMA14 <- mask(ndvistackYUMA14, croplandFANTAYUMA14Active)
writeRaster(croplandYUMA14, "croplandYUMA14", format = "GTiff")


cropdataYUMA14 <- as.matrix(croplandYUMA14, mode="list")
ncell(! is.na(cropdataYUMA14[]))
naomitcropdataYUMA14 <- na.omit(cropdataYUMA14)
meancropYUMA14 <- colMeans(naomitcropdataYUMA14, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA14, file =  "updatemeanActivecropYUMA14")


gc()
removeTmpFiles()
rm(list=ls())

#YUMA Fanta
YUMAfanta2015<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2011NLCD/YUMA_2015_SD3_1_26_19.tif")
YUMAndvi2015<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2015/YUMA_NDVI2015305.tif")

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



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2011agYUMA <-  projectRaster(NLCD2011agYUMA, YUMAndvi2015)
YUMAfanta2015 <-  projectRaster(YUMAfanta2015, YUMAndvi2015)


#mask
croplandFANTAYUMA15AC <- mask(YUMAfanta2015, NLCD2011agYUMA)
croplandFANTAYUMA15FA <- mask(YUMAfanta2015, NLCD2011agYUMA)
plot(croplandFANTAYUMA15AC)


#Crop
croplandFANTAYUMA15AC[croplandFANTAYUMA15AC != 2] = NA
croplandFANTAYUMA15Active <- croplandFANTAYUMA15AC



#2015 AG mask, matrix, mean, extract, 
#2015
ndviYUMA15 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2015"), full.names = TRUE, pattern = ".tif$")
ndviYUMA15
ndvistackYUMA15 <- stack(ndviYUMA15, bands=1)
croplandYUMA15 <- mask(ndvistackYUMA15, croplandFANTAYUMA15Active)
writeRaster(croplandYUMA15, "croplandYUMA15", format = "GTiff")


cropdataYUMA15 <- as.matrix(croplandYUMA15, mode="list")
ncell(! is.na(cropdataYUMA15[]))
naomitcropdataYUMA15 <- na.omit(cropdataYUMA15)
meancropYUMA15 <- colMeans(naomitcropdataYUMA15, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA15, file =  "updatemeanActivecropYUMA15")




gc()
removeTmpFiles()
rm(list=ls())

#YUMA Fanta
YUMAfanta2016<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2011NLCD/YUMA_2016_SD3_1_26_19.tif")
YUMAndvi2016<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2016/YUMA_NDVI2016305.tif")

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



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2011agYUMA <-  projectRaster(NLCD2011agYUMA, YUMAndvi2016)
YUMAfanta2016 <-  projectRaster(YUMAfanta2016, YUMAndvi2016)


#mask
croplandFANTAYUMA16AC <- mask(YUMAfanta2016, NLCD2011agYUMA)
croplandFANTAYUMA16FA <- mask(YUMAfanta2016, NLCD2011agYUMA)
plot(croplandFANTAYUMA16AC)



#Crop
croplandFANTAYUMA16AC[croplandFANTAYUMA16AC != 2] = NA
croplandFANTAYUMA16Active <- croplandFANTAYUMA16AC



#2016 AG mask, matrix, mean, extract, 
#2016
ndviYUMA16 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2016"), full.names = TRUE, pattern = ".tif$")
ndviYUMA16
ndvistackYUMA16 <- stack(ndviYUMA16, bands=1)
croplandYUMA16 <- mask(ndvistackYUMA16, croplandFANTAYUMA16Active)
writeRaster(croplandYUMA16, "croplandYUMA16", format = "GTiff")


cropdataYUMA16 <- as.matrix(croplandYUMA16, mode="list")
ncell(! is.na(cropdataYUMA16[]))
naomitcropdataYUMA16 <- na.omit(cropdataYUMA16)
meancropYUMA16 <- colMeans(naomitcropdataYUMA16, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA16, file =  "updatemeanActivecropYUMA16")



gc()
removeTmpFiles()
rm(list=ls())


#YUMA Fanta
YUMAfanta2017<- raster("U:/Land Cover Change/Senior Water Rights/FANTA/AZ_3SD_8_12/YUMA_FANTA_SD3_2019//fanta_yuma_2011NLCD/YUMA_2017_SD3_9_26.tif")
YUMAndvi2017<- raster("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2017/YUMA_NDVI2017305.tif")

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



#Selecting Field
NLCD2001agYUMA[NLCD2001agYUMA != 82] = NA
plot(NLCD2001agYUMA)


NLCD2006agYUMA[NLCD2006agYUMA != 82] = NA
plot(NLCD2006agYUMA)

NLCD2011agYUMA[NLCD2011agYUMA != 82] = NA
plot(NLCD2011agYUMA)

#ProjectRaster
NLCD2011agYUMA <-  projectRaster(NLCD2011agYUMA, YUMAndvi2017)
YUMAfanta2017 <-  projectRaster(YUMAfanta2017, YUMAndvi2017)


#mask
croplandFANTAYUMA17AC <- mask(YUMAfanta2017, NLCD2011agYUMA)
croplandFANTAYUMA17FA <- mask(YUMAfanta2017, NLCD2011agYUMA)
plot(croplandFANTAYUMA17AC)


#Crop
croplandFANTAYUMA17AC[croplandFANTAYUMA17AC != 2] = NA
croplandFANTAYUMA17Active <- croplandFANTAYUMA17AC



#2017 AG mask, matrix, mean, extract, 
#2017
ndviYUMA17 <- list.files(("U:/Land Cover Change/Senior Water Rights/NDVI/YUMA_NDVI/2017"), full.names = TRUE, pattern = ".tif$")
ndviYUMA17
ndvistackYUMA17 <- stack(ndviYUMA17, bands=1)
croplandYUMA17 <- mask(ndvistackYUMA17, croplandFANTAYUMA17Active)
writeRaster(croplandYUMA17, "croplandYUMA17", format = "GTiff")

cropdataYUMA17 <- as.matrix(croplandYUMA17, mode="list")
ncell(! is.na(cropdataYUMA17[]))
naomitcropdataYUMA17 <- na.omit(cropdataYUMA17)
meancropYUMA17 <- colMeans(naomitcropdataYUMA17, na.rm = FALSE, dims = 1)

write.csv(meancropYUMA17, file =  "updatemeanActivecropYUMA17")



#MERGE

multMerge = function(mypath){
  filenames = list.files(path = mypath, full.names = TRUE)
  datalist = lapply(filenames, 
                    function(x){read.csv(file = x,
                                         header = TRUE,
                                         stringsAsFactors = FALSE)})
  Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)
}



phx2001_2017activeareaNDVI1january2019 <- multMerge("U:/Land Cover Change/Senior Water Rights/rcsv/NDVI_FANTA/phx")
yuma2001_2017activeareaNDVIjanuary2019  <- multMerge("U:/Land Cover Change/Senior Water Rights/rcsv/NDVI_FANTA/yuma")


write.csv(phx2001_2017activeareaNDVIjanuary2019 , file = "phx2001_2017activeareaNDVIjanuary2019 ")
write.csv(yuma2001_2017activeareaNDVIjanuary2019 , file = "yuma2001_2017activeareaNDVIjanuary2019 ")
