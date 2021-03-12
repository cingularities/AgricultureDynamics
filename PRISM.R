##Prism Data

library(raster)
library(rgdal)
library(ggplot2)


setwd('F:/MS Research Project 2020 EDITS/rcsv')
#Load Reference Classification


gc()
removeTmpFiles()
rm(list=ls())




#PHOENIX
#NLCD 2001
NLCD2001agPHX <-raster("F:/MS Research Project 2020 EDITS/2019 MS Thesis/NDVIRasters2018/SPATIALEXPLICITDATASETS/phxfanta/activeFANTAPHX01updatefinal.tif")
NLCD2001agPHX



extent(NLCD2001agPHX) <- extent(-113.639551572364,-110.621659441279, 31.9540370268944,34.1419509928867)

#PHX PPT
pptPHX <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/ppt"), full.names = TRUE, pattern = ".tif$")
pptPHX
pptstackPHX <- stack(pptPHX, bands=1)
crs(NLCD2001agPHX) <- crs(pptstackPHX)
croplandPHX <- crop(pptstackPHX, NLCD2001agPHX)
cropdataPHX <- as.matrix(croplandPHX, mode="list")
ncell(! is.na(cropdataPHX[]))
naomitcropdataPHX <- na.omit(cropdataPHX)
meancropPHX <- colMeans(naomitcropdataPHX, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropPHX, file =  "PHX_ppt.csv")


#YUMA
#NLCD 2001
NLCD2001agYUMA <-raster("F:/MS Research Project 2020 EDITS/2019 MS Thesis/NDVIRasters2018/SPATIALEXPLICITDATASETS/yumafanta/activeFANTAyuma01updatefinal.tif")
NLCD2001agYUMA


extent(NLCD2001agYUMA) <- extent(-115.214700203667,-113.346211759683, 32.190019229954,33.2907950989951)

#YUMA PPT
pptYUMA <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/ppt"), full.names = TRUE, pattern = ".tif$")
pptYUMA
pptstackYUMA <- stack(pptYUMA, bands=1)
crs(NLCD2001agYUMA) <- crs(pptstackYUMA)
croplandYUMA <- crop(pptstackYUMA, NLCD2001agYUMA)
cropdataYUMA <- as.matrix(croplandYUMA, mode="list")
ncell(! is.na(cropdataYUMA[]))
naomitcropdataYUMA <- na.omit(cropdataYUMA)
meancropYUMA <- colMeans(naomitcropdataYUMA, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropYUMA, file =  "YUMA_ppt.csv")

#PHX tmax
tmaxPHX <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/tmax"), full.names = TRUE, pattern = ".tif$")
tmaxPHX
tmaxstackPHX <- stack(tmaxPHX, bands=1)
crs(NLCD2001agPHX) <- crs(tmaxstackPHX)
croplandPHX <- crop(tmaxstackPHX, NLCD2001agPHX)
cropdataPHX <- as.matrix(croplandPHX, mode="list")
ncell(! is.na(cropdataPHX[]))
naomitcropdataPHX <- na.omit(cropdataPHX)
meancropPHX <- colMeans(naomitcropdataPHX, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropPHX, file =  "PHX_tmax.csv")


#YUMA tmax
tmaxYUMA <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/tmax"), full.names = TRUE, pattern = ".tif$")
tmaxYUMA
tmaxstackYUMA <- stack(tmaxYUMA, bands=1)
crs(NLCD2001agYUMA) <- crs(tmaxstackYUMA)
croplandYUMA <- crop(tmaxstackYUMA, NLCD2001agYUMA)
cropdataYUMA <- as.matrix(croplandYUMA, mode="list")
ncell(! is.na(cropdataYUMA[]))
naomitcropdataYUMA <- na.omit(cropdataYUMA)
meancropYUMA <- colMeans(naomitcropdataYUMA, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropYUMA, file =  "YUMA_tmax.csv")



#PHX tmin
tminPHX <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/tmin"), full.names = TRUE, pattern = ".tif$")
tminPHX
tminstackPHX <- stack(tminPHX, bands=1)
crs(NLCD2001agPHX) <- crs(tminstackPHX)
croplandPHX <- crop(tminstackPHX, NLCD2001agPHX)
cropdataPHX <- as.matrix(croplandPHX, mode="list")
ncell(! is.na(cropdataPHX[]))
naomitcropdataPHX <- na.omit(cropdataPHX)
meancropPHX <- colMeans(naomitcropdataPHX, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropPHX, file =  "PHX_tmin.csv")



#YUMA tmin
tminYUMA <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/tmin"), full.names = TRUE, pattern = ".tif$")
tminYUMA
tminstackYUMA <- stack(tminYUMA, bands=1)
crs(NLCD2001agYUMA) <- crs(tminstackYUMA)
croplandYUMA <- crop(tminstackYUMA, NLCD2001agYUMA)
cropdataYUMA <- as.matrix(croplandYUMA, mode="list")
ncell(! is.na(cropdataYUMA[]))
naomitcropdataYUMA <- na.omit(cropdataYUMA)
meancropYUMA <- colMeans(naomitcropdataYUMA, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropYUMA, file =  "YUMA_tmin.csv")


#PHX pet
petPHX <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/pet"), full.names = TRUE, pattern = ".tif$")
petPHX
petstackPHX <- stack(petPHX, bands=1)
crs(NLCD2001agPHX) <- crs(petstackPHX)
croplandPHX <- crop(petstackPHX, NLCD2001agPHX)
cropdataPHX <- as.matrix(croplandPHX, mode="list")
ncell(! is.na(cropdataPHX[]))
naomitcropdataPHX <- na.omit(cropdataPHX)
meancropPHX <- colMeans(naomitcropdataPHX, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropPHX, file =  "PHX_pet.csv")



#YUMA pet
petYUMA <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/pet"), full.names = TRUE, pattern = ".tif$")
petYUMA
petstackYUMA <- stack(petYUMA, bands=1)
crs(NLCD2001agYUMA) <- crs(petstackYUMA)
croplandYUMA <- crop(petstackYUMA, NLCD2001agYUMA)
cropdataYUMA <- as.matrix(croplandYUMA, mode="list")
ncell(! is.na(cropdataYUMA[]))
naomitcropdataYUMA <- na.omit(cropdataYUMA)
meancropYUMA <- colMeans(naomitcropdataYUMA, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropYUMA, file =  "YUMA_pet.csv")

#PHX vpdmax
vpdmaxPHX <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/vpdmax"), full.names = TRUE, pattern = ".tif$")
vpdmaxPHX
vpdmaxstackPHX <- stack(vpdmaxPHX, bands=1)
crs(NLCD2001agPHX) <- crs(vpdmaxstackPHX)
croplandPHX <- crop(vpdmaxstackPHX, NLCD2001agPHX)
cropdataPHX <- as.matrix(croplandPHX, mode="list")
ncell(! is.na(cropdataPHX[]))
naomitcropdataPHX <- na.omit(cropdataPHX)
meancropPHX <- colMeans(naomitcropdataPHX, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropPHX, file =  "PHX_vpdmax.csv")


#YUMA vpdmax
vpdmaxYUMA <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/vpdmax"), full.names = TRUE, pattern = ".tif$")
vpdmaxYUMA
vpdmaxstackYUMA <- stack(vpdmaxYUMA, bands=1)
crs(NLCD2001agYUMA) <- crs(vpdmaxstackYUMA)
croplandYUMA <- crop(vpdmaxstackYUMA, NLCD2001agYUMA)
cropdataYUMA <- as.matrix(croplandYUMA, mode="list")
ncell(! is.na(cropdataYUMA[]))
naomitcropdataYUMA <- na.omit(cropdataYUMA)
meancropYUMA <- colMeans(naomitcropdataYUMA, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropYUMA, file =  "YUMA_vpdmax.csv")


#PHX vpdmin
vpdminPHX <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/vpdmin"), full.names = TRUE, pattern = ".tif$")
vpdminPHX
vpdminstackPHX <- stack(vpdminPHX, bands=1)
crs(NLCD2001agPHX) <- crs(vpdminstackPHX)
croplandPHX <- crop(vpdminstackPHX, NLCD2001agPHX)
cropdataPHX <- as.matrix(croplandPHX, mode="list")
ncell(! is.na(cropdataPHX[]))
naomitcropdataPHX <- na.omit(cropdataPHX)
meancropPHX <- colMeans(naomitcropdataPHX, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropPHX, file =  "PHX_vpdmin.csv")


#YUMA vpdmin
vpdminYUMA <- list.files(("F:/MS Research Project 2020 EDITS/PRISM/vpdmin"), full.names = TRUE, pattern = ".tif$")
vpdminYUMA
vpdminstackYUMA <- stack(vpdminYUMA, bands=1)
crs(NLCD2001agYUMA) <- crs(vpdminstackYUMA)
croplandYUMA <- crop(vpdminstackYUMA, NLCD2001agYUMA)
cropdataYUMA <- as.matrix(croplandYUMA, mode="list")
ncell(! is.na(cropdataYUMA[]))
naomitcropdataYUMA <- na.omit(cropdataYUMA)
meancropYUMA <- colMeans(naomitcropdataYUMA, na.rm = FALSE, dims = 1)

#Write to CSV
write.csv(meancropYUMA, file =  "YUMA_vpdmin.csv")

