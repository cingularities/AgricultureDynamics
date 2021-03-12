gc()
removeTmpFiles()
rm(list=ls())


library(raster)
library(rgdal)
library(ggplot2)
library(maptools)
library(dplyr)
library(pracma)

##ALL VARIABLES
yumacrop <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/yumaactive.csv")
phxcrop <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/phxactive.csv")

#MarketValue
#marketvalue <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/Market Value_ AZ Crop Production (USDA) - Sheet1.csv")



#differncepercentage
#yumadifference <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/yumadifferencepercentage.csv")
#phxdifference <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/phxdifferencepercentage.csv")

#yumadifferenceyearly <- aggregate(DifferencePercentage ~ Year, data=yumadifference, mean, na.rm=TRUE, na.action=NULL)
#phxdifferenceyearly <- aggregate(DifferencePercentage  ~ Year, data=phxdifference, mean, na.rm=TRUE, na.action=NULL)


yumagrowingseason <- filter(yumacrop, DOY >=  97  & DOY <=  273 )
phxgrowingseason <- filter(phxcrop, DOY >=  97  & DOY <=  273 )

yumacropalternate <- subset.data.frame(yumacrop, DOY >= 273  | DOY <= 97)



yumacropalternategrowing <- aggregate(NDVI ~ Year, data=yumacropalternate, mean)
yumacropgrowing <- aggregate(NDVI ~ Year, data=yumagrowingseason, mean)
phxcropgrowing <- aggregate(NDVI ~ Year, data=phxgrowingseason, mean)

yumacropyear <- aggregate(NDVI ~ Year, data=yumacrop, mean)
phxcropyear <- aggregate(NDVI ~ Year, data=phxcrop, mean)


#Climate Variables
yumaclimate <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/yumaclimate.csv")
phxclimate <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/phxclimate.csv")



##Annual Cool Season Precip and Temp Filter
#Precip
yumacool <- filter(yumaclimate, Month <=  4  | Month >=  9 )
phxcool <- filter(phxclimate, Month <=  4  | Month >=  9 )


##Aggregate
#Precip
yumaprecipcoolyearavg <- aggregate(ppt ~ Year, data=yumacool, sum, na.rm=TRUE, na.action=NULL)
phxprecipcoolyearavg <- aggregate(ppt ~ Year, data=phxcool, sum, na.rm=TRUE, na.action=NULL)

#Max Temp
yumatempcoolyearavg <- aggregate(tmax ~ Year, data=yumacool, mean, na.rm=TRUE, na.action=NULL)
phxtempcoolyearavg <- aggregate(tmax~ Year, data=phxcool, mean, na.rm=TRUE, na.action=NULL)


#PET
yumapetcoolyearavg <- aggregate(pet ~ Year, data=yumacool, mean, na.rm=TRUE, na.action=NULL)
phxpetcoolyearavg <- aggregate(pet ~ Year, data=phxcool, mean, na.rm=TRUE, na.action=NULL)


#Water Balance (ppt-pet)
yumaaicoolyearavg <- aggregate(ai ~ Year, data=yumacool, mean, na.rm=TRUE, na.action=NULL)
phxaicoolyearavg <- aggregate(ai ~ Year, data=phxcool, mean, na.rm=TRUE, na.action=NULL)

#Avg Temp
yumatempavgcoolyearavg <- aggregate(tavg ~ Year, data=yumacool, mean, na.rm=TRUE, na.action=NULL)
phxtempavgcoolyearavg <- aggregate(tavg~ Year, data=phxcool, mean, na.rm=TRUE, na.action=NULL)


#Avg vpd
yumavpdavgcoolyearavg <- aggregate(vpdavg ~ Year, data=yumacool, mean, na.rm=TRUE, na.action=NULL)
phxvpdavgcoolyearavg <- aggregate(vpdavg~ Year, data=phxcool, mean, na.rm=TRUE, na.action=NULL)


##Annual Warm Season Precip and Temp Filter
#Precip
yumawarm <- filter(yumaclimate, Month >=  5  & Month <=  10 )
phxwarm <- filter(phxclimate, Month >=  5  & Month <=  10 )


##Aggregate
#Precip
yumaprecipwarmyearavg <- aggregate(ppt ~ Year, data=yumawarm, sum, na.rm=TRUE, na.action=NULL)
phxprecipwarmyearavg <- aggregate(ppt ~ Year, data=phxwarm, sum, na.rm=TRUE, na.action=NULL)

#Temp
yumatempwarmyearavg <- aggregate(tmax ~ Year, data=yumawarm, mean, na.rm=TRUE, na.action=NULL)
phxtempwarmyearavg <- aggregate(tmax ~ Year, data=phxwarm, mean, na.rm=TRUE, na.action=NULL)

#PET
yumapetwarmyearavg <- aggregate(pet ~ Year, data=yumawarm, mean, na.rm=TRUE, na.action=NULL)
phxpetwarmyearavg <- aggregate(pet ~ Year, data=phxwarm, mean, na.rm=TRUE, na.action=NULL)


#Water Balance (ppt-pet)
yumaaiwarmyearavg <- aggregate(ai ~ Year, data=yumawarm, mean, na.rm=TRUE, na.action=NULL)
phxaiwarmyearavg <- aggregate(ai ~ Year, data=phxwarm, mean, na.rm=TRUE, na.action=NULL)

#Avg Temp
yumatempavgwarmyearavg <- aggregate(tavg ~ Year, data=yumawarm, mean, na.rm=TRUE, na.action=NULL)
phxtempavgwarmyearavg <- aggregate(tavg~ Year, data=phxwarm, mean, na.rm=TRUE, na.action=NULL)

#avg vpd
yumavpdavgwarmyearavg <- aggregate(vpdavg ~ Year, data=yumawarm, mean, na.rm=TRUE, na.action=NULL)
phxvpdavgwarmyearavg <- aggregate(vpdavg~ Year, data=phxwarm, mean, na.rm=TRUE, na.action=NULL)


##Annual  Precip and Temp Filter
#Precip

##Aggregate
#Precip
yumaprecipyearavg <- aggregate(ppt ~ Year, data=yumaclimate, sum, na.rm=TRUE, na.action=NULL)
phxprecipyearavg <- aggregate(ppt ~ Year, data=phxclimate, sum, na.rm=TRUE, na.action=NULL)

#Temp
yumatempyearavg <- aggregate(tmax ~ Year, data=yumaclimate, mean, na.rm=TRUE, na.action=NULL)
phxtempyearavg <- aggregate(tmax ~ Year, data=phxclimate, mean, na.rm=TRUE, na.action=NULL)

#PET
yumapetyearavg <- aggregate(pet ~ Year, data=yumaclimate, mean, na.rm=TRUE, na.action=NULL)
phxpetyearavg <- aggregate(pet ~ Year, data=phxclimate, mean, na.rm=TRUE, na.action=NULL)


#Water Balance (ppt-pet)
yumaaiyearavg <- aggregate(ai ~ Year, data=yumaclimate, mean, na.rm=TRUE, na.action=NULL)
phxaiyearavg <- aggregate(ai ~ Year, data=phxclimate, mean, na.rm=TRUE, na.action=NULL)

#Avg Temp
yumatempavgyearavg <- aggregate(tavg ~ Year, data=yumaclimate, mean, na.rm=TRUE, na.action=NULL)
phxtempavgyearavg <- aggregate(tavg~ Year, data=phxclimate, mean, na.rm=TRUE, na.action=NULL)



#Area
mhapercentage <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/mHapercentage.csv")

#market Value
highmarketvalue <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/highmarketvalue.csv")
lowmarketvalue <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/lowmarketvalue.csv")

#Final PLOTS

integratedvariables <- read.csv("D:/MS Research Project 2020 EDITS/rcsv/integratedvariables.csv")
mvdetrend <- detrend(integratedvariables$cropMV, tt = 'linear')
mvdetrenddf <- data.frame(mvdetrend)
mvdetrenddf
integratedvariables <- cbind(integratedvariables, mvdetrenddf)

getwd()


write.csv(integratedvariables, file = "integratedvariablesupdate.csv")
windows()
#LCRP
#Final PLOTS
#NDVI vs CLIMATE

par(mar = c(5, 5, 3, 5))
plot(integratedvariables$lcrpcropalternategrowing ~integratedvariables$lcrpcoolyearvpdavg,  ylab = "iNDVI",
     main = "", xlab = "Cool Season VPD",
     col = "blue",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 2)
abline(lm(integratedvariables$lcrpcropalternategrowing~integratedvariables$lcrpcoolyearvpdavg), lwd=4, lty=2,col = "black")

mod1 = lm(integratedvariables$lcrpcropalternategrowing~integratedvariables$lcrpcoolyearvpdavg )
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
legend('topleft', legend = rp, bty = 'n', cex=1.5, text.col = "blue")


title("LCRP", outer=F,cex.main = 3)

#ACTIVE VS CLIMATE
par(mar = c(5, 5, 3, 5))
plot(integratedvariables$Yuma.Extent ~ integratedvariables$lcrpcoolyearvpdavg, ylab = "Active Cropland",
     main = "", xlab = "Cool Season VPD",
     col = "blue",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 2)
abline(lm(integratedvariables$Yuma.Extent~integratedvariables$lcrpcoolyearvpdavg), lwd=4, lty=2,col = "black")

mod1 = lm(integratedvariables$Yuma.Extent~integratedvariables$lcrpcoolyearvpdavg)
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
legend('bottomleft', legend = rp, bty = 'n', cex=1.5, text.col = "blue")

title("LCRP", outer=F,cex.main = 3)






#PPAMA
#NDVI VS CLIMATE
par(mar = c(5, 5, 3, 5))
plot(integratedvariables$ppamacropgrowing~integratedvariables$ppamawarmyearaiavg  , ylab = "iNDVI",
     main = "", xlab = "Warm Season Aridity",
     col = "red",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 2)
abline(lm(integratedvariables$ppamacropgrowing ~ integratedvariables$ppamawarmyearaiavg), lwd=4, lty=2,col = "black")

mod1 = lm(integratedvariables$ppamacropgrowing ~ integratedvariables$ppamawarmyearaiavg)
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
legend('bottomright', legend = rp, bty = 'n', cex=2.0, text.col = "red" )

title("PPAMA", outer=F,cex.main = 2)



#ACTIVE VS CLIMATE
par(mar = c(5, 5, 3, 5))
plot(integratedvariables$Phx.Extent~integratedvariables$ppamacoolyearaiavg, ylab = "Active Cropland",
     main = "", xlab = "Cool Season Aridity",
     col = "blue",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 2)
abline(lm(integratedvariables$Phx.Extent ~ integratedvariables$ppamacoolyearaiavg), lwd=4, lty=2,col = "black")

mod1 = lm(integratedvariables$Phx.Extent ~ integratedvariables$ppamacoolyearaiavg)
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
legend('topleft', legend = rp, bty = 'n', cex=1.0, text.col = "blue" )


title("PPAMA", outer=F,cex.main = 2)









#MARKET VALUE NEW STATISTICS
##phoenix
#


##Extent
par(mar = c(5, 5, 3, 5))
plot(integratedvariables$Phx.Extent~ integratedvariables$ppamacoolyearaiavg*integratedvariables$cropMVDetrend, ylab = "Cool Season Aridity (P/PET)",
     main = "", xlab = "% Active Crop",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)
abline(lm(integratedvariables$Phx.Extent~ integratedvariables$ppamacoolyearaiavg*integratedvariables$cropMVDetrend), lwd=3, lty=2,col = "blue")


mod1 = lm(integratedvariables$Phx.Extent~ integratedvariables$ppamacoolyearaiavg*integratedvariables$cropMVDetrend)
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
legend('topleft', legend = rp, bty = 'n', cex=1.5, text.col = "blue")

title("PPAMA", outer=F,cex.main = 2)




#MARKET VALUE NEW STATISTICS
##YUMA
#NDVI
plot(highmarketvalue$lcrpcropalternategrowing, highmarketvalue$lcrpcoolyearvpdavg, ylab = "Cool Season VPD",
     main = "", xlab = "iNDVI",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
plot(lowmarketvalue$lcrpcropalternategrowing, lowmarketvalue$lcrpcoolyearvpdavg, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(lowmarketvalue$lcrpcoolyearvpdavg ~ lowmarketvalue$lcrpcropalternategrowing), lwd=3, lty=2,col = "blue")



mod1 = lm(lowmarketvalue$lcrpcoolyearvpdavg ~ lowmarketvalue$lcrpcropalternategrowing)
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
legend('topleft', legend = rp, bty = 'n', cex=1.5, text.col = "blue")

title("LCRP", outer=F,cex.main = 2)



##Extent
par(mar = c(5, 5, 3, 5))
plot(highmarketvalue$Yuma.Extent, highmarketvalue$lcrpcoolyearvpdavg, ylab = "Cool Season VPD",
     main = "", xlab = "Active Crop",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
plot(lowmarketvalue$Yuma.Exten, lowmarketvalue$lcrpcoolyearvpdavg, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(highmarketvalue$lcrpcoolyearvpdavg ~ highmarketvalue$Yuma.Exten), lwd=3, lty=2,col = "orange")


mod1 = lm(highmarketvalue$lcrpcoolyearvpdavg ~ highmarketvalue$Yuma.Extent)
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
legend('topleft', legend = rp, bty = 'n', cex=1.5, text.col = "orange")


title("LCRP", outer=F,cex.main = 2)











##Market Value

##phoenix
#NDVI

par(mar = c(5, 5, 3, 5))
plot(highmarketvalue$ppamacropgrowing ~ highmarketvalue$ppamawarmyearaiavg, ylab = "iNDVI",
     main = "", xlab = "Warm  Season Aridity (P/PET)",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
plot(lowmarketvalue$ppamacropgrowing~ lowmarketvalue$ppamawarmyearaiavg, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(lowmarketvalue$ppamacropgrowing~ lowmarketvalue$ppamawarmyearaiavg), lwd=3, lty=2,col = "blue")



mod1 = lm(lowmarketvalue$ppamacropgrowing~ lowmarketvalue$ppamawarmyearaiavg)
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
legend('topleft', legend = rp, bty = 'n', cex=1.5, text.col = "blue")

title("PPAMA", outer=F,cex.main = 2)





##Extent
par(mar = c(5, 5, 3, 5))
plot(highmarketvalue$Phx.Extent~ highmarketvalue$ppamacoolyearaiavg, ylab = "Active Cropland",
     main = "", xlab = "Cool Season Aridity (P/PET)",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
plot(lowmarketvalue$Phx.Extent~ lowmarketvalue$ppamacoolyearaiavg, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(lowmarketvalue$Phx.Extent~lowmarketvalue$ppamacoolyearaiavg), lwd=3, lty=2,col = "blue")


mod1 = lm(lowmarketvalue$Phx.Extent ~ lowmarketvalue$ppamacoolyearaiavg)
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
legend('topleft', legend = rp, bty = 'n', cex=1.5, text.col = "blue")

title("PPAMA", outer=F,cex.main = 2)







##YUMA
#NDVI
windows()
plot(highmarketvalue$lcrpcropalternategrowing ~ highmarketvalue$lcrpcoolyearvpdavg, ylab = "iNDVI",
     main = "", xlab = "Cool Season VPD",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
plot(lowmarketvalue$lcrpcropalternategrowing ~ lowmarketvalue$lcrpcoolyearvpdavg, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(lowmarketvalue$lcrpcropalternategrowing ~ lowmarketvalue$lcrpcoolyearvpdavg), lwd=3, lty=2,col = "blue")
abline(lm(highmarketvalue$lcrpcropalternategrowing ~ highmarketvalue$lcrpcoolyearvpdavg), lwd=3, lty=2,col = "orange")


mod1 = lm(lowmarketvalue$lcrpcropalternategrowing ~ lowmarketvalue$lcrpcoolyearvpdavg)
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
legend('topleft', legend = rp, bty = 'n', cex=1, text.col = "blue")

title("LCRP", outer=F,cex.main = 2)
mod1 = lm(highmarketvalue$lcrpcropalternategrowing ~ highmarketvalue$lcrpcoolyearvpdavg)
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
legend('bottomright', legend = rp, bty = 'n', cex=1.5, text.col = "orange")

title("LCRP", outer=F,cex.main = 2)


##Extent
par(mar = c(5, 5, 3, 5))
plot(highmarketvalue$Yuma.Extent ~ highmarketvalue$lcrpcoolyearvpdavg, ylab = "Active Cropland",
     main = "", xlab = "Cool Season VPD",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
plot(lowmarketvalue$Yuma.Extent ~ lowmarketvalue$lcrpcoolyearvpdavg, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(highmarketvalue$Yuma.Extent ~ highmarketvalue$lcrpcoolyearvpdavg), lwd=3, lty=2,col = "orange")


mod1 = lm(highmarketvalue$Yuma.Extent ~ highmarketvalue$lcrpcoolyearvpdavg)
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
legend('topright', legend = rp, bty = 'n', cex=1, text.col = "orange")


title("LCRP", outer=F,cex.main = 2)











setwd("D:/MS Research Project 2020 EDITS/rcsv")


#statistics
#PPAMA
#WARM
#NDVI
PPAMA_NDVI_warm_ai <- summary(lm(integratedvariables$ppamawarmyearaiavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_warm_ai_r2 = PPAMA_NDVI_warm_ai$adj.r.squared
PPAMA_NDVI_warm_ai_my.p = PPAMA_NDVI_warm_ai$coefficients[2,4]
PPAMA_NDVI_warm_ai_my.slope = PPAMA_NDVI_warm_ai$coefficients[2]
PPAMA_NDVI_warm_ai_df <- list(PPAMA_NDVI_warm_ai_r2,PPAMA_NDVI_warm_ai_my.p, PPAMA_NDVI_warm_ai_my.slope)
PPAMA_NDVI_warm_ai_data<- do.call(cbind, PPAMA_NDVI_warm_ai_df)
colnames(PPAMA_NDVI_warm_ai_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_warm_pet <- summary(lm(integratedvariables$ppamawarmyearpetavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_warm_pet_r2 = PPAMA_NDVI_warm_pet$adj.r.squared
PPAMA_NDVI_warm_pet_my.p = PPAMA_NDVI_warm_pet$coefficients[2,4]
PPAMA_NDVI_warm_pet_my.slope = PPAMA_NDVI_warm_pet$coefficients[2]
PPAMA_NDVI_warm_pet_df <- list(PPAMA_NDVI_warm_pet_r2,PPAMA_NDVI_warm_pet_my.p, PPAMA_NDVI_warm_pet_my.slope)
PPAMA_NDVI_warm_pet_data<- do.call(cbind, PPAMA_NDVI_warm_pet_df)
colnames(PPAMA_NDVI_warm_pet_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_warm_ppt <- summary(lm(integratedvariables$ppamawarmyearprecipavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_warm_ppt_r2 = PPAMA_NDVI_warm_ppt$adj.r.squared
PPAMA_NDVI_warm_ppt_my.p = PPAMA_NDVI_warm_ppt$coefficients[2,4]
PPAMA_NDVI_warm_ppt_my.slope = PPAMA_NDVI_warm_ppt$coefficients[2]
PPAMA_NDVI_warm_ppt_df <- list(PPAMA_NDVI_warm_ppt_r2,PPAMA_NDVI_warm_ppt_my.p, PPAMA_NDVI_warm_ppt_my.slope)
PPAMA_NDVI_warm_ppt_data<- do.call(cbind, PPAMA_NDVI_warm_ppt_df)
colnames(PPAMA_NDVI_warm_ppt_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_warm_vpd <- summary(lm(integratedvariables$ppamawarmyearvpdavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_warm_vpd_r2 = PPAMA_NDVI_warm_vpd$adj.r.squared
PPAMA_NDVI_warm_vpd_my.p = PPAMA_NDVI_warm_vpd$coefficients[2,4]
PPAMA_NDVI_warm_vpd_my.slope = PPAMA_NDVI_warm_vpd$coefficients[2]
PPAMA_NDVI_warm_vpd_df <- list(PPAMA_NDVI_warm_vpd_r2,PPAMA_NDVI_warm_vpd_my.p, PPAMA_NDVI_warm_vpd_my.slope)
PPAMA_NDVI_warm_vpd_data<- do.call(cbind, PPAMA_NDVI_warm_vpd_df)
colnames(PPAMA_NDVI_warm_vpd_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_warm_temp <- summary(lm(integratedvariables$ppamawarmyeartempavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_warm_temp_r2 = PPAMA_NDVI_warm_temp$adj.r.squared
PPAMA_NDVI_warm_temp_my.p = PPAMA_NDVI_warm_temp$coefficients[2,4]
PPAMA_NDVI_warm_temp_my.slope = PPAMA_NDVI_warm_temp$coefficients[2]
PPAMA_NDVI_warm_temp_df <- list(PPAMA_NDVI_warm_temp_r2,PPAMA_NDVI_warm_temp_my.p, PPAMA_NDVI_warm_temp_my.slope)
PPAMA_NDVI_warm_temp_data<- do.call(cbind, PPAMA_NDVI_warm_temp_df)
colnames(PPAMA_NDVI_warm_temp_data) <- c("r2", "p.value", "slope") 


#COOL
#NDVI
PPAMA_NDVI_cool_ai <- summary(lm(integratedvariables$ppamacoolyearaiavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_cool_ai_r2 = PPAMA_NDVI_cool_ai$adj.r.squared
PPAMA_NDVI_cool_ai_my.p = PPAMA_NDVI_cool_ai$coefficients[2,4]
PPAMA_NDVI_cool_ai_my.slope = PPAMA_NDVI_cool_ai$coefficients[2]
PPAMA_NDVI_cool_ai_df <- list(PPAMA_NDVI_cool_ai_r2,PPAMA_NDVI_cool_ai_my.p, PPAMA_NDVI_cool_ai_my.slope)
PPAMA_NDVI_cool_ai_data<- do.call(cbind, PPAMA_NDVI_cool_ai_df)
colnames(PPAMA_NDVI_cool_ai_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_cool_pet <- summary(lm(integratedvariables$ppamacoolyearpetavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_cool_pet_r2 = PPAMA_NDVI_cool_pet$adj.r.squared
PPAMA_NDVI_cool_pet_my.p = PPAMA_NDVI_cool_pet$coefficients[2,4]
PPAMA_NDVI_cool_pet_my.slope = PPAMA_NDVI_cool_pet$coefficients[2]
PPAMA_NDVI_cool_pet_df <- list(PPAMA_NDVI_cool_pet_r2,PPAMA_NDVI_cool_pet_my.p, PPAMA_NDVI_cool_pet_my.slope)
PPAMA_NDVI_cool_pet_data<- do.call(cbind, PPAMA_NDVI_cool_pet_df)
colnames(PPAMA_NDVI_cool_pet_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_cool_ppt <- summary(lm(integratedvariables$ppamacoolyearprecipavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_cool_ppt_r2 = PPAMA_NDVI_cool_ppt$adj.r.squared
PPAMA_NDVI_cool_ppt_my.p = PPAMA_NDVI_cool_ppt$coefficients[2,4]
PPAMA_NDVI_cool_ppt_my.slope = PPAMA_NDVI_cool_ppt$coefficients[2]
PPAMA_NDVI_cool_ppt_df <- list(PPAMA_NDVI_cool_ppt_r2,PPAMA_NDVI_cool_ppt_my.p, PPAMA_NDVI_cool_ppt_my.slope)
PPAMA_NDVI_cool_ppt_data<- do.call(cbind, PPAMA_NDVI_cool_ppt_df)
colnames(PPAMA_NDVI_cool_ppt_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_cool_vpd <- summary(lm(integratedvariables$ppamacoolyearvpdavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_cool_vpd_r2 = PPAMA_NDVI_cool_vpd$adj.r.squared
PPAMA_NDVI_cool_vpd_my.p = PPAMA_NDVI_cool_vpd$coefficients[2,4]
PPAMA_NDVI_cool_vpd_my.slope = PPAMA_NDVI_cool_vpd$coefficients[2]
PPAMA_NDVI_cool_vpd_df <- list(PPAMA_NDVI_cool_vpd_r2,PPAMA_NDVI_cool_vpd_my.p, PPAMA_NDVI_cool_vpd_my.slope)
PPAMA_NDVI_cool_vpd_data<- do.call(cbind, PPAMA_NDVI_cool_vpd_df)
colnames(PPAMA_NDVI_cool_vpd_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_cool_temp <- summary(lm(integratedvariables$ppamacoolyeartempavg ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_cool_temp_r2 = PPAMA_NDVI_cool_temp$adj.r.squared
PPAMA_NDVI_cool_temp_my.p = PPAMA_NDVI_cool_temp$coefficients[2,4]
PPAMA_NDVI_cool_temp_my.slope = PPAMA_NDVI_cool_temp$coefficients[2]
PPAMA_NDVI_cool_temp_df <- list(PPAMA_NDVI_cool_temp_r2,PPAMA_NDVI_cool_temp_my.p, PPAMA_NDVI_cool_temp_my.slope)
PPAMA_NDVI_cool_temp_data<- do.call(cbind, PPAMA_NDVI_cool_temp_df)
colnames(PPAMA_NDVI_cool_temp_data) <- c("r2", "p.value", "slope") 



#EXTENT
#WARM
#EXTENT
PPAMA_EXTENT_warm_ai <- summary(lm(integratedvariables$ppamawarmyearaiavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_warm_ai_r2 = PPAMA_EXTENT_warm_ai$adj.r.squared
PPAMA_EXTENT_warm_ai_my.p = PPAMA_EXTENT_warm_ai$coefficients[2,4]
PPAMA_EXTENT_warm_ai_my.slope = PPAMA_EXTENT_warm_ai$coefficients[2]
PPAMA_EXTENT_warm_ai_df <- list(PPAMA_EXTENT_warm_ai_r2,PPAMA_EXTENT_warm_ai_my.p, PPAMA_EXTENT_warm_ai_my.slope)
PPAMA_EXTENT_warm_ai_data<- do.call(cbind, PPAMA_EXTENT_warm_ai_df)
colnames(PPAMA_EXTENT_warm_ai_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_warm_pet <- summary(lm(integratedvariables$ppamawarmyearpetavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_warm_pet_r2 = PPAMA_EXTENT_warm_pet$adj.r.squared
PPAMA_EXTENT_warm_pet_my.p = PPAMA_EXTENT_warm_pet$coefficients[2,4]
PPAMA_EXTENT_warm_pet_my.slope = PPAMA_EXTENT_warm_pet$coefficients[2]
PPAMA_EXTENT_warm_pet_df <- list(PPAMA_EXTENT_warm_pet_r2,PPAMA_EXTENT_warm_pet_my.p, PPAMA_EXTENT_warm_pet_my.slope)
PPAMA_EXTENT_warm_pet_data<- do.call(cbind, PPAMA_EXTENT_warm_pet_df)
colnames(PPAMA_EXTENT_warm_pet_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_warm_ppt <- summary(lm(integratedvariables$ppamawarmyearprecipavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_warm_ppt_r2 = PPAMA_EXTENT_warm_ppt$adj.r.squared
PPAMA_EXTENT_warm_ppt_my.p = PPAMA_EXTENT_warm_ppt$coefficients[2,4]
PPAMA_EXTENT_warm_ppt_my.slope = PPAMA_EXTENT_warm_ppt$coefficients[2]
PPAMA_EXTENT_warm_ppt_df <- list(PPAMA_EXTENT_warm_ppt_r2,PPAMA_EXTENT_warm_ppt_my.p, PPAMA_EXTENT_warm_ppt_my.slope)
PPAMA_EXTENT_warm_ppt_data<- do.call(cbind, PPAMA_EXTENT_warm_ppt_df)
colnames(PPAMA_EXTENT_warm_ppt_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_warm_vpd <- summary(lm(integratedvariables$ppamawarmyearvpdavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_warm_vpd_r2 = PPAMA_EXTENT_warm_vpd$adj.r.squared
PPAMA_EXTENT_warm_vpd_my.p = PPAMA_EXTENT_warm_vpd$coefficients[2,4]
PPAMA_EXTENT_warm_vpd_my.slope = PPAMA_EXTENT_warm_vpd$coefficients[2]
PPAMA_EXTENT_warm_vpd_df <- list(PPAMA_EXTENT_warm_vpd_r2,PPAMA_EXTENT_warm_vpd_my.p, PPAMA_EXTENT_warm_vpd_my.slope)
PPAMA_EXTENT_warm_vpd_data<- do.call(cbind, PPAMA_EXTENT_warm_vpd_df)
colnames(PPAMA_EXTENT_warm_vpd_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_warm_temp <- summary(lm(integratedvariables$ppamawarmyeartempavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_warm_temp_r2 = PPAMA_EXTENT_warm_temp$adj.r.squared
PPAMA_EXTENT_warm_temp_my.p = PPAMA_EXTENT_warm_temp$coefficients[2,4]
PPAMA_EXTENT_warm_temp_my.slope = PPAMA_EXTENT_warm_temp$coefficients[2]
PPAMA_EXTENT_warm_temp_df <- list(PPAMA_EXTENT_warm_temp_r2,PPAMA_EXTENT_warm_temp_my.p, PPAMA_EXTENT_warm_temp_my.slope)
PPAMA_EXTENT_warm_temp_data<- do.call(cbind, PPAMA_EXTENT_warm_temp_df)
colnames(PPAMA_EXTENT_warm_temp_data) <- c("r2", "p.value", "slope") 


#COOL
PPAMA_EXTENT_cool_ai <- summary(lm(integratedvariables$ppamacoolyearaiavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_cool_ai_r2 = PPAMA_EXTENT_cool_ai$adj.r.squared
PPAMA_EXTENT_cool_ai_my.p = PPAMA_EXTENT_cool_ai$coefficients[2,4]
PPAMA_EXTENT_cool_ai_my.slope = PPAMA_EXTENT_cool_ai$coefficients[2]
PPAMA_EXTENT_cool_ai_df <- list(PPAMA_EXTENT_cool_ai_r2,PPAMA_EXTENT_cool_ai_my.p, PPAMA_EXTENT_cool_ai_my.slope)
PPAMA_EXTENT_cool_ai_data<- do.call(cbind, PPAMA_EXTENT_cool_ai_df)
colnames(PPAMA_EXTENT_cool_ai_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_cool_pet <- summary(lm(integratedvariables$ppamacoolyearpetavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_cool_pet_r2 = PPAMA_EXTENT_cool_pet$adj.r.squared
PPAMA_EXTENT_cool_pet_my.p = PPAMA_EXTENT_cool_pet$coefficients[2,4]
PPAMA_EXTENT_cool_pet_my.slope = PPAMA_EXTENT_cool_pet$coefficients[2]
PPAMA_EXTENT_cool_pet_df <- list(PPAMA_EXTENT_cool_pet_r2,PPAMA_EXTENT_cool_pet_my.p, PPAMA_EXTENT_cool_pet_my.slope)
PPAMA_EXTENT_cool_pet_data<- do.call(cbind, PPAMA_EXTENT_cool_pet_df)
colnames(PPAMA_EXTENT_cool_pet_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_cool_ppt <- summary(lm(integratedvariables$ppamacoolyearprecipavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_cool_ppt_r2 = PPAMA_EXTENT_cool_ppt$adj.r.squared
PPAMA_EXTENT_cool_ppt_my.p = PPAMA_EXTENT_cool_ppt$coefficients[2,4]
PPAMA_EXTENT_cool_ppt_my.slope = PPAMA_EXTENT_cool_ppt$coefficients[2]
PPAMA_EXTENT_cool_ppt_df <- list(PPAMA_EXTENT_cool_ppt_r2,PPAMA_EXTENT_cool_ppt_my.p, PPAMA_EXTENT_cool_ppt_my.slope)
PPAMA_EXTENT_cool_ppt_data<- do.call(cbind, PPAMA_EXTENT_cool_ppt_df)
colnames(PPAMA_EXTENT_cool_ppt_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_cool_vpd <- summary(lm(integratedvariables$ppamacoolyearvpdavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_cool_vpd_r2 = PPAMA_EXTENT_cool_vpd$adj.r.squared
PPAMA_EXTENT_cool_vpd_my.p = PPAMA_EXTENT_cool_vpd$coefficients[2,4]
PPAMA_EXTENT_cool_vpd_my.slope = PPAMA_EXTENT_cool_vpd$coefficients[2]
PPAMA_EXTENT_cool_vpd_df <- list(PPAMA_EXTENT_cool_vpd_r2,PPAMA_EXTENT_cool_vpd_my.p, PPAMA_EXTENT_cool_vpd_my.slope)
PPAMA_EXTENT_cool_vpd_data<- do.call(cbind, PPAMA_EXTENT_cool_vpd_df)
colnames(PPAMA_EXTENT_cool_vpd_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_cool_temp <- summary(lm(integratedvariables$ppamacoolyeartempavg ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_cool_temp_r2 = PPAMA_EXTENT_cool_temp$adj.r.squared
PPAMA_EXTENT_cool_temp_my.p = PPAMA_EXTENT_cool_temp$coefficients[2,4]
PPAMA_EXTENT_cool_temp_my.slope = PPAMA_EXTENT_cool_temp$coefficients[2]
PPAMA_EXTENT_cool_temp_df <- list(PPAMA_EXTENT_cool_temp_r2,PPAMA_EXTENT_cool_temp_my.p, PPAMA_EXTENT_cool_temp_my.slope)
PPAMA_EXTENT_cool_temp_data<- do.call(cbind, PPAMA_EXTENT_cool_temp_df)
colnames(PPAMA_EXTENT_cool_temp_data) <- c("r2", "p.value", "slope") 






#statistics
#LCRP
#WARM
#NDVI
LCRP_NDVI_warm_ai <- summary(lm(integratedvariables$lcrpwarmyearaiavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_warm_ai_r2 = LCRP_NDVI_warm_ai$adj.r.squared
LCRP_NDVI_warm_ai_my.p = LCRP_NDVI_warm_ai$coefficients[2,4]
LCRP_NDVI_warm_ai_my.slope = LCRP_NDVI_warm_ai$coefficients[2]
LCRP_NDVI_warm_ai_df <- list(LCRP_NDVI_warm_ai_r2,LCRP_NDVI_warm_ai_my.p, LCRP_NDVI_warm_ai_my.slope)
LCRP_NDVI_warm_ai_data<- do.call(cbind, LCRP_NDVI_warm_ai_df)
colnames(LCRP_NDVI_warm_ai_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_warm_pet <- summary(lm(integratedvariables$lcrpwarmyearpetavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_warm_pet_r2 = LCRP_NDVI_warm_pet$adj.r.squared
LCRP_NDVI_warm_pet_my.p = LCRP_NDVI_warm_pet$coefficients[2,4]
LCRP_NDVI_warm_pet_my.slope = LCRP_NDVI_warm_pet$coefficients[2]
LCRP_NDVI_warm_pet_df <- list(LCRP_NDVI_warm_pet_r2,LCRP_NDVI_warm_pet_my.p, LCRP_NDVI_warm_pet_my.slope)
LCRP_NDVI_warm_pet_data<- do.call(cbind, LCRP_NDVI_warm_pet_df)
colnames(LCRP_NDVI_warm_pet_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_warm_ppt <- summary(lm(integratedvariables$lcrpwarmyearprecipavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_warm_ppt_r2 = LCRP_NDVI_warm_ppt$adj.r.squared
LCRP_NDVI_warm_ppt_my.p = LCRP_NDVI_warm_ppt$coefficients[2,4]
LCRP_NDVI_warm_ppt_my.slope = LCRP_NDVI_warm_ppt$coefficients[2]
LCRP_NDVI_warm_ppt_df <- list(LCRP_NDVI_warm_ppt_r2,LCRP_NDVI_warm_ppt_my.p, LCRP_NDVI_warm_ppt_my.slope)
LCRP_NDVI_warm_ppt_data<- do.call(cbind, LCRP_NDVI_warm_ppt_df)
colnames(LCRP_NDVI_warm_ppt_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_warm_vpd <- summary(lm(integratedvariables$lcrpwarmyearvpdavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_warm_vpd_r2 = LCRP_NDVI_warm_vpd$adj.r.squared
LCRP_NDVI_warm_vpd_my.p = LCRP_NDVI_warm_vpd$coefficients[2,4]
LCRP_NDVI_warm_vpd_my.slope = LCRP_NDVI_warm_vpd$coefficients[2]
LCRP_NDVI_warm_vpd_df <- list(LCRP_NDVI_warm_vpd_r2,LCRP_NDVI_warm_vpd_my.p, LCRP_NDVI_warm_vpd_my.slope)
LCRP_NDVI_warm_vpd_data<- do.call(cbind, LCRP_NDVI_warm_vpd_df)
colnames(LCRP_NDVI_warm_vpd_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_warm_temp <- summary(lm(integratedvariables$lcrpwarmyeartempavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_warm_temp_r2 = LCRP_NDVI_warm_temp$adj.r.squared
LCRP_NDVI_warm_temp_my.p = LCRP_NDVI_warm_temp$coefficients[2,4]
LCRP_NDVI_warm_temp_my.slope = LCRP_NDVI_warm_temp$coefficients[2]
LCRP_NDVI_warm_temp_df <- list(LCRP_NDVI_warm_temp_r2,LCRP_NDVI_warm_temp_my.p, LCRP_NDVI_warm_temp_my.slope)
LCRP_NDVI_warm_temp_data<- do.call(cbind, LCRP_NDVI_warm_temp_df)
colnames(LCRP_NDVI_warm_temp_data) <- c("r2", "p.value", "slope") 


#COOL
#NDVI
LCRP_NDVI_cool_ai <- summary(lm(integratedvariables$lcrpcoolyearaiavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_cool_ai_r2 = LCRP_NDVI_cool_ai$adj.r.squared
LCRP_NDVI_cool_ai_my.p = LCRP_NDVI_cool_ai$coefficients[2,4]
LCRP_NDVI_cool_ai_my.slope = LCRP_NDVI_cool_ai$coefficients[2]
LCRP_NDVI_cool_ai_df <- list(LCRP_NDVI_cool_ai_r2,LCRP_NDVI_cool_ai_my.p, LCRP_NDVI_cool_ai_my.slope)
LCRP_NDVI_cool_ai_data<- do.call(cbind, LCRP_NDVI_cool_ai_df)
colnames(LCRP_NDVI_cool_ai_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_cool_pet <- summary(lm(integratedvariables$lcrpcoolyearpetavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_cool_pet_r2 = LCRP_NDVI_cool_pet$adj.r.squared
LCRP_NDVI_cool_pet_my.p = LCRP_NDVI_cool_pet$coefficients[2,4]
LCRP_NDVI_cool_pet_my.slope = LCRP_NDVI_cool_pet$coefficients[2]
LCRP_NDVI_cool_pet_df <- list(LCRP_NDVI_cool_pet_r2,LCRP_NDVI_cool_pet_my.p, LCRP_NDVI_cool_pet_my.slope)
LCRP_NDVI_cool_pet_data<- do.call(cbind, LCRP_NDVI_cool_pet_df)
colnames(LCRP_NDVI_cool_pet_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_cool_ppt <- summary(lm(integratedvariables$lcrpcoolyearprecipavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_cool_ppt_r2 = LCRP_NDVI_cool_ppt$adj.r.squared
LCRP_NDVI_cool_ppt_my.p = LCRP_NDVI_cool_ppt$coefficients[2,4]
LCRP_NDVI_cool_ppt_my.slope = LCRP_NDVI_cool_ppt$coefficients[2]
LCRP_NDVI_cool_ppt_df <- list(LCRP_NDVI_cool_ppt_r2,LCRP_NDVI_cool_ppt_my.p, LCRP_NDVI_cool_ppt_my.slope)
LCRP_NDVI_cool_ppt_data<- do.call(cbind, LCRP_NDVI_cool_ppt_df)
colnames(LCRP_NDVI_cool_ppt_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_cool_vpd <- summary(lm(integratedvariables$lcrpcoolyearvpdavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_cool_vpd_r2 = LCRP_NDVI_cool_vpd$adj.r.squared
LCRP_NDVI_cool_vpd_my.p = LCRP_NDVI_cool_vpd$coefficients[2,4]
LCRP_NDVI_cool_vpd_my.slope = LCRP_NDVI_cool_vpd$coefficients[2]
LCRP_NDVI_cool_vpd_df <- list(LCRP_NDVI_cool_vpd_r2,LCRP_NDVI_cool_vpd_my.p, LCRP_NDVI_cool_vpd_my.slope)
LCRP_NDVI_cool_vpd_data<- do.call(cbind, LCRP_NDVI_cool_vpd_df)
colnames(LCRP_NDVI_cool_vpd_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_cool_temp <- summary(lm(integratedvariables$lcrpcoolyeartempavg ~ integratedvariables$lcrpcropalternategrowing))
LCRP_NDVI_cool_temp_r2 = LCRP_NDVI_cool_temp$adj.r.squared
LCRP_NDVI_cool_temp_my.p = LCRP_NDVI_cool_temp$coefficients[2,4]
LCRP_NDVI_cool_temp_my.slope = LCRP_NDVI_cool_temp$coefficients[2]
LCRP_NDVI_cool_temp_df <- list(LCRP_NDVI_cool_temp_r2,LCRP_NDVI_cool_temp_my.p, LCRP_NDVI_cool_temp_my.slope)
LCRP_NDVI_cool_temp_data<- do.call(cbind, LCRP_NDVI_cool_temp_df)
colnames(LCRP_NDVI_cool_temp_data) <- c("r2", "p.value", "slope") 



#EXTENT
#WARM
#EXTENT
LCRP_EXTENT_warm_ai <- summary(lm(integratedvariables$lcrpwarmyearaiavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_warm_ai_r2 = LCRP_EXTENT_warm_ai$adj.r.squared
LCRP_EXTENT_warm_ai_my.p = LCRP_EXTENT_warm_ai$coefficients[2,4]
LCRP_EXTENT_warm_ai_my.slope = LCRP_EXTENT_warm_ai$coefficients[2]
LCRP_EXTENT_warm_ai_df <- list(LCRP_EXTENT_warm_ai_r2,LCRP_EXTENT_warm_ai_my.p, LCRP_EXTENT_warm_ai_my.slope)
LCRP_EXTENT_warm_ai_data<- do.call(cbind, LCRP_EXTENT_warm_ai_df)
colnames(LCRP_EXTENT_warm_ai_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_warm_pet <- summary(lm(integratedvariables$lcrpwarmyearpetavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_warm_pet_r2 = LCRP_EXTENT_warm_pet$adj.r.squared
LCRP_EXTENT_warm_pet_my.p = LCRP_EXTENT_warm_pet$coefficients[2,4]
LCRP_EXTENT_warm_pet_my.slope = LCRP_EXTENT_warm_pet$coefficients[2]
LCRP_EXTENT_warm_pet_df <- list(LCRP_EXTENT_warm_pet_r2,LCRP_EXTENT_warm_pet_my.p, LCRP_EXTENT_warm_pet_my.slope)
LCRP_EXTENT_warm_pet_data<- do.call(cbind, LCRP_EXTENT_warm_pet_df)
colnames(LCRP_EXTENT_warm_pet_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_warm_ppt <- summary(lm(integratedvariables$lcrpwarmyearprecipavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_warm_ppt_r2 = LCRP_EXTENT_warm_ppt$adj.r.squared
LCRP_EXTENT_warm_ppt_my.p = LCRP_EXTENT_warm_ppt$coefficients[2,4]
LCRP_EXTENT_warm_ppt_my.slope = LCRP_EXTENT_warm_ppt$coefficients[2]
LCRP_EXTENT_warm_ppt_df <- list(LCRP_EXTENT_warm_ppt_r2,LCRP_EXTENT_warm_ppt_my.p, LCRP_EXTENT_warm_ppt_my.slope)
LCRP_EXTENT_warm_ppt_data<- do.call(cbind, LCRP_EXTENT_warm_ppt_df)
colnames(LCRP_EXTENT_warm_ppt_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_warm_vpd <- summary(lm(integratedvariables$lcrpwarmyearvpdavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_warm_vpd_r2 = LCRP_EXTENT_warm_vpd$adj.r.squared
LCRP_EXTENT_warm_vpd_my.p = LCRP_EXTENT_warm_vpd$coefficients[2,4]
LCRP_EXTENT_warm_vpd_my.slope = LCRP_EXTENT_warm_vpd$coefficients[2]
LCRP_EXTENT_warm_vpd_df <- list(LCRP_EXTENT_warm_vpd_r2,LCRP_EXTENT_warm_vpd_my.p, LCRP_EXTENT_warm_vpd_my.slope)
LCRP_EXTENT_warm_vpd_data<- do.call(cbind, LCRP_EXTENT_warm_vpd_df)
colnames(LCRP_EXTENT_warm_vpd_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_warm_temp <- summary(lm(integratedvariables$lcrpwarmyeartempavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_warm_temp_r2 = LCRP_EXTENT_warm_temp$adj.r.squared
LCRP_EXTENT_warm_temp_my.p = LCRP_EXTENT_warm_temp$coefficients[2,4]
LCRP_EXTENT_warm_temp_my.slope = LCRP_EXTENT_warm_temp$coefficients[2]
LCRP_EXTENT_warm_temp_df <- list(LCRP_EXTENT_warm_temp_r2,LCRP_EXTENT_warm_temp_my.p, LCRP_EXTENT_warm_temp_my.slope)
LCRP_EXTENT_warm_temp_data<- do.call(cbind, LCRP_EXTENT_warm_temp_df)
colnames(LCRP_EXTENT_warm_temp_data) <- c("r2", "p.value", "slope") 


#COOL
LCRP_EXTENT_cool_ai <- summary(lm(integratedvariables$lcrpcoolyearaiavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_cool_ai_r2 = LCRP_EXTENT_cool_ai$adj.r.squared
LCRP_EXTENT_cool_ai_my.p = LCRP_EXTENT_cool_ai$coefficients[2,4]
LCRP_EXTENT_cool_ai_my.slope = LCRP_EXTENT_cool_ai$coefficients[2]
LCRP_EXTENT_cool_ai_df <- list(LCRP_EXTENT_cool_ai_r2,LCRP_EXTENT_cool_ai_my.p, LCRP_EXTENT_cool_ai_my.slope)
LCRP_EXTENT_cool_ai_data<- do.call(cbind, LCRP_EXTENT_cool_ai_df)
colnames(LCRP_EXTENT_cool_ai_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_cool_pet <- summary(lm(integratedvariables$lcrpcoolyearpetavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_cool_pet_r2 = LCRP_EXTENT_cool_pet$adj.r.squared
LCRP_EXTENT_cool_pet_my.p = LCRP_EXTENT_cool_pet$coefficients[2,4]
LCRP_EXTENT_cool_pet_my.slope = LCRP_EXTENT_cool_pet$coefficients[2]
LCRP_EXTENT_cool_pet_df <- list(LCRP_EXTENT_cool_pet_r2,LCRP_EXTENT_cool_pet_my.p, LCRP_EXTENT_cool_pet_my.slope)
LCRP_EXTENT_cool_pet_data<- do.call(cbind, LCRP_EXTENT_cool_pet_df)
colnames(LCRP_EXTENT_cool_pet_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_cool_ppt <- summary(lm(integratedvariables$lcrpcoolyearprecipavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_cool_ppt_r2 = LCRP_EXTENT_cool_ppt$adj.r.squared
LCRP_EXTENT_cool_ppt_my.p = LCRP_EXTENT_cool_ppt$coefficients[2,4]
LCRP_EXTENT_cool_ppt_my.slope = LCRP_EXTENT_cool_ppt$coefficients[2]
LCRP_EXTENT_cool_ppt_df <- list(LCRP_EXTENT_cool_ppt_r2,LCRP_EXTENT_cool_ppt_my.p, LCRP_EXTENT_cool_ppt_my.slope)
LCRP_EXTENT_cool_ppt_data<- do.call(cbind, LCRP_EXTENT_cool_ppt_df)
colnames(LCRP_EXTENT_cool_ppt_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_cool_vpd <- summary(lm(integratedvariables$lcrpcoolyearvpdavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_cool_vpd_r2 = LCRP_EXTENT_cool_vpd$adj.r.squared
LCRP_EXTENT_cool_vpd_my.p = LCRP_EXTENT_cool_vpd$coefficients[2,4]
LCRP_EXTENT_cool_vpd_my.slope = LCRP_EXTENT_cool_vpd$coefficients[2]
LCRP_EXTENT_cool_vpd_df <- list(LCRP_EXTENT_cool_vpd_r2,LCRP_EXTENT_cool_vpd_my.p, LCRP_EXTENT_cool_vpd_my.slope)
LCRP_EXTENT_cool_vpd_data<- do.call(cbind, LCRP_EXTENT_cool_vpd_df)
colnames(LCRP_EXTENT_cool_vpd_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_cool_temp <- summary(lm(integratedvariables$lcrpcoolyeartempavg ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_cool_temp_r2 = LCRP_EXTENT_cool_temp$adj.r.squared
LCRP_EXTENT_cool_temp_my.p = LCRP_EXTENT_cool_temp$coefficients[2,4]
LCRP_EXTENT_cool_temp_my.slope = LCRP_EXTENT_cool_temp$coefficients[2]
LCRP_EXTENT_cool_temp_df <- list(LCRP_EXTENT_cool_temp_r2,LCRP_EXTENT_cool_temp_my.p, LCRP_EXTENT_cool_temp_my.slope)
LCRP_EXTENT_cool_temp_data<- do.call(cbind, LCRP_EXTENT_cool_temp_df)
colnames(LCRP_EXTENT_cool_temp_data) <- c("r2", "p.value", "slope") 



#Market Value
#statistics
#PPAMA
#WARM
#HIGH
#NDVI
HIGH_PPAMA_NDVI_warm_ai <- summary(lm(highmarketvalue$ppamawarmyearaiavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_warm_ai_r2 = HIGH_PPAMA_NDVI_warm_ai$adj.r.squared
HIGH_PPAMA_NDVI_warm_ai_my.p = HIGH_PPAMA_NDVI_warm_ai$coefficients[2,4]
HIGH_PPAMA_NDVI_warm_ai_my.slope = HIGH_PPAMA_NDVI_warm_ai$coefficients[2]
HIGH_PPAMA_NDVI_warm_ai_df <- list(HIGH_PPAMA_NDVI_warm_ai_r2,HIGH_PPAMA_NDVI_warm_ai_my.p, HIGH_PPAMA_NDVI_warm_ai_my.slope)
HIGH_PPAMA_NDVI_warm_ai_data<- do.call(cbind, HIGH_PPAMA_NDVI_warm_ai_df)
colnames(HIGH_PPAMA_NDVI_warm_ai_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_NDVI_warm_pet <- summary(lm(highmarketvalue$ppamawarmyearpetavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_warm_pet_r2 = HIGH_PPAMA_NDVI_warm_pet$adj.r.squared
HIGH_PPAMA_NDVI_warm_pet_my.p = HIGH_PPAMA_NDVI_warm_pet$coefficients[2,4]
HIGH_PPAMA_NDVI_warm_pet_my.slope = HIGH_PPAMA_NDVI_warm_pet$coefficients[2]
HIGH_PPAMA_NDVI_warm_pet_df <- list(HIGH_PPAMA_NDVI_warm_pet_r2,HIGH_PPAMA_NDVI_warm_pet_my.p, HIGH_PPAMA_NDVI_warm_pet_my.slope)
HIGH_PPAMA_NDVI_warm_pet_data<- do.call(cbind, HIGH_PPAMA_NDVI_warm_pet_df)
colnames(HIGH_PPAMA_NDVI_warm_pet_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_NDVI_warm_ppt <- summary(lm(highmarketvalue$ppamawarmyearprecipavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_warm_ppt_r2 = HIGH_PPAMA_NDVI_warm_ppt$adj.r.squared
HIGH_PPAMA_NDVI_warm_ppt_my.p = HIGH_PPAMA_NDVI_warm_ppt$coefficients[2,4]
HIGH_PPAMA_NDVI_warm_ppt_my.slope = HIGH_PPAMA_NDVI_warm_ppt$coefficients[2]
HIGH_PPAMA_NDVI_warm_ppt_df <- list(HIGH_PPAMA_NDVI_warm_ppt_r2,HIGH_PPAMA_NDVI_warm_ppt_my.p, HIGH_PPAMA_NDVI_warm_ppt_my.slope)
HIGH_PPAMA_NDVI_warm_ppt_data<- do.call(cbind, HIGH_PPAMA_NDVI_warm_ppt_df)
colnames(HIGH_PPAMA_NDVI_warm_ppt_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_NDVI_warm_vpd <- summary(lm(highmarketvalue$ppamawarmyearvpdavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_warm_vpd_r2 = HIGH_PPAMA_NDVI_warm_vpd$adj.r.squared
HIGH_PPAMA_NDVI_warm_vpd_my.p = HIGH_PPAMA_NDVI_warm_vpd$coefficients[2,4]
HIGH_PPAMA_NDVI_warm_vpd_my.slope = HIGH_PPAMA_NDVI_warm_vpd$coefficients[2]
HIGH_PPAMA_NDVI_warm_vpd_df <- list(HIGH_PPAMA_NDVI_warm_vpd_r2,HIGH_PPAMA_NDVI_warm_vpd_my.p, HIGH_PPAMA_NDVI_warm_vpd_my.slope)
HIGH_PPAMA_NDVI_warm_vpd_data<- do.call(cbind, HIGH_PPAMA_NDVI_warm_vpd_df)
colnames(HIGH_PPAMA_NDVI_warm_vpd_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_NDVI_warm_temp <- summary(lm(highmarketvalue$ppamawarmyeartempavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_warm_temp_r2 = HIGH_PPAMA_NDVI_warm_temp$adj.r.squared
HIGH_PPAMA_NDVI_warm_temp_my.p = HIGH_PPAMA_NDVI_warm_temp$coefficients[2,4]
HIGH_PPAMA_NDVI_warm_temp_my.slope = HIGH_PPAMA_NDVI_warm_temp$coefficients[2]
HIGH_PPAMA_NDVI_warm_temp_df <- list(HIGH_PPAMA_NDVI_warm_temp_r2,HIGH_PPAMA_NDVI_warm_temp_my.p, HIGH_PPAMA_NDVI_warm_temp_my.slope)
HIGH_PPAMA_NDVI_warm_temp_data<- do.call(cbind, HIGH_PPAMA_NDVI_warm_temp_df)
colnames(HIGH_PPAMA_NDVI_warm_temp_data) <- c("r2", "p.value", "slope") 

#LOW
LOW_PPAMA_NDVI_warm_ai <- summary(lm(lowmarketvalue$ppamawarmyearaiavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_warm_ai_r2 = LOW_PPAMA_NDVI_warm_ai$adj.r.squared
LOW_PPAMA_NDVI_warm_ai_my.p = LOW_PPAMA_NDVI_warm_ai$coefficients[2,4]
LOW_PPAMA_NDVI_warm_ai_my.slope = LOW_PPAMA_NDVI_warm_ai$coefficients[2]
LOW_PPAMA_NDVI_warm_ai_df <- list(LOW_PPAMA_NDVI_warm_ai_r2,LOW_PPAMA_NDVI_warm_ai_my.p, LOW_PPAMA_NDVI_warm_ai_my.slope)
LOW_PPAMA_NDVI_warm_ai_data<- do.call(cbind, LOW_PPAMA_NDVI_warm_ai_df)
colnames(LOW_PPAMA_NDVI_warm_ai_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_NDVI_warm_pet <- summary(lm(lowmarketvalue$ppamawarmyearpetavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_warm_pet_r2 = LOW_PPAMA_NDVI_warm_pet$adj.r.squared
LOW_PPAMA_NDVI_warm_pet_my.p = LOW_PPAMA_NDVI_warm_pet$coefficients[2,4]
LOW_PPAMA_NDVI_warm_pet_my.slope = LOW_PPAMA_NDVI_warm_pet$coefficients[2]
LOW_PPAMA_NDVI_warm_pet_df <- list(LOW_PPAMA_NDVI_warm_pet_r2,LOW_PPAMA_NDVI_warm_pet_my.p, LOW_PPAMA_NDVI_warm_pet_my.slope)
LOW_PPAMA_NDVI_warm_pet_data<- do.call(cbind, LOW_PPAMA_NDVI_warm_pet_df)
colnames(LOW_PPAMA_NDVI_warm_pet_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_NDVI_warm_ppt <- summary(lm(lowmarketvalue$ppamawarmyearprecipavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_warm_ppt_r2 = LOW_PPAMA_NDVI_warm_ppt$adj.r.squared
LOW_PPAMA_NDVI_warm_ppt_my.p = LOW_PPAMA_NDVI_warm_ppt$coefficients[2,4]
LOW_PPAMA_NDVI_warm_ppt_my.slope = LOW_PPAMA_NDVI_warm_ppt$coefficients[2]
LOW_PPAMA_NDVI_warm_ppt_df <- list(LOW_PPAMA_NDVI_warm_ppt_r2,LOW_PPAMA_NDVI_warm_ppt_my.p, LOW_PPAMA_NDVI_warm_ppt_my.slope)
LOW_PPAMA_NDVI_warm_ppt_data<- do.call(cbind, LOW_PPAMA_NDVI_warm_ppt_df)
colnames(LOW_PPAMA_NDVI_warm_ppt_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_NDVI_warm_vpd <- summary(lm(lowmarketvalue$ppamawarmyearvpdavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_warm_vpd_r2 = LOW_PPAMA_NDVI_warm_vpd$adj.r.squared
LOW_PPAMA_NDVI_warm_vpd_my.p = LOW_PPAMA_NDVI_warm_vpd$coefficients[2,4]
LOW_PPAMA_NDVI_warm_vpd_my.slope = LOW_PPAMA_NDVI_warm_vpd$coefficients[2]
LOW_PPAMA_NDVI_warm_vpd_df <- list(LOW_PPAMA_NDVI_warm_vpd_r2,LOW_PPAMA_NDVI_warm_vpd_my.p, LOW_PPAMA_NDVI_warm_vpd_my.slope)
LOW_PPAMA_NDVI_warm_vpd_data<- do.call(cbind, LOW_PPAMA_NDVI_warm_vpd_df)
colnames(LOW_PPAMA_NDVI_warm_vpd_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_NDVI_warm_temp <- summary(lm(lowmarketvalue$ppamawarmyeartempavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_warm_temp_r2 = LOW_PPAMA_NDVI_warm_temp$adj.r.squared
LOW_PPAMA_NDVI_warm_temp_my.p = LOW_PPAMA_NDVI_warm_temp$coefficients[2,4]
LOW_PPAMA_NDVI_warm_temp_my.slope = LOW_PPAMA_NDVI_warm_temp$coefficients[2]
LOW_PPAMA_NDVI_warm_temp_df <- list(LOW_PPAMA_NDVI_warm_temp_r2,LOW_PPAMA_NDVI_warm_temp_my.p, LOW_PPAMA_NDVI_warm_temp_my.slope)
LOW_PPAMA_NDVI_warm_temp_data<- do.call(cbind, LOW_PPAMA_NDVI_warm_temp_df)
colnames(LOW_PPAMA_NDVI_warm_temp_data) <- c("r2", "p.value", "slope") 

#WARM
#HIGH
#EXTENT
HIGH_PPAMA_EXTENT_warm_ai <- summary(lm(highmarketvalue$ppamawarmyearaiavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_warm_ai_r2 = HIGH_PPAMA_EXTENT_warm_ai$adj.r.squared
HIGH_PPAMA_EXTENT_warm_ai_my.p = HIGH_PPAMA_EXTENT_warm_ai$coefficients[2,4]
HIGH_PPAMA_EXTENT_warm_ai_my.slope = HIGH_PPAMA_EXTENT_warm_ai$coefficients[2]
HIGH_PPAMA_EXTENT_warm_ai_df <- list(HIGH_PPAMA_EXTENT_warm_ai_r2,HIGH_PPAMA_EXTENT_warm_ai_my.p, HIGH_PPAMA_EXTENT_warm_ai_my.slope)
HIGH_PPAMA_EXTENT_warm_ai_data<- do.call(cbind, HIGH_PPAMA_EXTENT_warm_ai_df)
colnames(HIGH_PPAMA_EXTENT_warm_ai_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_EXTENT_warm_pet <- summary(lm(highmarketvalue$ppamawarmyearpetavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_warm_pet_r2 = HIGH_PPAMA_EXTENT_warm_pet$adj.r.squared
HIGH_PPAMA_EXTENT_warm_pet_my.p = HIGH_PPAMA_EXTENT_warm_pet$coefficients[2,4]
HIGH_PPAMA_EXTENT_warm_pet_my.slope = HIGH_PPAMA_EXTENT_warm_pet$coefficients[2]
HIGH_PPAMA_EXTENT_warm_pet_df <- list(HIGH_PPAMA_EXTENT_warm_pet_r2,HIGH_PPAMA_EXTENT_warm_pet_my.p, HIGH_PPAMA_EXTENT_warm_pet_my.slope)
HIGH_PPAMA_EXTENT_warm_pet_data<- do.call(cbind, HIGH_PPAMA_EXTENT_warm_pet_df)
colnames(HIGH_PPAMA_EXTENT_warm_pet_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_EXTENT_warm_ppt <- summary(lm(highmarketvalue$ppamawarmyearprecipavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_warm_ppt_r2 = HIGH_PPAMA_EXTENT_warm_ppt$adj.r.squared
HIGH_PPAMA_EXTENT_warm_ppt_my.p = HIGH_PPAMA_EXTENT_warm_ppt$coefficients[2,4]
HIGH_PPAMA_EXTENT_warm_ppt_my.slope = HIGH_PPAMA_EXTENT_warm_ppt$coefficients[2]
HIGH_PPAMA_EXTENT_warm_ppt_df <- list(HIGH_PPAMA_EXTENT_warm_ppt_r2,HIGH_PPAMA_EXTENT_warm_ppt_my.p, HIGH_PPAMA_EXTENT_warm_ppt_my.slope)
HIGH_PPAMA_EXTENT_warm_ppt_data<- do.call(cbind, HIGH_PPAMA_EXTENT_warm_ppt_df)
colnames(HIGH_PPAMA_EXTENT_warm_ppt_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_EXTENT_warm_vpd <- summary(lm(highmarketvalue$ppamawarmyearvpdavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_warm_vpd_r2 = HIGH_PPAMA_EXTENT_warm_vpd$adj.r.squared
HIGH_PPAMA_EXTENT_warm_vpd_my.p = HIGH_PPAMA_EXTENT_warm_vpd$coefficients[2,4]
HIGH_PPAMA_EXTENT_warm_vpd_my.slope = HIGH_PPAMA_EXTENT_warm_vpd$coefficients[2]
HIGH_PPAMA_EXTENT_warm_vpd_df <- list(HIGH_PPAMA_EXTENT_warm_vpd_r2,HIGH_PPAMA_EXTENT_warm_vpd_my.p, HIGH_PPAMA_EXTENT_warm_vpd_my.slope)
HIGH_PPAMA_EXTENT_warm_vpd_data<- do.call(cbind, HIGH_PPAMA_EXTENT_warm_vpd_df)
colnames(HIGH_PPAMA_EXTENT_warm_vpd_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_EXTENT_warm_temp <- summary(lm(highmarketvalue$ppamawarmyeartempavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_warm_temp_r2 = HIGH_PPAMA_EXTENT_warm_temp$adj.r.squared
HIGH_PPAMA_EXTENT_warm_temp_my.p = HIGH_PPAMA_EXTENT_warm_temp$coefficients[2,4]
HIGH_PPAMA_EXTENT_warm_temp_my.slope = HIGH_PPAMA_EXTENT_warm_temp$coefficients[2]
HIGH_PPAMA_EXTENT_warm_temp_df <- list(HIGH_PPAMA_EXTENT_warm_temp_r2,HIGH_PPAMA_EXTENT_warm_temp_my.p, HIGH_PPAMA_EXTENT_warm_temp_my.slope)
HIGH_PPAMA_EXTENT_warm_temp_data<- do.call(cbind, HIGH_PPAMA_EXTENT_warm_temp_df)
colnames(HIGH_PPAMA_EXTENT_warm_temp_data) <- c("r2", "p.value", "slope") 

#LOW
LOW_PPAMA_EXTENT_warm_ai <- summary(lm(lowmarketvalue$ppamawarmyearaiavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_warm_ai_r2 = LOW_PPAMA_EXTENT_warm_ai$adj.r.squared
LOW_PPAMA_EXTENT_warm_ai_my.p = LOW_PPAMA_EXTENT_warm_ai$coefficients[2,4]
LOW_PPAMA_EXTENT_warm_ai_my.slope = LOW_PPAMA_EXTENT_warm_ai$coefficients[2]
LOW_PPAMA_EXTENT_warm_ai_df <- list(LOW_PPAMA_EXTENT_warm_ai_r2,LOW_PPAMA_EXTENT_warm_ai_my.p, LOW_PPAMA_EXTENT_warm_ai_my.slope)
LOW_PPAMA_EXTENT_warm_ai_data<- do.call(cbind, LOW_PPAMA_EXTENT_warm_ai_df)
colnames(LOW_PPAMA_EXTENT_warm_ai_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_EXTENT_warm_pet <- summary(lm(lowmarketvalue$ppamawarmyearpetavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_warm_pet_r2 = LOW_PPAMA_EXTENT_warm_pet$adj.r.squared
LOW_PPAMA_EXTENT_warm_pet_my.p = LOW_PPAMA_EXTENT_warm_pet$coefficients[2,4]
LOW_PPAMA_EXTENT_warm_pet_my.slope = LOW_PPAMA_EXTENT_warm_pet$coefficients[2]
LOW_PPAMA_EXTENT_warm_pet_df <- list(LOW_PPAMA_EXTENT_warm_pet_r2,LOW_PPAMA_EXTENT_warm_pet_my.p, LOW_PPAMA_EXTENT_warm_pet_my.slope)
LOW_PPAMA_EXTENT_warm_pet_data<- do.call(cbind, LOW_PPAMA_EXTENT_warm_pet_df)
colnames(LOW_PPAMA_EXTENT_warm_pet_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_EXTENT_warm_ppt <- summary(lm(lowmarketvalue$ppamawarmyearprecipavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_warm_ppt_r2 = LOW_PPAMA_EXTENT_warm_ppt$adj.r.squared
LOW_PPAMA_EXTENT_warm_ppt_my.p = LOW_PPAMA_EXTENT_warm_ppt$coefficients[2,4]
LOW_PPAMA_EXTENT_warm_ppt_my.slope = LOW_PPAMA_EXTENT_warm_ppt$coefficients[2]
LOW_PPAMA_EXTENT_warm_ppt_df <- list(LOW_PPAMA_EXTENT_warm_ppt_r2,LOW_PPAMA_EXTENT_warm_ppt_my.p, LOW_PPAMA_EXTENT_warm_ppt_my.slope)
LOW_PPAMA_EXTENT_warm_ppt_data<- do.call(cbind, LOW_PPAMA_EXTENT_warm_ppt_df)
colnames(LOW_PPAMA_EXTENT_warm_ppt_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_EXTENT_warm_vpd <- summary(lm(lowmarketvalue$ppamawarmyearvpdavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_warm_vpd_r2 = LOW_PPAMA_EXTENT_warm_vpd$adj.r.squared
LOW_PPAMA_EXTENT_warm_vpd_my.p = LOW_PPAMA_EXTENT_warm_vpd$coefficients[2,4]
LOW_PPAMA_EXTENT_warm_vpd_my.slope = LOW_PPAMA_EXTENT_warm_vpd$coefficients[2]
LOW_PPAMA_EXTENT_warm_vpd_df <- list(LOW_PPAMA_EXTENT_warm_vpd_r2,LOW_PPAMA_EXTENT_warm_vpd_my.p, LOW_PPAMA_EXTENT_warm_vpd_my.slope)
LOW_PPAMA_EXTENT_warm_vpd_data<- do.call(cbind, LOW_PPAMA_EXTENT_warm_vpd_df)
colnames(LOW_PPAMA_EXTENT_warm_vpd_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_EXTENT_warm_temp <- summary(lm(lowmarketvalue$ppamawarmyeartempavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_warm_temp_r2 = LOW_PPAMA_EXTENT_warm_temp$adj.r.squared
LOW_PPAMA_EXTENT_warm_temp_my.p = LOW_PPAMA_EXTENT_warm_temp$coefficients[2,4]
LOW_PPAMA_EXTENT_warm_temp_my.slope = LOW_PPAMA_EXTENT_warm_temp$coefficients[2]
LOW_PPAMA_EXTENT_warm_temp_df <- list(LOW_PPAMA_EXTENT_warm_temp_r2,LOW_PPAMA_EXTENT_warm_temp_my.p, LOW_PPAMA_EXTENT_warm_temp_my.slope)
LOW_PPAMA_EXTENT_warm_temp_data<- do.call(cbind, LOW_PPAMA_EXTENT_warm_temp_df)
colnames(LOW_PPAMA_EXTENT_warm_temp_data) <- c("r2", "p.value", "slope") 

#PPAMA
#cool
#HIGH
#NDVI
HIGH_PPAMA_NDVI_cool_ai <- summary(lm(highmarketvalue$ppamacoolyearaiavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_cool_ai_r2 = HIGH_PPAMA_NDVI_cool_ai$adj.r.squared
HIGH_PPAMA_NDVI_cool_ai_my.p = HIGH_PPAMA_NDVI_cool_ai$coefficients[2,4]
HIGH_PPAMA_NDVI_cool_ai_my.slope = HIGH_PPAMA_NDVI_cool_ai$coefficients[2]
HIGH_PPAMA_NDVI_cool_ai_df <- list(HIGH_PPAMA_NDVI_cool_ai_r2,HIGH_PPAMA_NDVI_cool_ai_my.p, HIGH_PPAMA_NDVI_cool_ai_my.slope)
HIGH_PPAMA_NDVI_cool_ai_data<- do.call(cbind, HIGH_PPAMA_NDVI_cool_ai_df)
colnames(HIGH_PPAMA_NDVI_cool_ai_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_NDVI_cool_pet <- summary(lm(highmarketvalue$ppamacoolyearpetavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_cool_pet_r2 = HIGH_PPAMA_NDVI_cool_pet$adj.r.squared
HIGH_PPAMA_NDVI_cool_pet_my.p = HIGH_PPAMA_NDVI_cool_pet$coefficients[2,4]
HIGH_PPAMA_NDVI_cool_pet_my.slope = HIGH_PPAMA_NDVI_cool_pet$coefficients[2]
HIGH_PPAMA_NDVI_cool_pet_df <- list(HIGH_PPAMA_NDVI_cool_pet_r2,HIGH_PPAMA_NDVI_cool_pet_my.p, HIGH_PPAMA_NDVI_cool_pet_my.slope)
HIGH_PPAMA_NDVI_cool_pet_data<- do.call(cbind, HIGH_PPAMA_NDVI_cool_pet_df)
colnames(HIGH_PPAMA_NDVI_cool_pet_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_NDVI_cool_ppt <- summary(lm(highmarketvalue$ppamacoolyearprecipavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_cool_ppt_r2 = HIGH_PPAMA_NDVI_cool_ppt$adj.r.squared
HIGH_PPAMA_NDVI_cool_ppt_my.p = HIGH_PPAMA_NDVI_cool_ppt$coefficients[2,4]
HIGH_PPAMA_NDVI_cool_ppt_my.slope = HIGH_PPAMA_NDVI_cool_ppt$coefficients[2]
HIGH_PPAMA_NDVI_cool_ppt_df <- list(HIGH_PPAMA_NDVI_cool_ppt_r2,HIGH_PPAMA_NDVI_cool_ppt_my.p, HIGH_PPAMA_NDVI_cool_ppt_my.slope)
HIGH_PPAMA_NDVI_cool_ppt_data<- do.call(cbind, HIGH_PPAMA_NDVI_cool_ppt_df)
colnames(HIGH_PPAMA_NDVI_cool_ppt_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_NDVI_cool_vpd <- summary(lm(highmarketvalue$ppamacoolyearvpdavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_cool_vpd_r2 = HIGH_PPAMA_NDVI_cool_vpd$adj.r.squared
HIGH_PPAMA_NDVI_cool_vpd_my.p = HIGH_PPAMA_NDVI_cool_vpd$coefficients[2,4]
HIGH_PPAMA_NDVI_cool_vpd_my.slope = HIGH_PPAMA_NDVI_cool_vpd$coefficients[2]
HIGH_PPAMA_NDVI_cool_vpd_df <- list(HIGH_PPAMA_NDVI_cool_vpd_r2,HIGH_PPAMA_NDVI_cool_vpd_my.p, HIGH_PPAMA_NDVI_cool_vpd_my.slope)
HIGH_PPAMA_NDVI_cool_vpd_data<- do.call(cbind, HIGH_PPAMA_NDVI_cool_vpd_df)
colnames(HIGH_PPAMA_NDVI_cool_vpd_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_NDVI_cool_temp <- summary(lm(highmarketvalue$ppamacoolyeartempavg ~ highmarketvalue$ppamacropgrowing))
HIGH_PPAMA_NDVI_cool_temp_r2 = HIGH_PPAMA_NDVI_cool_temp$adj.r.squared
HIGH_PPAMA_NDVI_cool_temp_my.p = HIGH_PPAMA_NDVI_cool_temp$coefficients[2,4]
HIGH_PPAMA_NDVI_cool_temp_my.slope = HIGH_PPAMA_NDVI_cool_temp$coefficients[2]
HIGH_PPAMA_NDVI_cool_temp_df <- list(HIGH_PPAMA_NDVI_cool_temp_r2,HIGH_PPAMA_NDVI_cool_temp_my.p, HIGH_PPAMA_NDVI_cool_temp_my.slope)
HIGH_PPAMA_NDVI_cool_temp_data<- do.call(cbind, HIGH_PPAMA_NDVI_cool_temp_df)
colnames(HIGH_PPAMA_NDVI_cool_temp_data) <- c("r2", "p.value", "slope") 

#LOW
LOW_PPAMA_NDVI_cool_ai <- summary(lm(lowmarketvalue$ppamacoolyearaiavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_cool_ai_r2 = LOW_PPAMA_NDVI_cool_ai$adj.r.squared
LOW_PPAMA_NDVI_cool_ai_my.p = LOW_PPAMA_NDVI_cool_ai$coefficients[2,4]
LOW_PPAMA_NDVI_cool_ai_my.slope = LOW_PPAMA_NDVI_cool_ai$coefficients[2]
LOW_PPAMA_NDVI_cool_ai_df <- list(LOW_PPAMA_NDVI_cool_ai_r2,LOW_PPAMA_NDVI_cool_ai_my.p, LOW_PPAMA_NDVI_cool_ai_my.slope)
LOW_PPAMA_NDVI_cool_ai_data<- do.call(cbind, LOW_PPAMA_NDVI_cool_ai_df)
colnames(LOW_PPAMA_NDVI_cool_ai_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_NDVI_cool_pet <- summary(lm(lowmarketvalue$ppamacoolyearpetavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_cool_pet_r2 = LOW_PPAMA_NDVI_cool_pet$adj.r.squared
LOW_PPAMA_NDVI_cool_pet_my.p = LOW_PPAMA_NDVI_cool_pet$coefficients[2,4]
LOW_PPAMA_NDVI_cool_pet_my.slope = LOW_PPAMA_NDVI_cool_pet$coefficients[2]
LOW_PPAMA_NDVI_cool_pet_df <- list(LOW_PPAMA_NDVI_cool_pet_r2,LOW_PPAMA_NDVI_cool_pet_my.p, LOW_PPAMA_NDVI_cool_pet_my.slope)
LOW_PPAMA_NDVI_cool_pet_data<- do.call(cbind, LOW_PPAMA_NDVI_cool_pet_df)
colnames(LOW_PPAMA_NDVI_cool_pet_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_NDVI_cool_ppt <- summary(lm(lowmarketvalue$ppamacoolyearprecipavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_cool_ppt_r2 = LOW_PPAMA_NDVI_cool_ppt$adj.r.squared
LOW_PPAMA_NDVI_cool_ppt_my.p = LOW_PPAMA_NDVI_cool_ppt$coefficients[2,4]
LOW_PPAMA_NDVI_cool_ppt_my.slope = LOW_PPAMA_NDVI_cool_ppt$coefficients[2]
LOW_PPAMA_NDVI_cool_ppt_df <- list(LOW_PPAMA_NDVI_cool_ppt_r2,LOW_PPAMA_NDVI_cool_ppt_my.p, LOW_PPAMA_NDVI_cool_ppt_my.slope)
LOW_PPAMA_NDVI_cool_ppt_data<- do.call(cbind, LOW_PPAMA_NDVI_cool_ppt_df)
colnames(LOW_PPAMA_NDVI_cool_ppt_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_NDVI_cool_vpd <- summary(lm(lowmarketvalue$ppamacoolyearvpdavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_cool_vpd_r2 = LOW_PPAMA_NDVI_cool_vpd$adj.r.squared
LOW_PPAMA_NDVI_cool_vpd_my.p = LOW_PPAMA_NDVI_cool_vpd$coefficients[2,4]
LOW_PPAMA_NDVI_cool_vpd_my.slope = LOW_PPAMA_NDVI_cool_vpd$coefficients[2]
LOW_PPAMA_NDVI_cool_vpd_df <- list(LOW_PPAMA_NDVI_cool_vpd_r2,LOW_PPAMA_NDVI_cool_vpd_my.p, LOW_PPAMA_NDVI_cool_vpd_my.slope)
LOW_PPAMA_NDVI_cool_vpd_data<- do.call(cbind, LOW_PPAMA_NDVI_cool_vpd_df)
colnames(LOW_PPAMA_NDVI_cool_vpd_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_NDVI_cool_temp <- summary(lm(lowmarketvalue$ppamacoolyeartempavg ~ lowmarketvalue$ppamacropgrowing))
LOW_PPAMA_NDVI_cool_temp_r2 = LOW_PPAMA_NDVI_cool_temp$adj.r.squared
LOW_PPAMA_NDVI_cool_temp_my.p = LOW_PPAMA_NDVI_cool_temp$coefficients[2,4]
LOW_PPAMA_NDVI_cool_temp_my.slope = LOW_PPAMA_NDVI_cool_temp$coefficients[2]
LOW_PPAMA_NDVI_cool_temp_df <- list(LOW_PPAMA_NDVI_cool_temp_r2,LOW_PPAMA_NDVI_cool_temp_my.p, LOW_PPAMA_NDVI_cool_temp_my.slope)
LOW_PPAMA_NDVI_cool_temp_data<- do.call(cbind, LOW_PPAMA_NDVI_cool_temp_df)
colnames(LOW_PPAMA_NDVI_cool_temp_data) <- c("r2", "p.value", "slope") 

#cool
#low
#EXTENT
HIGH_PPAMA_EXTENT_cool_ai <- summary(lm(highmarketvalue$ppamacoolyearaiavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_cool_ai_r2 = HIGH_PPAMA_EXTENT_cool_ai$adj.r.squared
HIGH_PPAMA_EXTENT_cool_ai_my.p = HIGH_PPAMA_EXTENT_cool_ai$coefficients[2,4]
HIGH_PPAMA_EXTENT_cool_ai_my.slope = HIGH_PPAMA_EXTENT_cool_ai$coefficients[2]
HIGH_PPAMA_EXTENT_cool_ai_df <- list(HIGH_PPAMA_EXTENT_cool_ai_r2,HIGH_PPAMA_EXTENT_cool_ai_my.p, HIGH_PPAMA_EXTENT_cool_ai_my.slope)
HIGH_PPAMA_EXTENT_cool_ai_data<- do.call(cbind, HIGH_PPAMA_EXTENT_cool_ai_df)
colnames(HIGH_PPAMA_EXTENT_cool_ai_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_EXTENT_cool_pet <- summary(lm(highmarketvalue$ppamacoolyearpetavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_cool_pet_r2 = HIGH_PPAMA_EXTENT_cool_pet$adj.r.squared
HIGH_PPAMA_EXTENT_cool_pet_my.p = HIGH_PPAMA_EXTENT_cool_pet$coefficients[2,4]
HIGH_PPAMA_EXTENT_cool_pet_my.slope = HIGH_PPAMA_EXTENT_cool_pet$coefficients[2]
HIGH_PPAMA_EXTENT_cool_pet_df <- list(HIGH_PPAMA_EXTENT_cool_pet_r2,HIGH_PPAMA_EXTENT_cool_pet_my.p, HIGH_PPAMA_EXTENT_cool_pet_my.slope)
HIGH_PPAMA_EXTENT_cool_pet_data<- do.call(cbind, HIGH_PPAMA_EXTENT_cool_pet_df)
colnames(HIGH_PPAMA_EXTENT_cool_pet_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_EXTENT_cool_ppt <- summary(lm(highmarketvalue$ppamacoolyearprecipavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_cool_ppt_r2 = HIGH_PPAMA_EXTENT_cool_ppt$adj.r.squared
HIGH_PPAMA_EXTENT_cool_ppt_my.p = HIGH_PPAMA_EXTENT_cool_ppt$coefficients[2,4]
HIGH_PPAMA_EXTENT_cool_ppt_my.slope = HIGH_PPAMA_EXTENT_cool_ppt$coefficients[2]
HIGH_PPAMA_EXTENT_cool_ppt_df <- list(HIGH_PPAMA_EXTENT_cool_ppt_r2,HIGH_PPAMA_EXTENT_cool_ppt_my.p, HIGH_PPAMA_EXTENT_cool_ppt_my.slope)
HIGH_PPAMA_EXTENT_cool_ppt_data<- do.call(cbind, HIGH_PPAMA_EXTENT_cool_ppt_df)
colnames(HIGH_PPAMA_EXTENT_cool_ppt_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_EXTENT_cool_vpd <- summary(lm(highmarketvalue$ppamacoolyearvpdavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_cool_vpd_r2 = HIGH_PPAMA_EXTENT_cool_vpd$adj.r.squared
HIGH_PPAMA_EXTENT_cool_vpd_my.p = HIGH_PPAMA_EXTENT_cool_vpd$coefficients[2,4]
HIGH_PPAMA_EXTENT_cool_vpd_my.slope = HIGH_PPAMA_EXTENT_cool_vpd$coefficients[2]
HIGH_PPAMA_EXTENT_cool_vpd_df <- list(HIGH_PPAMA_EXTENT_cool_vpd_r2,HIGH_PPAMA_EXTENT_cool_vpd_my.p, HIGH_PPAMA_EXTENT_cool_vpd_my.slope)
HIGH_PPAMA_EXTENT_cool_vpd_data<- do.call(cbind, HIGH_PPAMA_EXTENT_cool_vpd_df)
colnames(HIGH_PPAMA_EXTENT_cool_vpd_data) <- c("r2", "p.value", "slope") 

HIGH_PPAMA_EXTENT_cool_temp <- summary(lm(highmarketvalue$ppamacoolyeartempavg ~ highmarketvalue$Phx.Extent))
HIGH_PPAMA_EXTENT_cool_temp_r2 = HIGH_PPAMA_EXTENT_cool_temp$adj.r.squared
HIGH_PPAMA_EXTENT_cool_temp_my.p = HIGH_PPAMA_EXTENT_cool_temp$coefficients[2,4]
HIGH_PPAMA_EXTENT_cool_temp_my.slope = HIGH_PPAMA_EXTENT_cool_temp$coefficients[2]
HIGH_PPAMA_EXTENT_cool_temp_df <- list(HIGH_PPAMA_EXTENT_cool_temp_r2,HIGH_PPAMA_EXTENT_cool_temp_my.p, HIGH_PPAMA_EXTENT_cool_temp_my.slope)
HIGH_PPAMA_EXTENT_cool_temp_data<- do.call(cbind, HIGH_PPAMA_EXTENT_cool_temp_df)
colnames(HIGH_PPAMA_EXTENT_cool_temp_data) <- c("r2", "p.value", "slope") 

#LOW
LOW_PPAMA_EXTENT_cool_ai <- summary(lm(lowmarketvalue$ppamacoolyearaiavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_cool_ai_r2 = LOW_PPAMA_EXTENT_cool_ai$adj.r.squared
LOW_PPAMA_EXTENT_cool_ai_my.p = LOW_PPAMA_EXTENT_cool_ai$coefficients[2,4]
LOW_PPAMA_EXTENT_cool_ai_my.slope = LOW_PPAMA_EXTENT_cool_ai$coefficients[2]
LOW_PPAMA_EXTENT_cool_ai_df <- list(LOW_PPAMA_EXTENT_cool_ai_r2,LOW_PPAMA_EXTENT_cool_ai_my.p, LOW_PPAMA_EXTENT_cool_ai_my.slope)
LOW_PPAMA_EXTENT_cool_ai_data<- do.call(cbind, LOW_PPAMA_EXTENT_cool_ai_df)
colnames(LOW_PPAMA_EXTENT_cool_ai_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_EXTENT_cool_pet <- summary(lm(lowmarketvalue$ppamacoolyearpetavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_cool_pet_r2 = LOW_PPAMA_EXTENT_cool_pet$adj.r.squared
LOW_PPAMA_EXTENT_cool_pet_my.p = LOW_PPAMA_EXTENT_cool_pet$coefficients[2,4]
LOW_PPAMA_EXTENT_cool_pet_my.slope = LOW_PPAMA_EXTENT_cool_pet$coefficients[2]
LOW_PPAMA_EXTENT_cool_pet_df <- list(LOW_PPAMA_EXTENT_cool_pet_r2,LOW_PPAMA_EXTENT_cool_pet_my.p, LOW_PPAMA_EXTENT_cool_pet_my.slope)
LOW_PPAMA_EXTENT_cool_pet_data<- do.call(cbind, LOW_PPAMA_EXTENT_cool_pet_df)
colnames(LOW_PPAMA_EXTENT_cool_pet_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_EXTENT_cool_ppt <- summary(lm(lowmarketvalue$ppamacoolyearprecipavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_cool_ppt_r2 = LOW_PPAMA_EXTENT_cool_ppt$adj.r.squared
LOW_PPAMA_EXTENT_cool_ppt_my.p = LOW_PPAMA_EXTENT_cool_ppt$coefficients[2,4]
LOW_PPAMA_EXTENT_cool_ppt_my.slope = LOW_PPAMA_EXTENT_cool_ppt$coefficients[2]
LOW_PPAMA_EXTENT_cool_ppt_df <- list(LOW_PPAMA_EXTENT_cool_ppt_r2,LOW_PPAMA_EXTENT_cool_ppt_my.p, LOW_PPAMA_EXTENT_cool_ppt_my.slope)
LOW_PPAMA_EXTENT_cool_ppt_data<- do.call(cbind, LOW_PPAMA_EXTENT_cool_ppt_df)
colnames(LOW_PPAMA_EXTENT_cool_ppt_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_EXTENT_cool_vpd <- summary(lm(lowmarketvalue$ppamacoolyearvpdavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_cool_vpd_r2 = LOW_PPAMA_EXTENT_cool_vpd$adj.r.squared
LOW_PPAMA_EXTENT_cool_vpd_my.p = LOW_PPAMA_EXTENT_cool_vpd$coefficients[2,4]
LOW_PPAMA_EXTENT_cool_vpd_my.slope = LOW_PPAMA_EXTENT_cool_vpd$coefficients[2]
LOW_PPAMA_EXTENT_cool_vpd_df <- list(LOW_PPAMA_EXTENT_cool_vpd_r2,LOW_PPAMA_EXTENT_cool_vpd_my.p, LOW_PPAMA_EXTENT_cool_vpd_my.slope)
LOW_PPAMA_EXTENT_cool_vpd_data<- do.call(cbind, LOW_PPAMA_EXTENT_cool_vpd_df)
colnames(LOW_PPAMA_EXTENT_cool_vpd_data) <- c("r2", "p.value", "slope") 

LOW_PPAMA_EXTENT_cool_temp <- summary(lm(lowmarketvalue$ppamacoolyeartempavg ~ lowmarketvalue$Phx.Extent))
LOW_PPAMA_EXTENT_cool_temp_r2 = LOW_PPAMA_EXTENT_cool_temp$adj.r.squared
LOW_PPAMA_EXTENT_cool_temp_my.p = LOW_PPAMA_EXTENT_cool_temp$coefficients[2,4]
LOW_PPAMA_EXTENT_cool_temp_my.slope = LOW_PPAMA_EXTENT_cool_temp$coefficients[2]
LOW_PPAMA_EXTENT_cool_temp_df <- list(LOW_PPAMA_EXTENT_cool_temp_r2,LOW_PPAMA_EXTENT_cool_temp_my.p, LOW_PPAMA_EXTENT_cool_temp_my.slope)
LOW_PPAMA_EXTENT_cool_temp_data<- do.call(cbind, LOW_PPAMA_EXTENT_cool_temp_df)
colnames(LOW_PPAMA_EXTENT_cool_temp_data) <- c("r2", "p.value", "slope")



setwd("D:/MS Research Project 2020 EDITS/rcsv")


#statistics
#LCRP
#YEAR
#NDVI
LCRP_NDVI_YEAR_ai <- summary(lm(integratedvariables$lcrpyearai ~ integratedvariables$lcrpcropgrowing))
LCRP_NDVI_YEAR_ai_r2 = LCRP_NDVI_YEAR_ai$adj.r.squared
LCRP_NDVI_YEAR_ai_my.p = LCRP_NDVI_YEAR_ai$coefficients[2,4]
LCRP_NDVI_YEAR_ai_my.slope = LCRP_NDVI_YEAR_ai$coefficients[2]
LCRP_NDVI_YEAR_ai_df <- list(LCRP_NDVI_YEAR_ai_r2,LCRP_NDVI_YEAR_ai_my.p, LCRP_NDVI_YEAR_ai_my.slope)
LCRP_NDVI_YEAR_ai_data<- do.call(cbind, LCRP_NDVI_YEAR_ai_df)
colnames(LCRP_NDVI_YEAR_ai_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_YEAR_pet <- summary(lm(integratedvariables$lcrpyearpet ~ integratedvariables$lcrpcropgrowing))
LCRP_NDVI_YEAR_pet_r2 = LCRP_NDVI_YEAR_pet$adj.r.squared
LCRP_NDVI_YEAR_pet_my.p = LCRP_NDVI_YEAR_pet$coefficients[2,4]
LCRP_NDVI_YEAR_pet_my.slope = LCRP_NDVI_YEAR_pet$coefficients[2]
LCRP_NDVI_YEAR_pet_df <- list(LCRP_NDVI_YEAR_pet_r2,LCRP_NDVI_YEAR_pet_my.p, LCRP_NDVI_YEAR_pet_my.slope)
LCRP_NDVI_YEAR_pet_data<- do.call(cbind, LCRP_NDVI_YEAR_pet_df)
colnames(LCRP_NDVI_YEAR_pet_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_YEAR_ppt <- summary(lm(integratedvariables$lcrpyearppt ~ integratedvariables$lcrpcropgrowing))
LCRP_NDVI_YEAR_ppt_r2 = LCRP_NDVI_YEAR_ppt$adj.r.squared
LCRP_NDVI_YEAR_ppt_my.p = LCRP_NDVI_YEAR_ppt$coefficients[2,4]
LCRP_NDVI_YEAR_ppt_my.slope = LCRP_NDVI_YEAR_ppt$coefficients[2]
LCRP_NDVI_YEAR_ppt_df <- list(LCRP_NDVI_YEAR_ppt_r2,LCRP_NDVI_YEAR_ppt_my.p, LCRP_NDVI_YEAR_ppt_my.slope)
LCRP_NDVI_YEAR_ppt_data<- do.call(cbind, LCRP_NDVI_YEAR_ppt_df)
colnames(LCRP_NDVI_YEAR_ppt_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_YEAR_vpd <- summary(lm(integratedvariables$lcrpyearvpd ~ integratedvariables$lcrpcropgrowing))
LCRP_NDVI_YEAR_vpd_r2 = LCRP_NDVI_YEAR_vpd$adj.r.squared
LCRP_NDVI_YEAR_vpd_my.p = LCRP_NDVI_YEAR_vpd$coefficients[2,4]
LCRP_NDVI_YEAR_vpd_my.slope = LCRP_NDVI_YEAR_vpd$coefficients[2]
LCRP_NDVI_YEAR_vpd_df <- list(LCRP_NDVI_YEAR_vpd_r2,LCRP_NDVI_YEAR_vpd_my.p, LCRP_NDVI_YEAR_vpd_my.slope)
LCRP_NDVI_YEAR_vpd_data<- do.call(cbind, LCRP_NDVI_YEAR_vpd_df)
colnames(LCRP_NDVI_YEAR_vpd_data) <- c("r2", "p.value", "slope") 

LCRP_NDVI_YEAR_temp <- summary(lm(integratedvariables$lcrpyeartemp ~ integratedvariables$lcrpcropgrowing))
LCRP_NDVI_YEAR_temp_r2 = LCRP_NDVI_YEAR_temp$adj.r.squared
LCRP_NDVI_YEAR_temp_my.p = LCRP_NDVI_YEAR_temp$coefficients[2,4]
LCRP_NDVI_YEAR_temp_my.slope = LCRP_NDVI_YEAR_temp$coefficients[2]
LCRP_NDVI_YEAR_temp_df <- list(LCRP_NDVI_YEAR_temp_r2,LCRP_NDVI_YEAR_temp_my.p, LCRP_NDVI_YEAR_temp_my.slope)
LCRP_NDVI_YEAR_temp_data<- do.call(cbind, LCRP_NDVI_YEAR_temp_df)
colnames(LCRP_NDVI_YEAR_temp_data) <- c("r2", "p.value", "slope") 


#YEAR
#NDVI
PPAMA_NDVI_YEAR_ai <- summary(lm(integratedvariables$ppamayearai ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_YEAR_ai_r2 = PPAMA_NDVI_YEAR_ai$adj.r.squared
PPAMA_NDVI_YEAR_ai_my.p = PPAMA_NDVI_YEAR_ai$coefficients[2,4]
PPAMA_NDVI_YEAR_ai_my.slope = PPAMA_NDVI_YEAR_ai$coefficients[2]
PPAMA_NDVI_YEAR_ai_df <- list(PPAMA_NDVI_YEAR_ai_r2,PPAMA_NDVI_YEAR_ai_my.p, PPAMA_NDVI_YEAR_ai_my.slope)
PPAMA_NDVI_YEAR_ai_data<- do.call(cbind, PPAMA_NDVI_YEAR_ai_df)
colnames(PPAMA_NDVI_YEAR_ai_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_YEAR_pet <- summary(lm(integratedvariables$ppamayearppt ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_YEAR_pet_r2 = PPAMA_NDVI_YEAR_pet$adj.r.squared
PPAMA_NDVI_YEAR_pet_my.p = PPAMA_NDVI_YEAR_pet$coefficients[2,4]
PPAMA_NDVI_YEAR_pet_my.slope = PPAMA_NDVI_YEAR_pet$coefficients[2]
PPAMA_NDVI_YEAR_pet_df <- list(PPAMA_NDVI_YEAR_pet_r2,PPAMA_NDVI_YEAR_pet_my.p, PPAMA_NDVI_YEAR_pet_my.slope)
PPAMA_NDVI_YEAR_pet_data<- do.call(cbind, PPAMA_NDVI_YEAR_pet_df)
colnames(PPAMA_NDVI_YEAR_pet_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_YEAR_ppt <- summary(lm(integratedvariables$ppamayearppt ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_YEAR_ppt_r2 = PPAMA_NDVI_YEAR_ppt$adj.r.squared
PPAMA_NDVI_YEAR_ppt_my.p = PPAMA_NDVI_YEAR_ppt$coefficients[2,4]
PPAMA_NDVI_YEAR_ppt_my.slope = PPAMA_NDVI_YEAR_ppt$coefficients[2]
PPAMA_NDVI_YEAR_ppt_df <- list(PPAMA_NDVI_YEAR_ppt_r2,PPAMA_NDVI_YEAR_ppt_my.p, PPAMA_NDVI_YEAR_ppt_my.slope)
PPAMA_NDVI_YEAR_ppt_data<- do.call(cbind, PPAMA_NDVI_YEAR_ppt_df)
colnames(PPAMA_NDVI_YEAR_ppt_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_YEAR_vpd <- summary(lm(integratedvariables$ppamayearvpd ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_YEAR_vpd_r2 = PPAMA_NDVI_YEAR_vpd$adj.r.squared
PPAMA_NDVI_YEAR_vpd_my.p = PPAMA_NDVI_YEAR_vpd$coefficients[2,4]
PPAMA_NDVI_YEAR_vpd_my.slope = PPAMA_NDVI_YEAR_vpd$coefficients[2]
PPAMA_NDVI_YEAR_vpd_df <- list(PPAMA_NDVI_YEAR_vpd_r2,PPAMA_NDVI_YEAR_vpd_my.p, PPAMA_NDVI_YEAR_vpd_my.slope)
PPAMA_NDVI_YEAR_vpd_data<- do.call(cbind, PPAMA_NDVI_YEAR_vpd_df)
colnames(PPAMA_NDVI_YEAR_vpd_data) <- c("r2", "p.value", "slope") 

PPAMA_NDVI_YEAR_temp <- summary(lm(integratedvariables$ppamayeartemp ~ integratedvariables$ppamacropgrowing))
PPAMA_NDVI_YEAR_temp_r2 = PPAMA_NDVI_YEAR_temp$adj.r.squared
PPAMA_NDVI_YEAR_temp_my.p = PPAMA_NDVI_YEAR_temp$coefficients[2,4]
PPAMA_NDVI_YEAR_temp_my.slope = PPAMA_NDVI_YEAR_temp$coefficients[2]
PPAMA_NDVI_YEAR_temp_df <- list(PPAMA_NDVI_YEAR_temp_r2,PPAMA_NDVI_YEAR_temp_my.p, PPAMA_NDVI_YEAR_temp_my.slope)
PPAMA_NDVI_YEAR_temp_data<- do.call(cbind, PPAMA_NDVI_YEAR_temp_df)
colnames(PPAMA_NDVI_YEAR_temp_data) <- c("r2", "p.value", "slope") 



#statistics
#LCRP
#YEAR
#EXTENT
LCRP_EXTENT_YEAR_ai <- summary(lm(integratedvariables$lcrpyearai ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_YEAR_ai_r2 = LCRP_EXTENT_YEAR_ai$adj.r.squared
LCRP_EXTENT_YEAR_ai_my.p = LCRP_EXTENT_YEAR_ai$coefficients[2,4]
LCRP_EXTENT_YEAR_ai_my.slope = LCRP_EXTENT_YEAR_ai$coefficients[2]
LCRP_EXTENT_YEAR_ai_df <- list(LCRP_EXTENT_YEAR_ai_r2,LCRP_EXTENT_YEAR_ai_my.p, LCRP_EXTENT_YEAR_ai_my.slope)
LCRP_EXTENT_YEAR_ai_data<- do.call(cbind, LCRP_EXTENT_YEAR_ai_df)
colnames(LCRP_EXTENT_YEAR_ai_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_YEAR_pet <- summary(lm(integratedvariables$lcrpyearpet ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_YEAR_pet_r2 = LCRP_EXTENT_YEAR_pet$adj.r.squared
LCRP_EXTENT_YEAR_pet_my.p = LCRP_EXTENT_YEAR_pet$coefficients[2,4]
LCRP_EXTENT_YEAR_pet_my.slope = LCRP_EXTENT_YEAR_pet$coefficients[2]
LCRP_EXTENT_YEAR_pet_df <- list(LCRP_EXTENT_YEAR_pet_r2,LCRP_EXTENT_YEAR_pet_my.p, LCRP_EXTENT_YEAR_pet_my.slope)
LCRP_EXTENT_YEAR_pet_data<- do.call(cbind, LCRP_EXTENT_YEAR_pet_df)
colnames(LCRP_EXTENT_YEAR_pet_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_YEAR_ppt <- summary(lm(integratedvariables$lcrpyearppt ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_YEAR_ppt_r2 = LCRP_EXTENT_YEAR_ppt$adj.r.squared
LCRP_EXTENT_YEAR_ppt_my.p = LCRP_EXTENT_YEAR_ppt$coefficients[2,4]
LCRP_EXTENT_YEAR_ppt_my.slope = LCRP_EXTENT_YEAR_ppt$coefficients[2]
LCRP_EXTENT_YEAR_ppt_df <- list(LCRP_EXTENT_YEAR_ppt_r2,LCRP_EXTENT_YEAR_ppt_my.p, LCRP_EXTENT_YEAR_ppt_my.slope)
LCRP_EXTENT_YEAR_ppt_data<- do.call(cbind, LCRP_EXTENT_YEAR_ppt_df)
colnames(LCRP_EXTENT_YEAR_ppt_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_YEAR_vpd <- summary(lm(integratedvariables$lcrpyearvpd ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_YEAR_vpd_r2 = LCRP_EXTENT_YEAR_vpd$adj.r.squared
LCRP_EXTENT_YEAR_vpd_my.p = LCRP_EXTENT_YEAR_vpd$coefficients[2,4]
LCRP_EXTENT_YEAR_vpd_my.slope = LCRP_EXTENT_YEAR_vpd$coefficients[2]
LCRP_EXTENT_YEAR_vpd_df <- list(LCRP_EXTENT_YEAR_vpd_r2,LCRP_EXTENT_YEAR_vpd_my.p, LCRP_EXTENT_YEAR_vpd_my.slope)
LCRP_EXTENT_YEAR_vpd_data<- do.call(cbind, LCRP_EXTENT_YEAR_vpd_df)
colnames(LCRP_EXTENT_YEAR_vpd_data) <- c("r2", "p.value", "slope") 

LCRP_EXTENT_YEAR_temp <- summary(lm(integratedvariables$lcrpyeartemp ~ integratedvariables$Yuma.Extent))
LCRP_EXTENT_YEAR_temp_r2 = LCRP_EXTENT_YEAR_temp$adj.r.squared
LCRP_EXTENT_YEAR_temp_my.p = LCRP_EXTENT_YEAR_temp$coefficients[2,4]
LCRP_EXTENT_YEAR_temp_my.slope = LCRP_EXTENT_YEAR_temp$coefficients[2]
LCRP_EXTENT_YEAR_temp_df <- list(LCRP_EXTENT_YEAR_temp_r2,LCRP_EXTENT_YEAR_temp_my.p, LCRP_EXTENT_YEAR_temp_my.slope)
LCRP_EXTENT_YEAR_temp_data<- do.call(cbind, LCRP_EXTENT_YEAR_temp_df)
colnames(LCRP_EXTENT_YEAR_temp_data) <- c("r2", "p.value", "slope") 


#YEAR
#EXTENT
PPAMA_EXTENT_YEAR_ai <- summary(lm(integratedvariables$ppamayearai ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_YEAR_ai_r2 = PPAMA_EXTENT_YEAR_ai$adj.r.squared
PPAMA_EXTENT_YEAR_ai_my.p = PPAMA_EXTENT_YEAR_ai$coefficients[2,4]
PPAMA_EXTENT_YEAR_ai_my.slope = PPAMA_EXTENT_YEAR_ai$coefficients[2]
PPAMA_EXTENT_YEAR_ai_df <- list(PPAMA_EXTENT_YEAR_ai_r2,PPAMA_EXTENT_YEAR_ai_my.p, PPAMA_EXTENT_YEAR_ai_my.slope)
PPAMA_EXTENT_YEAR_ai_data<- do.call(cbind, PPAMA_EXTENT_YEAR_ai_df)
colnames(PPAMA_EXTENT_YEAR_ai_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_YEAR_pet <- summary(lm(integratedvariables$ppamayearppt ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_YEAR_pet_r2 = PPAMA_EXTENT_YEAR_pet$adj.r.squared
PPAMA_EXTENT_YEAR_pet_my.p = PPAMA_EXTENT_YEAR_pet$coefficients[2,4]
PPAMA_EXTENT_YEAR_pet_my.slope = PPAMA_EXTENT_YEAR_pet$coefficients[2]
PPAMA_EXTENT_YEAR_pet_df <- list(PPAMA_EXTENT_YEAR_pet_r2,PPAMA_EXTENT_YEAR_pet_my.p, PPAMA_EXTENT_YEAR_pet_my.slope)
PPAMA_EXTENT_YEAR_pet_data<- do.call(cbind, PPAMA_EXTENT_YEAR_pet_df)
colnames(PPAMA_EXTENT_YEAR_pet_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_YEAR_ppt <- summary(lm(integratedvariables$ppamayearppt ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_YEAR_ppt_r2 = PPAMA_EXTENT_YEAR_ppt$adj.r.squared
PPAMA_EXTENT_YEAR_ppt_my.p = PPAMA_EXTENT_YEAR_ppt$coefficients[2,4]
PPAMA_EXTENT_YEAR_ppt_my.slope = PPAMA_EXTENT_YEAR_ppt$coefficients[2]
PPAMA_EXTENT_YEAR_ppt_df <- list(PPAMA_EXTENT_YEAR_ppt_r2,PPAMA_EXTENT_YEAR_ppt_my.p, PPAMA_EXTENT_YEAR_ppt_my.slope)
PPAMA_EXTENT_YEAR_ppt_data<- do.call(cbind, PPAMA_EXTENT_YEAR_ppt_df)
colnames(PPAMA_EXTENT_YEAR_ppt_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_YEAR_vpd <- summary(lm(integratedvariables$ppamayearvpd ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_YEAR_vpd_r2 = PPAMA_EXTENT_YEAR_vpd$adj.r.squared
PPAMA_EXTENT_YEAR_vpd_my.p = PPAMA_EXTENT_YEAR_vpd$coefficients[2,4]
PPAMA_EXTENT_YEAR_vpd_my.slope = PPAMA_EXTENT_YEAR_vpd$coefficients[2]
PPAMA_EXTENT_YEAR_vpd_df <- list(PPAMA_EXTENT_YEAR_vpd_r2,PPAMA_EXTENT_YEAR_vpd_my.p, PPAMA_EXTENT_YEAR_vpd_my.slope)
PPAMA_EXTENT_YEAR_vpd_data<- do.call(cbind, PPAMA_EXTENT_YEAR_vpd_df)
colnames(PPAMA_EXTENT_YEAR_vpd_data) <- c("r2", "p.value", "slope") 

PPAMA_EXTENT_YEAR_temp <- summary(lm(integratedvariables$ppamayeartemp ~ integratedvariables$Phx.Extent))
PPAMA_EXTENT_YEAR_temp_r2 = PPAMA_EXTENT_YEAR_temp$adj.r.squared
PPAMA_EXTENT_YEAR_temp_my.p = PPAMA_EXTENT_YEAR_temp$coefficients[2,4]
PPAMA_EXTENT_YEAR_temp_my.slope = PPAMA_EXTENT_YEAR_temp$coefficients[2]
PPAMA_EXTENT_YEAR_temp_df <- list(PPAMA_EXTENT_YEAR_temp_r2,PPAMA_EXTENT_YEAR_temp_my.p, PPAMA_EXTENT_YEAR_temp_my.slope)
PPAMA_EXTENT_YEAR_temp_data<- do.call(cbind, PPAMA_EXTENT_YEAR_temp_df)
colnames(PPAMA_EXTENT_YEAR_temp_data) <- c("r2", "p.value", "slope") 


#RBIND and export
#PPAMA
#NDVI
#YEAR
PPAMA_EXTENT_YEAR_list<- list(PPAMA_EXTENT_YEAR_ai_data, PPAMA_EXTENT_YEAR_pet_data, PPAMA_EXTENT_YEAR_ppt_data, PPAMA_EXTENT_YEAR_vpd_data, PPAMA_EXTENT_YEAR_temp_data)
PPAMA_EXTENT_YEAR_bind<- do.call(rbind, PPAMA_EXTENT_YEAR_list)
rownames(PPAMA_EXTENT_YEAR_bind) <- c("PPAMA_EXTENT_YEAR_ai_data", "PPAMA_EXTENT_YEAR_pet_data", "PPAMA_EXTENT_YEAR_ppt_data", "PPAMA_EXTENT_YEAR_vpd_data", "PPAMA_EXTENT_YEAR_temp_data") 

#Extent
PPAMA_NDVI_YEAR_list<- list(PPAMA_NDVI_YEAR_ai_data, PPAMA_NDVI_YEAR_pet_data, PPAMA_NDVI_YEAR_ppt_data, PPAMA_NDVI_YEAR_vpd_data, PPAMA_NDVI_YEAR_temp_data)
PPAMA_NDVI_YEAR_bind<- do.call(rbind, PPAMA_NDVI_YEAR_list)
rownames(PPAMA_NDVI_YEAR_bind) <- c("PPAMA_NDVI_YEAR_ai_data", "PPAMA_NDVI_YEAR_pet_data", "PPAMA_NDVI_YEAR_ppt_data", "PPAMA_NDVI_YEAR_vpd_data", "PPAMA_NDVI_YEAR_temp_data") 

PPAMA_YEAR_bind <- rbind(PPAMA_NDVI_YEAR_bind,PPAMA_EXTENT_YEAR_bind)
write.csv(PPAMA_YEAR_bind, file = "PPAMA_YEAR_bind.csv")



#RBIND and export
#LCRP
#NDVI
#YEAR
LCRP_EXTENT_YEAR_list<- list(LCRP_EXTENT_YEAR_ai_data, LCRP_EXTENT_YEAR_pet_data, LCRP_EXTENT_YEAR_ppt_data, LCRP_EXTENT_YEAR_vpd_data, LCRP_EXTENT_YEAR_temp_data)
LCRP_EXTENT_YEAR_bind<- do.call(rbind, LCRP_EXTENT_YEAR_list)
rownames(LCRP_EXTENT_YEAR_bind) <- c("LCRP_EXTENT_YEAR_ai_data", "LCRP_EXTENT_YEAR_pet_data", "LCRP_EXTENT_YEAR_ppt_data", "LCRP_EXTENT_YEAR_vpd_data", "LCRP_EXTENT_YEAR_temp_data") 

#Extent
LCRP_NDVI_YEAR_list<- list(LCRP_NDVI_YEAR_ai_data, LCRP_NDVI_YEAR_pet_data, LCRP_NDVI_YEAR_ppt_data, LCRP_NDVI_YEAR_vpd_data, LCRP_NDVI_YEAR_temp_data)
LCRP_NDVI_YEAR_bind<- do.call(rbind, LCRP_NDVI_YEAR_list)
rownames(LCRP_NDVI_YEAR_bind) <- c("LCRP_NDVI_YEAR_ai_data", "LCRP_NDVI_YEAR_pet_data", "LCRP_NDVI_YEAR_ppt_data", "LCRP_NDVI_YEAR_vpd_data", "LCRP_NDVI_YEAR_temp_data") 

LCRP_YEAR_bind <- rbind(LCRP_NDVI_YEAR_bind,LCRP_EXTENT_YEAR_bind)
write.csv(LCRP_YEAR_bind, file = "LCRP_YEAR_bind.csv")






#RBIND and export
#ppama
#NDVI
#wARM
PPAMA_NDVI_warm_list<- list(PPAMA_NDVI_warm_ai_data, PPAMA_NDVI_warm_pet_data, PPAMA_NDVI_warm_ppt_data, PPAMA_NDVI_warm_vpd_data, PPAMA_NDVI_warm_temp_data)
PPAMA_NDVI_warm_bind<- do.call(rbind, PPAMA_NDVI_warm_list)
rownames(PPAMA_NDVI_warm_bind) <- c("PPAMA_NDVI_warm_ai_data", "PPAMA_NDVI_warm_pet_data", "PPAMA_NDVI_warm_ppt_data", "PPAMA_NDVI_warm_vpd_data", "PPAMA_NDVI_warm_temp_data") 

#cool
PPAMA_NDVI_cool_list<- list(PPAMA_NDVI_cool_ai_data, PPAMA_NDVI_cool_pet_data, PPAMA_NDVI_cool_ppt_data, PPAMA_NDVI_cool_vpd_data, PPAMA_NDVI_cool_temp_data)
PPAMA_NDVI_cool_bind<- do.call(rbind, PPAMA_NDVI_cool_list)
rownames(PPAMA_NDVI_cool_bind) <- c("PPAMA_NDVI_cool_ai_data", "PPAMA_NDVI_cool_pet_data", "PPAMA_NDVI_cool_ppt_data", "PPAMA_NDVI_cool_vpd_data", "PPAMA_NDVI_cool_temp_data") 

PPAMA_NDVI_bind <- rbind(PPAMA_NDVI_warm_bind,PPAMA_NDVI_cool_bind)
write.csv(PPAMA_NDVI_bind, file = "PPAMA_NDVI_bind.csv")



#NDVI
#wARM
PPAMA_EXTENT_warm_list<- list(PPAMA_EXTENT_warm_ai_data, PPAMA_EXTENT_warm_pet_data, PPAMA_EXTENT_warm_ppt_data, PPAMA_EXTENT_warm_vpd_data, PPAMA_EXTENT_warm_temp_data)
PPAMA_EXTENT_warm_bind<- do.call(rbind, PPAMA_EXTENT_warm_list)
rownames(PPAMA_EXTENT_warm_bind) <- c("PPAMA_EXTENT_warm_ai_data", "PPAMA_EXTENT_warm_pet_data", "PPAMA_EXTENT_warm_ppt_data", "PPAMA_EXTENT_warm_vpd_data", "PPAMA_EXTENT_warm_temp_data") 

#cool
PPAMA_EXTENT_cool_list<- list(PPAMA_EXTENT_cool_ai_data, PPAMA_EXTENT_cool_pet_data, PPAMA_EXTENT_cool_ppt_data, PPAMA_EXTENT_cool_vpd_data, PPAMA_EXTENT_cool_temp_data)
PPAMA_EXTENT_cool_bind<- do.call(rbind, PPAMA_EXTENT_cool_list)
rownames(PPAMA_EXTENT_cool_bind) <- c("PPAMA_EXTENT_cool_ai_data", "PPAMA_EXTENT_cool_pet_data", "PPAMA_EXTENT_cool_ppt_data", "PPAMA_EXTENT_cool_vpd_data", "PPAMA_EXTENT_cool_temp_data") 

PPAMA_EXTENT_bind <- rbind(PPAMA_EXTENT_warm_bind,PPAMA_EXTENT_cool_bind)
write.csv(PPAMA_EXTENT_bind, file = "PPAMA_EXTENT_bind.csv")



#RBIND
#ppama
#NDVI
#wARM
LCRP_NDVI_warm_list<- list(LCRP_NDVI_warm_ai_data, LCRP_NDVI_warm_pet_data, LCRP_NDVI_warm_ppt_data, LCRP_NDVI_warm_vpd_data, LCRP_NDVI_warm_temp_data)
LCRP_NDVI_warm_bind<- do.call(rbind, LCRP_NDVI_warm_list)
rownames(LCRP_NDVI_warm_bind) <- c("LCRP_NDVI_warm_ai_data", "LCRP_NDVI_warm_pet_data", "LCRP_NDVI_warm_ppt_data", "LCRP_NDVI_warm_vpd_data", "LCRP_NDVI_warm_temp_data") 

#cool
LCRP_NDVI_cool_list<- list(LCRP_NDVI_cool_ai_data, LCRP_NDVI_cool_pet_data, LCRP_NDVI_cool_ppt_data, LCRP_NDVI_cool_vpd_data, LCRP_NDVI_cool_temp_data)
LCRP_NDVI_cool_bind<- do.call(rbind, LCRP_NDVI_cool_list)
rownames(LCRP_NDVI_cool_bind) <- c("LCRP_NDVI_cool_ai_data", "LCRP_NDVI_cool_pet_data", "LCRP_NDVI_cool_ppt_data", "LCRP_NDVI_cool_vpd_data", "LCRP_NDVI_cool_temp_data") 

LCRP_NDVI_bind <- rbind(LCRP_NDVI_warm_bind,LCRP_NDVI_cool_bind)
write.csv(LCRP_NDVI_bind, file = "LCRP_NDVI_bind.csv")


#NDVI
#wARM
LCRP_EXTENT_warm_list<- list(LCRP_EXTENT_warm_ai_data, LCRP_EXTENT_warm_pet_data, LCRP_EXTENT_warm_ppt_data, LCRP_EXTENT_warm_vpd_data, LCRP_EXTENT_warm_temp_data)
LCRP_EXTENT_warm_bind<- do.call(rbind, LCRP_EXTENT_warm_list)
rownames(LCRP_EXTENT_warm_bind) <- c("LCRP_EXTENT_warm_ai_data", "LCRP_EXTENT_warm_pet_data", "LCRP_EXTENT_warm_ppt_data", "LCRP_EXTENT_warm_vpd_data", "LCRP_EXTENT_warm_temp_data") 

#cool
LCRP_EXTENT_cool_list<- list(LCRP_EXTENT_cool_ai_data, LCRP_EXTENT_cool_pet_data, LCRP_EXTENT_cool_ppt_data, LCRP_EXTENT_cool_vpd_data, LCRP_EXTENT_cool_temp_data)
LCRP_EXTENT_cool_bind<- do.call(rbind, LCRP_EXTENT_cool_list)
rownames(LCRP_EXTENT_cool_bind) <- c("LCRP_EXTENT_cool_ai_data", "LCRP_EXTENT_cool_pet_data", "LCRP_EXTENT_cool_ppt_data", "LCRP_EXTENT_cool_vpd_data", "LCRP_EXTENT_cool_temp_data") 

LCRP_EXTENT_bind <- rbind(LCRP_EXTENT_warm_bind,LCRP_EXTENT_cool_bind)
write.csv(LCRP_EXTENT_bind, file = "LCRP_EXTENT_bind.csv")










#RBIND
#ppama
#High Market Value
#NDVI
#wARM
HIGH_PPAMA_NDVI_warm_list<- list(HIGH_PPAMA_NDVI_warm_ai_data, HIGH_PPAMA_NDVI_warm_pet_data, HIGH_PPAMA_NDVI_warm_ppt_data, HIGH_PPAMA_NDVI_warm_vpd_data, HIGH_PPAMA_NDVI_warm_temp_data)
HIGH_PPAMA_NDVI_warm_bind<- do.call(rbind, HIGH_PPAMA_NDVI_warm_list)
rownames(HIGH_PPAMA_NDVI_warm_bind) <- c("HIGH_PPAMA_NDVI_warm_ai_data", "HIGH_PPAMA_NDVI_warm_pet_data", "HIGH_PPAMA_NDVI_warm_ppt_data", "HIGH_PPAMA_NDVI_warm_vpd_data", "HIGH_PPAMA_NDVI_warm_temp_data") 

#cool
HIGH_PPAMA_NDVI_cool_list<- list(HIGH_PPAMA_NDVI_cool_ai_data, HIGH_PPAMA_NDVI_cool_pet_data, HIGH_PPAMA_NDVI_cool_ppt_data, HIGH_PPAMA_NDVI_cool_vpd_data, HIGH_PPAMA_NDVI_cool_temp_data)
HIGH_PPAMA_NDVI_cool_bind<- do.call(rbind, HIGH_PPAMA_NDVI_cool_list)
rownames(HIGH_PPAMA_NDVI_cool_bind) <- c("HIGH_PPAMA_NDVI_cool_ai_data", "HIGH_PPAMA_NDVI_cool_pet_data", "HIGH_PPAMA_NDVI_cool_ppt_data", "HIGH_PPAMA_NDVI_cool_vpd_data", "HIGH_PPAMA_NDVI_cool_temp_data") 

HIGH_PPAMA_NDVI_bind <- rbind(HIGH_PPAMA_NDVI_cool_bind,HIGH_PPAMA_NDVI_warm_bind)
write.csv(HIGH_PPAMA_NDVI_bind, file = "HIGH_PPAMA_NDVI_bind.csv")

#NDVI
#wARM
HIGH_PPAMA_EXTENT_warm_list<- list(HIGH_PPAMA_EXTENT_warm_ai_data, HIGH_PPAMA_EXTENT_warm_pet_data, HIGH_PPAMA_EXTENT_warm_ppt_data, HIGH_PPAMA_EXTENT_warm_vpd_data, HIGH_PPAMA_EXTENT_warm_temp_data)
HIGH_PPAMA_EXTENT_warm_bind<- do.call(rbind, HIGH_PPAMA_EXTENT_warm_list)
rownames(HIGH_PPAMA_EXTENT_warm_bind) <- c("HIGH_PPAMA_EXTENT_warm_ai_data", "HIGH_PPAMA_EXTENT_warm_pet_data", "HIGH_PPAMA_EXTENT_warm_ppt_data", "HIGH_PPAMA_EXTENT_warm_vpd_data", "HIGH_PPAMA_EXTENT_warm_temp_data") 

#cool
HIGH_PPAMA_EXTENT_cool_list<- list(HIGH_PPAMA_EXTENT_cool_ai_data, HIGH_PPAMA_EXTENT_cool_pet_data, HIGH_PPAMA_EXTENT_cool_ppt_data, HIGH_PPAMA_EXTENT_cool_vpd_data, HIGH_PPAMA_EXTENT_cool_temp_data)
HIGH_PPAMA_EXTENT_cool_bind<- do.call(rbind, HIGH_PPAMA_EXTENT_cool_list)
rownames(HIGH_PPAMA_EXTENT_cool_bind) <- c("HIGH_PPAMA_EXTENT_cool_ai_data", "HIGH_PPAMA_EXTENT_cool_pet_data", "HIGH_PPAMA_EXTENT_cool_ppt_data", "HIGH_PPAMA_EXTENT_cool_vpd_data", "HIGH_PPAMA_EXTENT_cool_temp_data") 

HIGH_PPAMA_EXTENT_bind <- rbind(HIGH_PPAMA_EXTENT_cool_bind,HIGH_PPAMA_EXTENT_warm_bind)
write.csv(HIGH_PPAMA_EXTENT_bind, file = "HIGH_PPAMA_EXTENT_bind.csv")


#RBIND
#LCRP
#NDVI
#wARM
HIGH_LCRP_NDVI_warm_list<- list(HIGH_LCRP_NDVI_warm_ai_data, HIGH_LCRP_NDVI_warm_pet_data, HIGH_LCRP_NDVI_warm_ppt_data, HIGH_LCRP_NDVI_warm_vpd_data, HIGH_LCRP_NDVI_warm_temp_data)
HIGH_LCRP_NDVI_warm_bind<- do.call(rbind, HIGH_LCRP_NDVI_warm_list)
rownames(HIGH_LCRP_NDVI_warm_bind) <- c("HIGH_LCRP_NDVI_warm_ai_data", "HIGH_LCRP_NDVI_warm_pet_data", "HIGH_LCRP_NDVI_warm_ppt_data", "HIGH_LCRP_NDVI_warm_vpd_data", "HIGH_LCRP_NDVI_warm_temp_data") 

#cool
HIGH_LCRP_NDVI_cool_list<- list(HIGH_LCRP_NDVI_cool_ai_data, HIGH_LCRP_NDVI_cool_pet_data, HIGH_LCRP_NDVI_cool_ppt_data, HIGH_LCRP_NDVI_cool_vpd_data, HIGH_LCRP_NDVI_cool_temp_data)
HIGH_LCRP_NDVI_cool_bind<- do.call(rbind, HIGH_LCRP_NDVI_cool_list)
rownames(HIGH_LCRP_NDVI_cool_bind) <- c("HIGH_LCRP_NDVI_cool_ai_data", "HIGH_LCRP_NDVI_cool_pet_data", "HIGH_LCRP_NDVI_cool_ppt_data", "HIGH_LCRP_NDVI_cool_vpd_data", "HIGH_LCRP_NDVI_cool_temp_data") 

HIGH_LCRP_NDVI_bind <- rbind(HIGH_LCRP_NDVI_cool_bind,HIGH_LCRP_NDVI_warm_bind)
write.csv(HIGH_LCRP_NDVI_bind, file = "HIGH_LCRP_NDVI_bind.csv")

#NDVI
#wARM
HIGH_LCRP_EXTENT_warm_list<- list(HIGH_LCRP_EXTENT_warm_ai_data, HIGH_LCRP_EXTENT_warm_pet_data, HIGH_LCRP_EXTENT_warm_ppt_data, HIGH_LCRP_EXTENT_warm_vpd_data, HIGH_LCRP_EXTENT_warm_temp_data)
HIGH_LCRP_EXTENT_warm_bind<- do.call(rbind, HIGH_LCRP_EXTENT_warm_list)
rownames(HIGH_LCRP_EXTENT_warm_bind) <- c("HIGH_LCRP_EXTENT_warm_ai_data", "HIGH_LCRP_EXTENT_warm_pet_data", "HIGH_LCRP_EXTENT_warm_ppt_data", "HIGH_LCRP_EXTENT_warm_vpd_data", "HIGH_LCRP_EXTENT_warm_temp_data") 

#cool
HIGH_LCRP_EXTENT_cool_list<- list(HIGH_LCRP_EXTENT_cool_ai_data, HIGH_LCRP_EXTENT_cool_pet_data, HIGH_LCRP_EXTENT_cool_ppt_data, HIGH_LCRP_EXTENT_cool_vpd_data, HIGH_LCRP_EXTENT_cool_temp_data)
HIGH_LCRP_EXTENT_cool_bind<- do.call(rbind, HIGH_LCRP_EXTENT_cool_list)
rownames(HIGH_LCRP_EXTENT_cool_bind) <- c("HIGH_LCRP_EXTENT_cool_ai_data", "HIGH_LCRP_EXTENT_cool_pet_data", "HIGH_LCRP_EXTENT_cool_ppt_data", "HIGH_LCRP_EXTENT_cool_vpd_data", "HIGH_LCRP_EXTENT_cool_temp_data") 

HIGH_LCRP_EXTENT_bind <- rbind(HIGH_LCRP_EXTENT_warm_bind,HIGH_LCRP_EXTENT_cool_bind)
write.csv(HIGH_LCRP_EXTENT_bind, file = "HIGH_LCRP_EXTENT_cool_bind.csv")






#RBIND
#ppama
#LOW Market Value
#NDVI
#wARM
LOW_PPAMA_NDVI_warm_list<- list(LOW_PPAMA_NDVI_warm_ai_data, LOW_PPAMA_NDVI_warm_pet_data, LOW_PPAMA_NDVI_warm_ppt_data, LOW_PPAMA_NDVI_warm_vpd_data, LOW_PPAMA_NDVI_warm_temp_data)
LOW_PPAMA_NDVI_warm_bind<- do.call(rbind, LOW_PPAMA_NDVI_warm_list)
rownames(LOW_PPAMA_NDVI_warm_bind) <- c("LOW_PPAMA_NDVI_warm_ai_data", "LOW_PPAMA_NDVI_warm_pet_data", "LOW_PPAMA_NDVI_warm_ppt_data", "LOW_PPAMA_NDVI_warm_vpd_data", "LOW_PPAMA_NDVI_warm_temp_data") 

#cool
LOW_PPAMA_NDVI_cool_list<- list(LOW_PPAMA_NDVI_cool_ai_data, LOW_PPAMA_NDVI_cool_pet_data, LOW_PPAMA_NDVI_cool_ppt_data, LOW_PPAMA_NDVI_cool_vpd_data, LOW_PPAMA_NDVI_cool_temp_data)
LOW_PPAMA_NDVI_cool_bind<- do.call(rbind, LOW_PPAMA_NDVI_cool_list)
rownames(LOW_PPAMA_NDVI_cool_bind) <- c("LOW_PPAMA_NDVI_cool_ai_data", "LOW_PPAMA_NDVI_cool_pet_data", "LOW_PPAMA_NDVI_cool_ppt_data", "LOW_PPAMA_NDVI_cool_vpd_data", "LOW_PPAMA_NDVI_cool_temp_data") 

LOW_PPAMA_NDVI_bind <- rbind(LOW_PPAMA_NDVI_cool_bind,LOW_PPAMA_NDVI_warm_bind)
write.csv(LOW_PPAMA_NDVI_bind, file = "LOW_PPAMA_NDVI_bind.csv")

#NDVI
#wARM
LOW_PPAMA_EXTENT_warm_list<- list(LOW_PPAMA_EXTENT_warm_ai_data, LOW_PPAMA_EXTENT_warm_pet_data, LOW_PPAMA_EXTENT_warm_ppt_data, LOW_PPAMA_EXTENT_warm_vpd_data, LOW_PPAMA_EXTENT_warm_temp_data)
LOW_PPAMA_EXTENT_warm_bind<- do.call(rbind, LOW_PPAMA_EXTENT_warm_list)
rownames(LOW_PPAMA_EXTENT_warm_bind) <- c("LOW_PPAMA_EXTENT_warm_ai_data", "LOW_PPAMA_EXTENT_warm_pet_data", "LOW_PPAMA_EXTENT_warm_ppt_data", "LOW_PPAMA_EXTENT_warm_vpd_data", "LOW_PPAMA_EXTENT_warm_temp_data") 

#cool
LOW_PPAMA_EXTENT_cool_list<- list(LOW_PPAMA_EXTENT_cool_ai_data, LOW_PPAMA_EXTENT_cool_pet_data, LOW_PPAMA_EXTENT_cool_ppt_data, LOW_PPAMA_EXTENT_cool_vpd_data, LOW_PPAMA_EXTENT_cool_temp_data)
LOW_PPAMA_EXTENT_cool_bind<- do.call(rbind, LOW_PPAMA_EXTENT_cool_list)
rownames(LOW_PPAMA_EXTENT_cool_bind) <- c("LOW_PPAMA_EXTENT_cool_ai_data", "LOW_PPAMA_EXTENT_cool_pet_data", "LOW_PPAMA_EXTENT_cool_ppt_data", "LOW_PPAMA_EXTENT_cool_vpd_data", "LOW_PPAMA_EXTENT_cool_temp_data") 

LOW_PPAMA_EXTENT_bind <- rbind(LOW_PPAMA_EXTENT_cool_bind,LOW_PPAMA_EXTENT_warm_bind)
write.csv(LOW_PPAMA_EXTENT_bind, file = "LOW_PPAMA_EXTENT_bind.csv")


#LCRP
#LOW Market Value
#NDVI
#wARM
LOW_LCRP_NDVI_warm_list<- list(LOW_LCRP_NDVI_warm_ai_data, LOW_LCRP_NDVI_warm_pet_data, LOW_LCRP_NDVI_warm_ppt_data, LOW_LCRP_NDVI_warm_vpd_data, LOW_LCRP_NDVI_warm_temp_data)
LOW_LCRP_NDVI_warm_bind<- do.call(rbind, LOW_LCRP_NDVI_warm_list)
rownames(LOW_LCRP_NDVI_warm_bind) <- c("LOW_LCRP_NDVI_warm_ai_data", "LOW_LCRP_NDVI_warm_pet_data", "LOW_LCRP_NDVI_warm_ppt_data", "LOW_LCRP_NDVI_warm_vpd_data", "LOW_LCRP_NDVI_warm_temp_data") 

#cool
LOW_LCRP_NDVI_cool_list<- list(LOW_LCRP_NDVI_cool_ai_data, LOW_LCRP_NDVI_cool_pet_data, LOW_LCRP_NDVI_cool_ppt_data, LOW_LCRP_NDVI_cool_vpd_data, LOW_LCRP_NDVI_cool_temp_data)
LOW_LCRP_NDVI_cool_bind<- do.call(rbind, LOW_LCRP_NDVI_cool_list)
rownames(LOW_LCRP_NDVI_cool_bind) <- c("LOW_LCRP_NDVI_cool_ai_data", "LOW_LCRP_NDVI_cool_pet_data", "LOW_LCRP_NDVI_cool_ppt_data", "LOW_LCRP_NDVI_cool_vpd_data", "LOW_LCRP_NDVI_cool_temp_data") 

LOW_LCRP_NDVI_bind <- rbind(LOW_LCRP_NDVI_cool_bind,LOW_LCRP_NDVI_warm_bind)
write.csv(LOW_LCRP_NDVI_bind, file = "LOW_LCRP_NDVI_bind.csv")

#NDVI
#wARM
LOW_LCRP_EXTENT_warm_list<- list(LOW_LCRP_EXTENT_warm_ai_data, LOW_LCRP_EXTENT_warm_pet_data, LOW_LCRP_EXTENT_warm_ppt_data, LOW_LCRP_EXTENT_warm_vpd_data, LOW_LCRP_EXTENT_warm_temp_data)
LOW_LCRP_EXTENT_warm_bind<- do.call(rbind, LOW_LCRP_EXTENT_warm_list)
rownames(LOW_LCRP_EXTENT_warm_bind) <- c("LOW_LCRP_EXTENT_warm_ai_data", "LOW_LCRP_EXTENT_warm_pet_data", "LOW_LCRP_EXTENT_warm_ppt_data", "LOW_LCRP_EXTENT_warm_vpd_data", "LOW_LCRP_EXTENT_warm_temp_data") 

#cool
LOW_LCRP_EXTENT_cool_list<- list(LOW_LCRP_EXTENT_cool_ai_data, LOW_LCRP_EXTENT_cool_pet_data, LOW_LCRP_EXTENT_cool_ppt_data, LOW_LCRP_EXTENT_cool_vpd_data, LOW_LCRP_EXTENT_cool_temp_data)
LOW_LCRP_EXTENT_cool_bind<- do.call(rbind, LOW_LCRP_EXTENT_cool_list)
rownames(LOW_LCRP_EXTENT_cool_bind) <- c("LOW_LCRP_EXTENT_cool_ai_data", "LOW_LCRP_EXTENT_cool_pet_data", "LOW_LCRP_EXTENT_cool_ppt_data", "LOW_LCRP_EXTENT_cool_vpd_data", "LOW_LCRP_EXTENT_cool_temp_data") 

LOW_LCRP_EXTENT_bind <- rbind(LOW_LCRP_EXTENT_cool_bind,LOW_LCRP_EXTENT_warm_bind)
write.csv(LOW_LCRP_EXTENT_bind, file = "LOW_LCRP_EXTENT_bind.csv")
