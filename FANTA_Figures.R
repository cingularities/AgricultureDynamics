##FIGURES
##Annual Meteorological Summaries vs NDVI
install.packages("raster")
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)

setwd('U:/Land Cover Change/Senior Water Rights/FANTA')


gc()
removeTmpFiles()
rm(list=ls())

#NDVI
phxactive <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/FANTA/phxactive.csv")
phxfallow <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/FANTA/phxfallow.csv")
yumaactive <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/FANTA/yumaactive.csv")
yumafallow <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/FANTA/yumafallow.csv")


par(mfrow=c(2,1))
par(cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
plot.default(phxactive$NDVI, main = "Phoenix NDVI Fanta", xlab="16 Day Interval (2001-2016)", ylab= "NDVI",  type= "l", col = "blue", lwd = 2, lty=5, ylim = c(0,0.7))
lines(phxfallow$NDVI, col = 'red', lwd =2, lty=5)

plot.default(yumaactive$NDVI, main = "Yuma NDVI Fanta", xlab="16 Day Interval (2001-2016)", ylab= "NDVI",  type= "l", col = "blue", lwd = 2, ylim = c(0,0.7))
lines(yumafallow$NDVI, col ="red", lwd=2)
legend("topleft", legend=c("Yuma Fallow", "Phx Fallow","Yuma Active", "Phx Active"),
       col=c("red", "red", "blue", "blue"), lty=1:2, cex=0.8)


#Subset
start_year<-1995
end_year <- 2017
years <- seq(1995,2017)

yumagrowingactive <- filter(yumaactive, DOY >= 97 & DOY <= 273)
phxgrowingactive <- filter(phxactive, DOY >=  97  & DOY <=  273)

yumaactivewarm <- subset.data.frame(yumaactive, DOY >= 273  | DOY <= 97)

yumafallowgrowingactive <- filter(yumafallow, DOY >=  97  & DOY <=  273)
phxfallowgrowingactive <- filter(phxfallow, DOY >=  97  & DOY <=  273)


yumafallowwarm <-  filter(yumafallow, DOY >= 273  | DOY <= 97)


yumaactivewarm <- aggregate(NDVI ~ Year, data=yumagrowingactive, mean)
phxactiveyearavg <- aggregate(NDVI ~ Year, data=phxgrowingactive, mean)

yumafallowwarm <- aggregate(NDVI ~ Year, data=yumafallowgrowingactive, mean)
phxfallowyearavg <- aggregate(NDVI ~ Year, data=phxfallowgrowingactive, mean)


#Climate Variables
phxclimate <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/PRISM/PHX_fanta_meterological.csv")
yumaclimate <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/PRISM/YUMA_fanta_meterological.csv")



##Annual Cool active Precip and Temp Filter
#Precip
yumacool <- filter(yumaclimate, Month <=  4  | Month >=  9 )
phxcool <- filter(phxclimate, Month <=  4  | Month >=  9 )


##Aggregate
#Precip
yumaprecipcoolyearavg <- aggregate(ppt ~ Year, data=yumacool, mean, na.rm=TRUE, na.action=NULL)
phxprecipcoolyearavg <- aggregate(ppt ~ Year, data=phxcool, mean, na.rm=TRUE, na.action=NULL)

#Max Temp
yumatempcoolyearavg <- aggregate(tmax ~ Year, data=yumacool, mean, na.rm=TRUE, na.action=NULL)
phxtempcoolyearavg <- aggregate(tmax~ Year, data=phxcool, mean, na.rm=TRUE, na.action=NULL)

#Water Balance (ppt-pet)
yumaaicoolyearavg <- aggregate(ai ~ Year, data=yumacool, mean, na.rm=TRUE, na.action=NULL)
phxaicoolyearavg <- aggregate(ai ~ Year, data=phxcool, mean, na.rm=TRUE, na.action=NULL)

#Avg Temp
yumatempavgcoolyearavg <- aggregate(tavg ~ Year, data=yumacool, mean, na.rm=TRUE, na.action=NULL)
phxtempavgcoolyearavg <- aggregate(tavg~ Year, data=phxcool, mean, na.rm=TRUE, na.action=NULL)


##Annual Warm active Precip and Temp Filter
#Precip
yumawarm <- filter(yumaclimate, Month >=  5  & Month <=  10 )
phxwarm <- filter(phxclimate, Month >=  5  & Month <=  10 )


##Aggregate
#Precip
yumaprecipwarmyearavg <- aggregate(ppt ~ Year, data=yumawarm, mean, na.rm=TRUE, na.action=NULL)
phxprecipwarmyearavg <- aggregate(ppt ~ Year, data=phxwarm, mean, na.rm=TRUE, na.action=NULL)

#Temp
yumatempwarmyearavg <- aggregate(tmax ~ Year, data=yumawarm, mean, na.rm=TRUE, na.action=NULL)
phxtempwarmyearavg <- aggregate(tmax ~ Year, data=phxwarm, mean, na.rm=TRUE, na.action=NULL)

#Water Balance (ppt-pet)
yumaaiwarmyearavg <- aggregate(ai ~ Year, data=yumawarm, mean, na.rm=TRUE, na.action=NULL)
phxaiwarmyearavg <- aggregate(ai ~ Year, data=phxwarm, mean, na.rm=TRUE, na.action=NULL)

#Avg Temp
yumatempavgwarmyearavg <- aggregate(tavg ~ Year, data=yumawarm, mean, na.rm=TRUE, na.action=NULL)
phxtempavgwarmyearavg <- aggregate(tavg~ Year, data=phxwarm, mean, na.rm=TRUE, na.action=NULL)





#Weather Regressions
par(mfrow=c(2,2))
par(oma=c(0,2,2,0))
#Yuma
#precip Cool Weather
plot(yumaactivewarm$NDVI, yumaprecipcoolyearavg$ppt, main = "Active",xlab="", ylab= "Sum precip: Cool Weather", col="green", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaprecipcoolyearavg$ppt ~ yumaactivewarm$NDVI))
text(0.4, 10, labels = bquote(italic(R)^2 == .(format(summary(lm(yumaprecipcoolyearavg$ppt ~ yumaactivewarm$NDVI))$adj.r.squared, digits = 3))))
plot(yumafallowwarm$NDVI, yumaprecipcoolyearavg$ppt, main = "Fallow",xlab="",ylab = "", col="green", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaprecipcoolyearavg$ppt ~ yumafallowwarm$NDVI))
text(0.26,10, labels = bquote(italic(R)^2 == .(format(summary(lm(yumaprecipcoolyearavg$ppt ~ yumafallowwarm$NDVI))$adj.r.squared, digits = 3))))


#precip Warm Weather
plot(yumaactivewarm$NDVI, yumaprecipwarmyearavg$ppt, main = "", xlab="NDVI", ylab= "Sum precip: Warm Weather", col="green", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaprecipwarmyearavg$ppt ~ yumaactivewarm$NDVI))
text(0.24, 3, labels = bquote(italic(R)^2 == .(format(summary(lm(yumaprecipwarmyearavg$ppt ~ yumaactivewarm$NDVI))$adj.r.squared, digits = 3))))
plot(yumafallowwarm$NDVI, yumaprecipwarmyearavg$ppt, main = "", xlab="NDVI", ylab= "", col="green", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaprecipwarmyearavg$ppt ~ yumafallowwarm$NDVI))
text(0.12,3, labels = bquote(italic(R)^2 == .(format(summary(lm(yumaprecipwarmyearavg$ppt ~ yumafallowwarm$NDVI))$adj.r.squared, digits = 3))))
title("Yuma: precipitation", outer = TRUE)


par(mfrow=c(2,2))
par(oma=c(0,2,2,0))
#Temp Cool Weather
plot(yumaactivewarm$NDVI, yumatempcoolyearavg$tmax, main = "Agriculture Area", xlab="", ylab=  "Average Temp: Cool Weather", col="green", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempcoolyearavg$tmax ~ yumaactivewarm$NDVI))
text(0.24, 80, labels = bquote(italic(R)^2 == .(format(summary(lm(yumatempcoolyearavg$tmax ~ yumaactivewarm$NDVI))$adj.r.squared, digits = 3))))
plot(yumafallowwarm$NDVI, yumatempcoolyearavg$tmax, main = "fallowland", xlab="", ylab= "", col="green", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempcoolyearavg$tmax ~ yumafallowwarm$NDVI))
text(0.12,80, labels = bquote(italic(R)^2 == .(format(summary(lm(yumatempcoolyearavg$tmax ~ yumafallowwarm$NDVI))$adj.r.squared, digits = 3))))

#Temp Warm Weather
plot(yumaactivewarm$NDVI, yumatempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "Average Temp: Warm Weather", col="green", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempwarmyearavg$tmax ~ yumaactivewarm$NDVI))
text(0.24,91, labels = bquote(italic(R)^2 == .(format(summary(lm(yumatempwarmyearavg$tmax ~ yumaactivewarm$NDVI))$adj.r.squared, digits = 3))))
plot(yumafallowwarm$NDVI, yumatempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "", col="green", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempwarmyearavg$tmax ~ yumafallowwarm$NDVI))
text(0.12,91, labels = bquote(italic(R)^2 == .(format(summary(lm(yumatempwarmyearavg$tmax ~ yumafallowwarm$NDVI))$adj.r.squared, digits = 3))))
title("Yuma:Temperature", outer=TRUE)

par(mfrow=c(2,2))
par(oma=c(0,2,2,0))
#Phoenix
#precip Cool Weather
plot(phxactiveyearavg$NDVI, phxprecipcoolyearavg$ppt, main = "Agriculture Area",xlab="", ylab= "Sum precip: Cool Weather", col="orange", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxprecipcoolyearavg$ppt ~ phxactiveyearavg$NDVI))
text(0.20, 3, labels = bquote(italic(R)^2 == .(format(summary(lm(phxprecipcoolyearavg$ppt ~ phxactiveyearavg$NDVI))$adj.r.squared, digits = 3))))
plot(phxfallowyearavg$NDVI, phxprecipcoolyearavg$ppt, main = "fallowland",xlab="",ylab = "", col="orange", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxprecipcoolyearavg$ppt ~ phxfallowyearavg$NDVI))
text(0.16,3, labels = bquote(italic(R)^2 == .(format(summary(lm(phxprecipcoolyearavg$ppt ~ phxfallowyearavg$NDVI))$adj.r.squared, digits = 3))))


#precip Warm Weather
plot(phxactiveyearavg$NDVI, phxprecipwarmyearavg$ppt, main = "", xlab="NDVI", ylab= "Sum precip: Warm Weather", col="orange", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxprecipwarmyearavg$ppt ~ phxactiveyearavg$NDVI))
text(0.20, 5, labels = bquote(italic(R)^2 == .(format(summary(lm(phxprecipwarmyearavg$ppt ~ phxactiveyearavg$NDVI))$adj.r.squared, digits = 3))))
plot(phxfallowyearavg$NDVI, phxprecipwarmyearavg$ppt, main = "", xlab="NDVI", ylab= "", col="orange", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxprecipwarmyearavg$ppt ~ phxfallowyearavg$NDVI))
text(0.16,5, labels = bquote(italic(R)^2 == .(format(summary(lm(phxprecipwarmyearavg$ppt ~ phxfallowyearavg$NDVI))$adj.r.squared, digits = 3))))
title("Phoenix: precipitation", outer = TRUE)



par(oma=c(0,2,2,0))
#Temp Cool Weather
plot(phxactiveyearavg$NDVI, phxtempcoolyearavg$tmax, main = "Agriculture Area", xlab="", ylab=  "Average Temp: Cool Weather", col="orange", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempcoolyearavg$tmax ~ phxactiveyearavg$NDVI))
text(0.20, 70, labels = bquote(italic(R)^2 == .(format(summary(lm(phxtempcoolyearavg$tmax ~ phxactiveyearavg$NDVI))$adj.r.squared, digits = 3))))
plot(phxfallowyearavg$NDVI, phxtempcoolyearavg$tmax, main = "fallowland", xlab="", ylab= "", col="orange", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempcoolyearavg$tmax ~ phxfallowyearavg$NDVI))
text(0.16,70, labels = bquote(italic(R)^2 == .(format(summary(lm(phxtempcoolyearavg$tmax ~ phxfallowyearavg$NDVI))$adj.r.squared, digits = 3))))

#Temp Warm Weather
plot(phxactiveyearavg$NDVI, phxtempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "Average Temp: Warm Weather", col="orange", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempwarmyearavg$tmax ~ phxactiveyearavg$NDVI))
text(0.20, 88, labels = bquote(italic(R)^2 == .(format(summary(lm(phxtempwarmyearavg$tmax ~ phxactiveyearavg$NDVI))$adj.r.squared, digits = 3))))
plot(phxfallowyearavg$NDVI, phxtempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "", col="orange", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempwarmyearavg$tmax ~ phxfallowyearavg$NDVI))
text(0.16,88, labels = bquote(italic(R)^2 == .(format(summary(lm(phxtempwarmyearavg$tmax ~ phxfallowyearavg$NDVI))$adj.r.squared, digits = 3))))
title("Phoenix:Temperature", outer=TRUE)









##Same Plots

#Yuma
##Same Plots
par(mfrow=c(2,2))
par(oma=c(0,2,2,0))
#precipitation
plot(yumaactivewarm$NDVI, yumaprecipcoolyearavg$ppt, main = "Active", xlab="", ylab= "Precipitation Average (mm)",  col="blue", ylim=c(0,20), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaprecipcoolyearavg$ppt ~ yumaactivewarm$NDVI), lwd=3, lty=2)
points(yumaactivewarm$NDVI, yumaprecipwarmyearavg$ppt, col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaprecipwarmyearavg$ppt ~ yumaactivewarm$NDVI), lwd=3, lty=1)
legend('bottomleft', legend=c("Warm active", "Cool active"),col=c("black", "black"), lty=1:2, cex=0.8, box.lty=0)
legend('bottomright', legend=c("Warm active", "Cool active"),col=c( "red","blue"), pch=16, cex=0.8, box.lty=0)

mod1 = lm(yumaprecipwarmyearavg$ppt ~ yumaactivewarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(yumaprecipcoolyearavg$ppt ~ yumaactivewarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')



plot(yumafallowwarm$NDVI, yumaprecipcoolyearavg$ppt, main = "Fallow", xlab="", ylab= "",  col="blue", ylim=c(0,20), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaprecipcoolyearavg$ppt ~ yumafallowwarm$NDVI), lwd=3, lty=2)
points(yumafallowwarm$NDVI, yumaprecipwarmyearavg$ppt, col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaprecipwarmyearavg$ppt ~ yumafallowwarm$NDVI), lwd=3, lty=1)

mod1 = lm(yumaprecipwarmyearavg$ppt ~ yumafallowwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(yumaprecipcoolyearavg$ppt ~ yumafallowwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')


title("Yuma", outer=TRUE,cex=0.8)


#Temperature Avg Avg
plot(yumaactivewarm$NDVI, yumatempavgcoolyearavg$tavg, main = "", xlab="NDVI", ylab= "Average Temperature",  col="blue", ylim=c(15,35), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempavgcoolyearavg$tavg ~ yumaactivewarm$NDVI), lwd=3, lty=2)
points(yumaactivewarm$NDVI, yumatempavgwarmyearavg$tavg, main = "", xlab="NDVI", ylab= "Average Temperature", col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempavgwarmyearavg$tavg ~ yumaactivewarm$NDVI), lwd=3, lty=1)
#legend(0.23, -170, legend=c("Warm active", "Cool active"),col=c("black", "black"), lty=1:2, cex=0.8, box.lty=0)
#legend(0.1, -155, legend=c("Warm active", "Cool active"),col=c( "red","blue"), pch=16, cex=0.8, box.lty=0)

mod1 = lm(yumatempavgwarmyearavg$tavg ~ yumaactivewarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(yumatempavgcoolyearavg$tavg ~ yumaactivewarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')


plot(yumafallowwarm$NDVI, yumatempavgcoolyearavg$tavg, main = "", xlab="NDVI", ylab= "",  col="blue", ylim=c(15,40), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempavgcoolyearavg$tavg ~ yumafallowwarm$NDVI), lwd=3, lty=2)
points(yumafallowwarm$NDVI, yumatempavgwarmyearavg$tavg, main = "", xlab="NDVI", ylab= "", col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempavgwarmyearavg$tavg ~ yumafallowwarm$NDVI), lwd=3, lty=1)

mod1 = lm(yumatempavgwarmyearavg$tavg ~ yumafallowwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(yumatempavgcoolyearavg$tavg ~ yumafallowwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')




#Temperature
plot(yumaactivewarm$NDVI, yumatempcoolyearavg$tmax, main = "", xlab="NDVI", ylab= "Temperature Max Average (C)",  col="blue", ylim=c(22,40), xlim=c(0.3,0.5), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempcoolyearavg$tmax ~ yumaactivewarm$NDVI), lwd=3, lty=2)
text(0.35, 35, labels = bquote("Cool active" ~ italic(R)^2 == .(format(summary(lm(yumatempcoolyearavg$tmax ~ yumaactivewarm$NDVI))$adj.r.squared, digits = 3))))
points(yumaactivewarm$NDVI, yumatempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "Average Temperature", col="red",ylim=c(22,40), xlim=c(0.3,0.5), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempwarmyearavg$tmax ~ yumaactivewarm$NDVI), lwd=3, lty=1)
text(0.35, 36.5, labels = bquote("Warm active" ~italic(R)^2 == .(format(summary(lm(yumatempwarmyearavg$tmax ~ yumaactivewarm$NDVI))$adj.r.squared, digits = 3))))


plot(yumafallowwarm$NDVI, yumatempcoolyearavg$tmax, main = "", xlab="NDVI", ylab= "",  col="blue", ylim=c(21,40),xlim=c(0.1,0.13), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempcoolyearavg$tmax ~ yumafallowwarm$NDVI), lwd=3, lty=2)
text(0.10, 35, labels = bquote("Cool active" ~ italic(R)^2 == .(format(summary(lm(yumatempcoolyearavg$tmax ~ yumafallowwarm$NDVI))$adj.r.squared, digits = 3))))
points(yumafallowwarm$NDVI, yumatempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "", col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumatempwarmyearavg$tmax ~ yumafallowwarm$NDVI), lwd=3, lty=1)
text(0.10, 36.5, labels = bquote("Warm active" ~italic(R)^2 == .(format(summary(lm(yumatempwarmyearavg$tmax ~ yumafallowwarm$NDVI))$adj.r.squared, digits = 3))))





#Phoenix
##Same Plots
par(mfrow=c(2,2))
par(oma=c(0,2,2,0))
#precipitation
plot(phxactiveyearavg$NDVI, phxprecipcoolyearavg$ppt, main = "Agriculture", xlab="", ylab= "Precipitation Average (mm)",  col="blue", ylim=c(0,40), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxprecipcoolyearavg$ppt ~ phxactiveyearavg$NDVI), lwd=3, lty=2)
points(phxactiveyearavg$NDVI, phxprecipwarmyearavg$ppt, col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxprecipwarmyearavg$ppt ~ phxactiveyearavg$NDVI), lwd=3, lty=1)
legend('bottomleft', legend=c("Warm active", "Cool active"),col=c("black", "black"), lty=1:2, cex=0.8, box.lty=0)
legend('bottomright', legend=c("Warm active", "Cool active"),col=c( "red","blue"), pch=16, cex=0.8, box.lty=0)

mod1 = lm(phxprecipwarmyearavg$ppt ~ phxactiveyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(phxprecipcoolyearavg$ppt ~ phxactiveyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')



plot(phxfallowyearavg$NDVI, phxprecipcoolyearavg$ppt, main = "fallowland", xlab="", ylab= "",  col="blue", ylim=c(0,40),  cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxprecipcoolyearavg$ppt ~ phxfallowyearavg$NDVI), lwd=3, lty=2)
points(phxfallowyearavg$NDVI, phxprecipwarmyearavg$ppt, col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxprecipwarmyearavg$ppt ~ phxfallowyearavg$NDVI), lwd=3, lty=1)

mod1 = lm(phxprecipwarmyearavg$ppt ~ phxfallowyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(phxprecipcoolyearavg$ppt ~ phxfallowyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')

title("Phoenix", outer=TRUE,cex=0.8)

#Temperature Avg Avg
plot(phxactiveyearavg$NDVI, phxtempavgcoolyearavg$tavg, main = "", xlab="NDVI", ylab= "Average Temperature",  col="blue", ylim=c(12,35), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempavgcoolyearavg$tavg ~ phxactiveyearavg$NDVI), lwd=3, lty=2)
points(phxactiveyearavg$NDVI, phxtempavgwarmyearavg$tavg, main = "", xlab="NDVI", ylab= "Average Temperature", col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempavgwarmyearavg$tavg ~ phxactiveyearavg$NDVI), lwd=3, lty=1)

mod1 = lm(phxtempavgwarmyearavg$tavg ~ phxactiveyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(phxtempavgcoolyearavg$tavg ~ phxactiveyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')


plot(phxfallowyearavg$NDVI, phxtempavgcoolyearavg$tavg, main = "", xlab="NDVI", ylab= "",  col="blue", ylim=c(12,35), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempavgcoolyearavg$tavg ~ phxfallowyearavg$NDVI), lwd=3, lty=2)
points(phxfallowyearavg$NDVI, phxtempavgwarmyearavg$tavg, main = "", xlab="NDVI", ylab= "", col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempavgwarmyearavg$tavg ~ phxfallowyearavg$NDVI), lwd=3, lty=1)

mod1 = lm(phxtempavgwarmyearavg$tavg ~ phxfallowyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(phxtempavgcoolyearavg$tavg ~ phxfallowyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')

title("Phoenix", outer=TRUE,cex=0.8)

#Temperature
plot(phxactiveyearavg$NDVI, phxtempcoolyearavg$tmax, main = "", xlab="NDVI", ylab= "Temperature Max Average (C)",  col="blue", ylim=c(15,40), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempcoolyearavg$tmax ~ phxactiveyearavg$NDVI), lwd=3, lty=2)
text(0.34, 30, labels = bquote("Cool active" ~ italic(R)^2 == .(format(summary(lm(phxtempcoolyearavg$tmax ~ phxactiveyearavg$NDVI))$adj.r.squared, digits = 3))))
points(phxactiveyearavg$NDVI, phxtempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "Average Temperature", col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempwarmyearavg$tmax ~ phxactiveyearavg$NDVI), lwd=3, lty=1)
text(0.34, 32, labels = bquote("Warm active" ~italic(R)^2 == .(format(summary(lm(phxtempwarmyearavg$tmax ~ phxactiveyearavg$NDVI))$adj.r.squared, digits = 3))))

plot(phxfallowyearavg$NDVI, phxtempcoolyearavg$tmax, main = "", xlab="NDVI", ylab= "",  col="blue", ylim=c(15,40),  cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempcoolyearavg$tmax ~ phxfallowyearavg$NDVI), lwd=3, lty=2)
text(0.15, 30, labels = bquote("Cool active" ~ italic(R)^2 == .(format(summary(lm(phxtempcoolyearavg$tmax ~ phxfallowyearavg$NDVI))$adj.r.squared, digits = 3))))
points(phxfallowyearavg$NDVI, phxtempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "", col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxtempwarmyearavg$tmax ~ phxfallowyearavg$NDVI), lwd=3, lty=1)
text(0.15, 32, labels = bquote("Warm active" ~italic(R)^2 == .(format(summary(lm(phxtempwarmyearavg$tmax ~ phxfallowyearavg$NDVI))$adj.r.squared, digits = 3))))















#ARIDITY INDEX
#Agriculture
par(mfrow=c(2,2))
par(oma=c(0,2,2,0))

#Yuma
#Water Balance (ppt-pet)
plot(yumaactivewarm$NDVI, yumaaicoolyearavg$ai, main = "Yuma", xlab="", ylab= "P/PET",  col="blue", ylim=c(0,1), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaaicoolyearavg$ai ~ yumaactivewarm$NDVI), lwd=3, lty=2)
points(yumaactivewarm$NDVI, yumaaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaaiwarmyearavg$ai ~ yumaactivewarm$NDVI), lwd=3, lty=1)
#legend(0.17, 0.6, legend=c("Warm active", "Cool active"),col=c("black", "black"), lty=1:2, cex=0.8, box.lty=0)
#legend(0.1735, 0.5, legend=c("Warm active", "Cool active"),col=c( "red","blue"), pch=16, cex=0.8, box.lty=0)


mod1 = lm(yumaaiwarmyearavg$ai ~ yumaactivewarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(yumaaicoolyearavg$ai ~ yumaactivewarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')



#Phoenix
#Water Balance (ppt-pet)
plot(phxactiveyearavg$NDVI, phxaicoolyearavg$ai, main = "Phoenix", xlab="", ylab= "P/PET",  col="blue", ylim=c(0,1), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxaicoolyearavg$ai ~ phxactiveyearavg$NDVI), lwd=3, lty=2)
points(phxactiveyearavg$NDVI, phxaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxaiwarmyearavg$ai ~ phxactiveyearavg$NDVI), lwd=3, lty=1)
#legend(0.17, 0.6, legend=c("Warm active", "Cool active"),col=c("black", "black"), lty=1:2, cex=0.8, box.lty=0)
#legend(0.1735, 0.5, legend=c("Warm active", "Cool active"),col=c( "red","blue"), pch=16, cex=0.8, box.lty=0)

mod1 = lm(phxaiwarmyearavg$ai ~ phxactiveyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(phxaicoolyearavg$ai ~ phxactiveyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')



title("Agriculture Aridity Index", outer=TRUE,cex=0.8)

#Water Balance (ppt-pet)

#fallowland


plot(yumafallowwarm$NDVI, yumaaicoolyearavg$ai, main = "Yuma", xlab="NDVI", ylab= "P/PET",  col="blue", ylim=c(0,1), xlim=c(0.10,0.16), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaaicoolyearavg$ai ~ yumafallowwarm$NDVI), lwd=3, lty=2)
points(yumafallowwarm$NDVI, yumaaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(yumaaiwarmyearavg$ai ~ yumafallowwarm$NDVI), lwd=3, lty=1)

mod1 = lm(yumaaiwarmyearavg$ai ~ yumafallowwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(yumaaicoolyearavg$ai ~ yumafallowwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')



plot(phxfallowyearavg$NDVI, phxaicoolyearavg$ai, main = "Phoenix", xlab="NDVI", ylab= "",  col="blue", ylim=c(0,1), cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxaicoolyearavg$ai ~ phxfallowyearavg$NDVI), lwd=3, lty=2)
points(phxfallowyearavg$NDVI, phxaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty ="n", pch=16)
abline(lm(phxaiwarmyearavg$ai ~ phxfallowyearavg$NDVI), lwd=3, lty=1)


mod1 = lm(phxaiwarmyearavg$ai ~ phxfallowyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Warm active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n')

mod1 = lm(phxaicoolyearavg$ai ~ phxfallowyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
rp = vector('expression',2)
rp[1] = substitute(expression("Cool active" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool active" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n')

title("fallowland Aridity Index", outer=TRUE,cex=0.8)

