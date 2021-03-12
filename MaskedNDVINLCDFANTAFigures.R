##Annual Meteorological Summaries vs NDVI
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
setwd('U:/Land Cover Change/Senior Water Rights')
gc()
removeTmpFiles()
rm(list=ls())

##Annual Growing Season NDVI
#NDVI

yumacrop <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/FANTA/yumaactive.csv")
phxcrop <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/FANTA/phxactive.csv")

yumashrub <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/FANTA/phxshrub.csv")
phxshrub <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/FANTA/yumashrub.csv")


par(new=T)
TickYear <- c(1,366,731,1096,1461)
#axis(1,at=TickYear,label=F,tck=-0.04,padj=3,font=4,lwd.ticks=2) # add col to adjust col

start_year<-2001
end_year <- 2017
years <- seq(2001,2017)
#Tickmonth <- rep(c(years),(end_year-start_year+1)) 
Monpos <- seq(1,391,23)
DOY_arr <- c(1,366,731,1096)
if(F){
  for(i in seq(1,(end_year-start_year+1),1))
  {
    Monpos[(i-1)*4 +1] <- 0 + DOY_arr[i] #March
    Monpos[(i-1)*4 +2] <- 91 + DOY_arr[i]#June
    Monpos[(i-1)*4 +3] <- 182 + DOY_arr[i]#Sep
    Monpos[(i*4)] <- 274 + DOY_arr[i]#Dec
  }
}

par(mfrow=c(2,1))
par(cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
plot.default(yumacrop$NDVI, main = "Irrigated Agriculture Area", xlab="", ylab= "NDVI",  type= "l", col = "blue", lwd = 2, ylim = c(0,0.7),xaxt='n')
lines(phxcrop$NDVI, col ="orange", lwd=2)
axis(1,at=Monpos,label=years,tck=-0.02,padj=0.1,font=1,lwd.ticks=1)# add col to adjust col
plot_colors <- c("blue","orange")
text <- c("Yuma", 
          "Phoenix")
legend('bottomleft', legend=text,col=plot_colors, lty=1, cex=1.3, pt.cex = 1,box.lty = 0)

plot(yumashrub$NDVI, main = "Shrubland", xlab="16 Day Interval (1995-2017)", ylab= "NDVI",  type= "l", col = "blue", lwd = 2, ylim = c(0,0.4),xaxt='n')
lines(phxshrub$NDVI, col ="orange", lwd=2)
axis(1,at=Monpos,label=years,tck=-0.02,padj=0.1,font=1,lwd.ticks=1)# add col to adjust col
plot_colors <- c("blue","orange")
text <- c("Yuma", 
          "Phoenix")
legend('bottomleft', legend=text,col=plot_colors, lty=1, cex=1.3, pt.cex = 1,box.lty = 0)

plot.new()
par(xpd=TRUE)
legend("center",legend = text, text.width = max(sapply(text, strwidth)),
       col=plot_colors, lwd=2, cex=1, horiz = TRUE)
par(xpd=FALSE)


#Subset
start_year<-1995
end_year <- 2017
years <- seq(1995,2017)


yumagrowingseason <- filter(yumacrop, DOY >=  97  & DOY <=  273 )
phxgrowingseason <- filter(phxcrop, DOY >=  97  & DOY <=  273 )

yumacropwarm <- subset.data.frame(yumacrop, DOY >= 273  | DOY <= 97)


yumashrubgrowingseason <- filter(yumashrub, DOY >=  97  & DOY <=  273 )
phxshrubgrowingseason <- filter(phxshrub, DOY >=  97  & DOY <=  273 )


yumashrubwarm <-  filter(yumashrub, DOY >= 273  | DOY <= 97)


yumacropwarm <- aggregate(NDVI ~ Year, data=yumagrowingseason, mean)
phxcropyearavg <- aggregate(NDVI ~ Year, data=phxgrowingseason, mean)


yumashrubwarm <- aggregate(NDVI ~ Year, data=yumashrubgrowingseason, mean)
phxshrubyearavg <- aggregate(NDVI ~ Year, data=phxshrubgrowingseason, mean)

#Climate Variables
yumaclimate <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/PRISM/YUMA_fanta_meterological_update.csv")
phxclimate <- read.csv("U:/Land Cover Change/Senior Water Rights/rcsv/PRISM/PHX_fanta_meterological_update.csv")



##Annual Cool Season Precip and Temp Filter
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


##Annual Warm Season Precip and Temp Filter
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


##Figures
#PLOT PLOT PLOT
#Timeseries
par(new=T)
TickYear <- c(1,366,731,1096,1461)
#axis(1,at=TickYear,label=F,tck=-0.04,padj=3,font=4,lwd.ticks=2) # add col to adjust col

start_year<-1995
end_year <- 2017
years <- seq(1995,2017)
#Tickmonth <- rep(c(years),(end_year-start_year+1)) 
Monpos <- seq(1,23,1)
#NDVI Growing Season
par(mfrow=c(3,2))
par(cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
plot(yumacropwarm$NDVI, main = "Irrigated Agriculture Area", xlab="1995-2017 (DOY:97-273)", ylab= "NDVI",  type= "l", col = "blue", lwd = 2, ylim = c(0.3,0.6))
lines(phxcropyearavg$NDVI, col ="orange", lwd=2)

plot(yumashrubwarm$NDVI, main = "Shrubland", xlab="1995-2017 (DOY:97-273)", ylab= "NDVI",  type= "l", col = "blue", lwd = 2, ylim = c(0.0,0.25))
lines(phxshrubyearavg$NDVI, col ="orange", lwd=2)

#Cool Season
#par(mfrow=c(3,1))
plot.default(yumaprecipcoolyearavg$ppt, main = "Precipitation", xlab="1995-2017 (M:9-4)", ylab= "Average: Cool Season (mm)",  type= "l", col = "blue", lwd = 2, ylim = c(0,40))
lines(phxprecipcoolyearavg$ppt, col ="orange", lwd=2)

plot.default(yumatempavgcoolyearavg$tavg, main = "Temperature Average", xlab="1995-2017 (DOY:9-4)", ylab= "Average (C)",  type= "l", col = "blue", lwd = 2, ylim = c(0,20))
lines(phxtempavgcoolyearavg$tavg, col ="orange", lwd=2)



#Warm Season
#warm Season
#par(mfrow=c(3,1))
plot.default(coprecipwarmyearavg$ppt, main = "Precipitation", xlab="1995-2017 (DOY:5-10)", ylab= "Average: Warm Season (mm)",  type= "l", col = "blue", lwd = 2, ylim = c(0,45))
lines(yumaprecipwarmyearavg$ppt, col = 'green', lwd =2)
lines(nmprecipwarmyearavg$ppt, col ="red", lwd=2)
lines(phxprecipwarmyearavg$ppt, col ="orange", lwd=2)


plot.default(yumatempavgwarmyearavg$tavg, main = "Temperature Average", xlab="1995-2017 (DOY:5-10)", ylab= "Average",  type= "l", col = "blue", lwd = 2, ylim = c(15,32))
lines(phxtempavgwarmyearavg$tavg, col ="orange", lwd=2)

par(mfrow=c(2,2))
plot.default(yumatempcoolyearavg$tmax, main = "Temperature Max", xlab="1995-2017 (DOY:9-4)", ylab= "Average (C): Cool Season",  type= "l", col = "blue", lwd = 2, ylim = c(5,40))
lines(phxtempcoolyearavg$tmax, col ="orange", lwd=2)

plot.default(yumaaicoolyearavg$ai, main = "P/PET", xlab="1995-2017 (DOY:9-4)", ylab= "",  type= "l", col = "blue", lwd = 2, ylim = c(0,1))
lines(phxaicoolyearavg$ai, col ="orange", lwd=2)


plot.default(yumatempwarmyearavg$tmax, main = "Temperature Max", xlab="1995-2017 (DOY:5-10)", ylab= "Average (C): Warm Season",  type= "l", col = "blue", lwd = 2, ylim = c(5,40))
lines(phxtempwarmyearavg$tmax, col ="orange", lwd=2)

plot.default(yumaaiwarmyearavg$ai, main = "P/PET", xlab="1995-2017 (DOY:5-10)", ylab= "",  type= "l", col = "blue", lwd = 2, ylim = c(0,1))
lines(phxaiwarmyearavg$ai, col ="orange", lwd=2)

#NDVI Growing Season
par(mfrow=c(3,2))
par(cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
plot(yumacropwarm$NDVI, main = "Irrigated Agriculture Area", xlab="1995-2017 (DOY:97-273)", ylab= "NDVI",  type= "l", col = "blue", lwd = 2, ylim = c(0.3,0.6))
lines(phxcropyearavg$NDVI, col ="orange", lwd=2)

plot(yumashrubwarm$NDVI, main = "Shrubland", xlab="1995-2017 (DOY:97-273)", ylab= "NDVI",  type= "l", col = "blue", lwd = 2, ylim = c(0.0,0.25))
lines(phxshrubyearavg$NDVI, col ="orange", lwd=2)

#Cool Season
#par(mfrow=c(3,1))
plot.default(yumaprecipcoolyearavg$ppt, main = "Precipitation", xlab="1995-2017 (M:9-4)", ylab= "Average: Cool Season (mm)",  type= "l", col = "blue", lwd = 2, ylim = c(0,40))
lines(phxprecipcoolyearavg$ppt, col ="orange", lwd=2)

plot.default(cotempavgcoolyearavg$tavg, main = "Temperature Average", xlab="1995-2017 (DOY:9-4)", ylab= "Average (C)",  type= "l", col = "blue", lwd = 2, ylim = c(0,20))
lines(yumatempavgcoolyearavg$tavg, col = 'green', lwd =2)
lines(nmtempavgcoolyearavg$tavg, col ="red", lwd=2)
lines(phxtempavgcoolyearavg$tavg, col ="orange", lwd=2)



#Warm Season
#warm Season
#par(mfrow=c(3,1))
plot.default(yumaprecipwarmyearavg$ppt, main = "Precipitation", xlab="1995-2017 (DOY:5-10)", ylab= "Average: Warm Season (mm)",  type= "l", col = "blue", lwd = 2, ylim = c(0,45))
lines(phxprecipwarmyearavg$ppt, col ="orange", lwd=2)


plot.default(yumatempavgwarmyearavg$tavg, main = "Temperature Average", xlab="1995-2017 (DOY:5-10)", ylab= "Average",  type= "l", col = "blue", lwd = 2, ylim = c(15,32))
lines(phxtempavgwarmyearavg$tavg, col ="orange", lwd=2)

par(mfrow=c(2,2))
plot.default(yumatempcoolyearavg$tmax, main = "Temperature Max", xlab="1995-2017 (DOY:9-4)", ylab= "Average (C): Cool Season",  type= "l", col = "blue", lwd = 2, ylim = c(5,40))
lines(phxtempcoolyearavg$tmax, col ="orange", lwd=2)

plot.default(yumaaicoolyearavg$ai, main = "P/PET", xlab="1995-2017 (DOY:9-4)", ylab= "",  type= "l", col = "blue", lwd = 2, ylim = c(0,1))
lines(phxaicoolyearavg$ai, col ="orange", lwd=2)


plot.default(yumatempwarmyearavg$tmax, main = "Temperature Max", xlab="1995-2017 (DOY:5-10)", ylab= "Average (C): Warm Season",  type= "l", col = "blue", lwd = 2, ylim = c(5,40))
lines(phxtempwarmyearavg$tmax, col ="orange", lwd=2)

plot.default(yumaaiwarmyearavg$ai, main = "P/PET", xlab="1995-2017 (DOY:5-10)", ylab= "",  type= "l", col = "blue", lwd = 2, ylim = c(0,1))
lines(phxaiwarmyearavg$ai, col ="orange", lwd=2)



plot_colors <- c("blue","green", "red", "orange")
text <- c("Colorado",  "Yuma","New Mexico", 
          "Phoenix")
plot.new()
par(xpd=TRUE)
legend("center",legend = text, text.width = max(sapply(text, strwidth)),
       col=plot_colors, lwd=2, cex=1, horiz = TRUE)
par(xpd=FALSE)



#Phoenix
##Same Plots
par(mfrow=c(2,2))
par(oma=c(0,2,2,0))
#precipitation
plot(phxcropyearavg$NDVI, phxprecipcoolyearavg$ppt, main = "Agriculture", xlab="", ylab= "Precipitation Average (mm)",  col="blue", ylim=c(0,40), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxprecipcoolyearavg$ppt ~ phxcropyearavg$NDVI), lwd=3, lty=2)
points(phxcropyearavg$NDVI, phxprecipwarmyearavg$ppt, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxprecipwarmyearavg$ppt ~ phxcropyearavg$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)
legend('bottomleft', legend=c("Warm Season", "Cool Season"),col=c("black", "black"), lty=1:2, cex=1, box.lty=0)
legend('bottomright', legend=c("Warm Season", "Cool Season"),col=c( "red","blue"),  pch=16, cex=1, box.lty=0)

mod1 = lm(phxprecipwarmyearavg$ppt ~ phxcropyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(phxprecipcoolyearavg$ppt ~ phxcropyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)



plot(phxshrubyearavg$NDVI, phxprecipcoolyearavg$ppt, main = "Shrubland", xlab="", ylab= "",  col="blue", ylim=c(0,40),  cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxprecipcoolyearavg$ppt ~ phxshrubyearavg$NDVI), lwd=3, lty=2)
points(phxshrubyearavg$NDVI, phxprecipwarmyearavg$ppt, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxprecipwarmyearavg$ppt ~ phxshrubyearavg$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(phxprecipwarmyearavg$ppt ~ phxshrubyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)

mod1 = lm(phxprecipcoolyearavg$ppt ~ phxshrubyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


title("Phoenix vs Climate", outer=TRUE,cex.main = 3)

#Temperature Avg Avg
plot(phxcropyearavg$NDVI, phxtempavgcoolyearavg$tavg, main = "", xlab="NDVI", ylab= "Average Temperature",  col="blue", ylim=c(12,35), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxtempavgcoolyearavg$tavg ~ phxcropyearavg$NDVI), lwd=3, lty=2)
points(phxcropyearavg$NDVI, phxtempavgwarmyearavg$tavg, main = "", xlab="NDVI", ylab= "Average Temperature", col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxtempavgwarmyearavg$tavg ~ phxcropyearavg$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(phxtempavgwarmyearavg$tavg ~ phxcropyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(phxtempavgcoolyearavg$tavg ~ phxcropyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)



plot(phxshrubyearavg$NDVI, phxtempavgcoolyearavg$tavg, main = "", xlab="NDVI", ylab= "",  col="blue", ylim=c(12,35), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxtempavgcoolyearavg$tavg ~ phxshrubyearavg$NDVI), lwd=3, lty=2)
points(phxshrubyearavg$NDVI, phxtempavgwarmyearavg$tavg, main = "", xlab="NDVI", ylab= "", col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxtempavgwarmyearavg$tavg ~ phxshrubyearavg$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(phxtempavgwarmyearavg$tavg ~ phxshrubyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(phxtempavgcoolyearavg$tavg ~ phxshrubyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)




#Temperature
plot(phxcropyearavg$NDVI, phxtempcoolyearavg$tmax, main = "", xlab="NDVI", ylab= "Temperature Max Average (C)",  col="blue", ylim=c(15,40), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxtempcoolyearavg$tmax ~ phxcropyearavg$NDVI), lwd=3, lty=2)
text(0.34, 30, labels = bquote("Cool Season" ~ italic(R)^2 == .(format(summary(lm(phxtempcoolyearavg$tmax ~ phxcropyearavg$NDVI))$adj.r.squared, digits = 3))))
points(phxcropyearavg$NDVI, phxtempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "Average Temperature", col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxtempwarmyearavg$tmax ~ phxcropyearavg$NDVI), lwd=3, lty=1)
text(0.34, 32, labels = bquote("Warm Season" ~italic(R)^2 == .(format(summary(lm(phxtempwarmyearavg$tmax ~ phxcropyearavg$NDVI))$adj.r.squared, digits = 3))))

plot(phxshrubyearavg$NDVI, phxtempcoolyearavg$tmax, main = "", xlab="NDVI", ylab= "",  col="blue", ylim=c(15,40),  cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxtempcoolyearavg$tmax ~ phxshrubyearavg$NDVI), lwd=3, lty=2)
text(0.15, 30, labels = bquote("Cool Season" ~ italic(R)^2 == .(format(summary(lm(phxtempcoolyearavg$tmax ~ phxshrubyearavg$NDVI))$adj.r.squared, digits = 3))))
points(phxshrubyearavg$NDVI, phxtempwarmyearavg$tmax, main = "", xlab="NDVI", ylab= "", col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxtempwarmyearavg$tmax ~ phxshrubyearavg$NDVI), lwd=3, lty=1)
text(0.15, 32, labels = bquote("Warm Season" ~italic(R)^2 == .(format(summary(lm(phxtempwarmyearavg$tmax ~ phxshrubyearavg$NDVI))$adj.r.squared, digits = 3))))















#ARIDITY INDEX
#Agriculture
par(mfrow=c(2,2))
par(oma=c(0,2,2,0))

#Yuma
#Water Balance (ppt-pet)
plot(yumacropwarm$NDVI, yumaaicoolyearavg$ai, main = "Yuma", xlab="", ylab= "P/PET: Agriculture",  col="blue", ylim=c(0,1), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaaicoolyearavg$ai ~ yumacropwarm$NDVI), lwd=3, lty=2)
points(yumacropwarm$NDVI, yumaaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaaiwarmyearavg$ai ~ yumacropwarm$NDVI), lwd=3, lty=1)
legend(0.17, 0.6, legend=c("Warm Season", "Cool Season"),col=c("black", "black"), lty=1:2, cex=1, box.lty=0)
legend(0.1735, 0.5, legend=c("Warm Season", "Cool Season"),col=c( "red","blue"),  pch=16, cex=1, box.lty=0)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumaaiwarmyearavg$ai ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(yumaaicoolyearavg$ai ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)




#Phoenix
#Water Balance (ppt-pet)
plot(phxcropyearavg$NDVI, phxaicoolyearavg$ai, main = "Phoenix", xlab="", ylab= "",  col="blue", ylim=c(0,1), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxaicoolyearavg$ai ~ phxcropyearavg$NDVI), lwd=3, lty=2)
points(phxcropyearavg$NDVI, phxaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxaiwarmyearavg$ai ~ phxcropyearavg$NDVI), lwd=3, lty=1)
#legend(0.17, 0.6, legend=c("Warm Season", "Cool Season"),col=c("black", "black"), lty=1:2, cex=0.8, box.lty=0)
#legend(0.1735, 0.5, legend=c("Warm Season", "Cool Season"),col=c( "red","blue"),  pch=16, yaxt = "none", xaxt ="none", cex=0.8, box.lty=0)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(phxaiwarmyearavg$ai ~ phxcropyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(phxaicoolyearavg$ai ~ phxcropyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)



title("Aridity Index", outer=TRUE,cex.main = 3)

#Water Balance (ppt-pet)

plot(yumashrubwarm$NDVI, yumaaicoolyearavg$ai, main = "", xlab="NDVI", ylab= "P/PET: Shrub",  col="blue", ylim=c(0,1), xlim=c(0.10,0.16), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaaicoolyearavg$ai ~ yumashrubwarm$NDVI), lwd=3, lty=2)
points(yumashrubwarm$NDVI, yumaaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaaiwarmyearavg$ai ~ yumashrubwarm$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumaaiwarmyearavg$ai ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(yumaaicoolyearavg$ai ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)




plot(phxshrubyearavg$NDVI, phxaicoolyearavg$ai, main = "", xlab="NDVI", ylab= "",  col="blue", ylim=c(0,1), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxaicoolyearavg$ai ~ phxshrubyearavg$NDVI), lwd=3, lty=2)
points(phxshrubyearavg$NDVI, phxaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(phxaiwarmyearavg$ai ~ phxshrubyearavg$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(phxaiwarmyearavg$ai ~ phxshrubyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)

mod1 = lm(phxaicoolyearavg$ai ~ phxshrubyearavg$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)







##YUMA GROWING SEASON
par(mfrow=c(3,2))
par(oma=c(0,3,2,0))

#Yuma
#Water Balance (ppt-pet)
plot(yumacropwarm$NDVI, yumaaicoolyearavg$ai, main = "Agriculture", xlab="", ylab= "Aridity Index (P/PET)",  col="blue", ylim=c(0,1), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaaicoolyearavg$ai ~ yumacropwarm$NDVI), lwd=3, lty=2)
points(yumacropwarm$NDVI, yumaaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaaiwarmyearavg$ai ~ yumacropwarm$NDVI), lwd=3, lty=1)
legend('bottomleft', legend=c("Warm Season", "Cool Season"),col=c("black", "black"), lty=1:2, cex=0.8, box.lty=0)
legend('bottomright', legend=c("Warm Season", "Cool Season"),col=c( "red","blue"),  pch=16, yaxt = "none", xaxt ="none", cex=0.8, box.lty=0)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumaaiwarmyearavg$ai ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(yumaaicoolyearavg$ai ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


plot(yumashrubwarm$NDVI, yumaaicoolyearavg$ai, main = "Shrubland", xlab="", ylab= "",  col="blue", ylim=c(0,1),  cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaaicoolyearavg$ai ~ yumashrubwarm$NDVI), lwd=3, lty=2)
points(yumashrubwarm$NDVI, yumaaiwarmyearavg$ai, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaaiwarmyearavg$ai ~ yumashrubwarm$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumaaiwarmyearavg$ai ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(yumaaicoolyearavg$ai ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)




#precipitation
plot(yumacropwarm$NDVI, yumaprecipcoolyearavg$ppt, main = "", xlab="", ylab= "Precipitation Average (mm)",  col="blue", ylim=c(0,20), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaprecipcoolyearavg$ppt ~ yumacropwarm$NDVI), lwd=3, lty=2)
points(yumacropwarm$NDVI, yumaprecipwarmyearavg$ppt, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaprecipwarmyearavg$ppt ~ yumacropwarm$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumaprecipwarmyearavg$ppt ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(yumaprecipcoolyearavg$ppt ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


plot(yumashrubwarm$NDVI, yumaprecipcoolyearavg$ppt, main = "", xlab="", ylab= "",  col="blue", ylim=c(0,20), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaprecipcoolyearavg$ppt ~ yumashrubwarm$NDVI), lwd=3, lty=2)
points(yumashrubwarm$NDVI, yumaprecipwarmyearavg$ppt, col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumaprecipwarmyearavg$ppt ~ yumashrubwarm$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumaprecipwarmyearavg$ppt ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(yumaprecipcoolyearavg$ppt ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)



#Temperature Avg Avg
plot(yumacropwarm$NDVI, yumatempavgcoolyearavg$tavg, main = "", xlab="NDVI", ylab= "Temperatature Average Average (C)",  col="blue", ylim=c(15,35),  cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumatempavgcoolyearavg$tavg ~ yumacropwarm$NDVI), lwd=3, lty=2)
points(yumacropwarm$NDVI, yumatempavgwarmyearavg$tavg, main = "", xlab="NDVI", ylab= "Average Temperature", col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumatempavgwarmyearavg$tavg ~ yumacropwarm$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumatempavgwarmyearavg$tavg ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(umatempavgcoolyearavg$tavg ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


plot(yumashrubwarm$NDVI, yumatempavgcoolyearavg$tavg, main = "", xlab="NDVI", ylab= "",  col="blue", ylim=c(15,40),  cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumatempavgcoolyearavg$tavg ~ yumashrubwarm$NDVI), lwd=3, lty=2)
points(yumashrubwarm$NDVI, yumatempavgwarmyearavg$tavg, main = "", xlab="NDVI", ylab= "", col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumatempavgwarmyearavg$tavg ~ yumashrubwarm$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumatempavgwarmyearavg$tavg ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)

mod1 = lm(yumatempavgcoolyearavg$tavg ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


title("Yuma vs Climate", outer=TRUE,cex.main = 3)

















#Temperature
plot(yumacropwarm$NDVI, yumatempcoolyearavg$tmax, main = "", xlab="NDVI", ylab= "Temperature Max Average (C)",  col="blue", ylim=c(22,40),  cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumatempcoolyearavg$tmax ~ yumacropwarm$NDVI), lwd=3, lty=2)
points(yumacropwarm$NDVI, yumatempwarmyearavg$tmax, main = "", xlab="", ylab= "Average Temperature", col="red",ylim=c(22,40), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumatempwarmyearavg$tmax ~ yumacropwarm$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumatempwarmyearavg$tmax ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(yumatempcoolyearavg$tmax ~ yumacropwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)


plot(yumashrubwarm$NDVI, yumatempcoolyearavg$tmax, main = "", xlab="", ylab= "",  col="blue", ylim=c(21,40), cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumatempcoolyearavg$tmax ~ yumashrubwarm$NDVI), lwd=3, lty=2)
points(yumashrubwarm$NDVI, yumatempwarmyearavg$tmax, main = "", xlab="", ylab= "", col="red", cex.lab = 1.5, cex.main=1.5, bty = "o",  pch=16, yaxt = "none", xaxt ="none")
abline(lm(yumatempwarmyearavg$tmax ~ yumashrubwarm$NDVI), lwd=3, lty=1)
axis(2,las=2, font=4)
axis(1, font=3)

mod1 = lm(yumatempwarmyearavg$tmax ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Warm Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Warm Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Warm Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topright', legend = rp, bty = 'n', cex=1.5)


mod1 = lm(yumatempcoolyearavg$tmax ~ yumashrubwarm$NDVI)
modsum = summary(mod1)
r2 = modsum$adj.r.squared
my.p = modsum$coefficients[2,4]
my.slope = modsum$coefficients[2]
rp = vector('expression',3)
rp[1] = substitute(expression("Cool Season" ~ italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression("Cool Season" ~ italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]
rp[3] = substitute(expression("Cool Season" ~ italic(slope) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.slope, digits = 2)))[2]
legend('topleft', legend = rp, bty = 'n', cex=1.5)





##Timeseries
ts (inputData, frequency = 4, start = c(1959, 2)) # frequency 4 => Quarterly Data
ts (1:10, frequency = 12, start = 1990) # freq 12 => Monthly data. 
ts (inputData, start=c(2009), end=c(2014), frequency=1) # Yearly Data