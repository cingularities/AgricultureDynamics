gc()
removeTmpFiles()
rm(list=ls())


library(raster)
library(rgdal)
library(ggplot2)
library(maptools)
library(dplyr)
library(readr)



highmarketvalue <- read.csv("E:/Senior Water Rights/rcsv/HighMarketValue.csv")
lowmarketvalue <- read.csv("E:/Senior Water Rights/rcsv/LowMarketValue.csv")

windows()
##MarketAnomaly Years and Extent 
par(mar = c(5, 5, 3, 5))
plot(lowmarketvalue$Years, lowmarketvalue$Percent.Active.Phx, ylab = "% Active Extent",
     main = "", xlab = "Years",
     col = "blue",cex.lab = 2,  cex.main=1.5, bty ="o", pch=18, cex=3, cex.axis = 1.5, ylim=c(0.75,0.85))


mod1 = lm(lowmarketvalue$Percent.Active.Phx~ lowmarketvalue$Years)
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

points(highmarketvalue$Years, highmarketvalue$Percent.Active.Phx, ylim= c(0.75,0.92), ylab = "% Active Extent",
     main = "", xlab = "Years",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)
abline(lm(highmarketvalue$Percent.Active.Phx ~ highmarketvalue$Years), lwd=3, lty=2,col = "orange")


mod1 = lm(highmarketvalue$Percent.Active.Phx ~ highmarketvalue$Years)
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
legend("topright", legend = rp, bty = 'n', cex=1.5, text.col = "orange")







par(mar = c(5, 5, 3, 5))
points(lowmarketvalue$Years, lowmarketvalue$Percent.Active.Yuma, ylab = "% Active Crop",
     main = "", xlab = "NDVI",
     col = "blue",cex.lab = 2,  cex.main=1.5, bty ="o", pch=18, cex=3, cex.axis = 1.5)


mod1 = lm(lowmarketvalue$Percent.Active.Yuma~ lowmarketvalue$Years)
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

plot(highmarketvalue$Years, highmarketvalue$Percent.Active.Yuma, ylab = "% Active Crop",
       main = "", xlab = "NDVI",
       col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)
abline(lm(highmarketvalue$Percent.Active.Yuma ~ highmarketvalue$Years), lwd=3, lty=2,col = "orange")


mod1 = lm(highmarketvalue$Percent.Active.Yuma ~ highmarketvalue$Years)
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
legend("topright", legend = rp, bty = 'n', cex=1.5, text.col = "orange")


##Market Value Correlation
par(mar = c(5, 5, 3, 5))
plot(lowmarketvalue$Market.Value.Anomaly, lowmarketvalue$Percent.Active.Phx, ylab = "% Active Extent",
     main = "", xlab = "Low Demand Anomaly",
     col = "aquamarine4",cex.lab = 2,  cex.main=1.5, bty ="o", pch=17, cex=3, cex.axis = 1.5, ylim=c(0.75,0.91))
abline(lm(lowmarketvalue$Percent.Active.Phx~ lowmarketvalue$Market.Value.Anomaly), lwd=3, lty=2,col = "aquamarine4")

points(lowmarketvalue$Market.Value.Anomaly, lowmarketvalue$Percent.Active.Yuma, ylab = "% Active Extent",
     main = "", xlab = "Years",
     col = "indianred1",cex.lab = 2,  cex.main=1.5, bty ="o", pch=15, cex=3, cex.axis = 1.5)

mod1 = lm(lowmarketvalue$Percent.Active.Phx~ lowmarketvalue$Market.Value.Anomaly)
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
legend('bottomleft', legend = rp, bty = 'n', cex=1.5, text.col = "aquamarine4")


mod1 = lm(lowmarketvalue$Percent.Active.Yuma~ lowmarketvalue$Market.Value.Anomaly)
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
legend('bottomright', legend = rp, bty = 'n', cex=1.5, text.col = "indianred1")

windows()
par(mar = c(5, 5, 3, 5))
plot(highmarketvalue$Market.Value.Anomaly, highmarketvalue$Percent.Active.Phx, ylab = "% Active Extent",
     main = "", xlab = "high Demand Anomaly",
     col = "aquamarine4",cex.lab = 2,  cex.main=1.5, bty ="o", pch=17, cex=3, cex.axis = 1.5, ylim=c(0.75,0.91))
abline(lm(highmarketvalue$Percent.Active.Phx~ highmarketvalue$Market.Value.Anomaly), lwd=3, lty=2,col = "aquamarine4")

points(highmarketvalue$Market.Value.Anomaly, highmarketvalue$Percent.Active.Yuma, ylab = "% Active Extent",
       main = "", xlab = "Years",
       col = "indianred1",cex.lab = 2,  cex.main=1.5, bty ="o", pch=15, cex=3, cex.axis = 1.5)

mod1 = lm(highmarketvalue$Percent.Active.Phx~ highmarketvalue$Market.Value.Anomaly)
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
legend('bottomleft', legend = rp, bty = 'n', cex=1.5, text.col = "aquamarine4")


mod1 = lm(highmarketvalue$Percent.Active.Yuma~ highmarketvalue$Market.Value.Anomaly)
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
legend('bottomright', legend = rp, bty = 'n', cex=1.5, text.col = "indianred1")





##phoenix
#NDVI
par(mar = c(5, 5, 3, 5))
plot(highmarketvalue$PHX.Warm.Growing.Season.NDVI, highmarketvalue$PHX.Warm.Season.Mean.Precip, ylab = "Warm Season Precipitation (mm)",
     main = "", xlab = "NDVI",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
plot(lowmarketvalue$PHX.Warm.Growing.Season.NDVI, lowmarketvalue$PHX.Warm.Season.Mean.Precip, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(lowmarketvalue$PHX.Warm.Season.Mean.Precip ~ lowmarketvalue$PHX.Warm.Growing.Season.NDVI), lwd=3, lty=2,col = "blue")


mod1 = lm(highmarketvalue$PHX.Warm.Season.Mean.Precip ~ highmarketvalue$PHX.Warm.Growing.Season.NDVI)
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

mod1 = lm(lowmarketvalue$PHX.Warm.Season.Mean.Precip ~ lowmarketvalue$PHX.Warm.Growing.Season.NDVI)
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
legend('topright', legend = rp, bty = 'n', cex=1.5, text.col = "blue")

title("PHOENIX", outer=F,cex.main = 2)

##Extent
par(mar = c(5, 5, 3, 5))
plot.new()
plot(highmarketvalue$Percent.Active.Phx, highmarketvalue$PHX.Cool.Season.Precip, ylab = "Cool Season Precipitation (mm)",
     main = "", xlab = "% Active Crop",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
plot(lowmarketvalue$Percent.Active.Phx, lowmarketvalue$PHX.Cool.Season.Precip, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(lowmarketvalue$PHX.Cool.Season.Precip ~ lowmarketvalue$Percent.Active.Phx), lwd=3, lty=2,col = "blue")


mod1 = lm(highmarketvalue$PHX.Cool.Season.Precip ~ highmarketvalue$Percent.Active.Phx)
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

mod1 = lm(lowmarketvalue$PHX.Cool.Season.Precip ~ lowmarketvalue$Percent.Active.Phx)
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
legend('topright', legend = rp, bty = 'n', cex=1.5, text.col = "blue")

title("PHOENIX", outer=F,cex.main = 2)



##YUMA
#NDVI
par(mar = c(5, 5, 3, 5))
plot(highmarketvalue$Yuma.Cool.Growing.Season.NDVI, highmarketvalue$Yuma.Cool.Season.Mean.Temp, ylab = "Cool Season Temperature (C)",
     main = "", xlab = "NDVI",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
plot(lowmarketvalue$Yuma.Cool.Growing.Season.NDVI, lowmarketvalue$Yuma.Cool.Season.Mean.Temp, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(lowmarketvalue$Yuma.Cool.Season.Mean.Temp ~ lowmarketvalue$Yuma.Cool.Growing.Season.NDVI), lwd=3, lty=2,col = "blue")


mod1 = lm(highmarketvalue$Yuma.Cool.Season.Mean.Temp ~ highmarketvalue$Yuma.Cool.Growing.Season.NDVI)
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

mod1 = lm(lowmarketvalue$Yuma.Cool.Season.Mean.Temp ~ lowmarketvalue$Yuma.Cool.Growing.Season.NDVI)
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
legend('topright', legend = rp, bty = 'n', cex=1.5, text.col = "blue")

title("YUMA", outer=F,cex.main = 2)

##Extent
windows()
par(mar = c(5, 5, 3, 5))
plot(highmarketvalue$Percent.Active.Yuma, highmarketvalue$Yuma.Cool.Season.Mean.Temp, ylab = "Cool Season Temperature (C)",
     main = "", xlab = "% Active",
     col = "orange",cex.lab = 2,  cex.main=1.5, bty ="o", pch=16, cex=3, cex.axis = 1.5)

par(new = TRUE)
points(lowmarketvalue$Percent.Active.Yuma, lowmarketvalue$Yuma.Cool.Season.Mean.Temp, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "blue", lty = 2, cex.lab = 1.5, cex.main=4, bty ="o", pch=18, cex=3)
abline(lm(lowmarketvalue$Yuma.Cool.Season.Mean.Temp ~ lowmarketvalue$Percent.Active.Yuma), lwd=3, lty=2,col = "orange")


mod1 = lm(highmarketvalue$Yuma.Cool.Season.Mean.Temp ~ highmarketvalue$Percent.Active.Yuma)
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

mod1 = lm(lowmarketvalue$Yuma.Cool.Season.Mean.Temp ~ lowmarketvalue$Percent.Active.Yuma)
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
legend('topright', legend = rp, bty = 'n', cex=1.5, text.col = "blue")

