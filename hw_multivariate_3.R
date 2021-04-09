library(MVA)
library("lattice")
#### 3.1. ####
data("heptathlon",package="HSAUR2")
pairs(heptathlon,
      panel = function(x,y, ...){
        bvbox(cbind(x,y),add=TRUE)
      })
pairs(heptathlon)

#### 3.2. ####
attach(USairpollution)
head(USairpollution)
plot(manu,SO2); abline(lm(SO2~manu));lines(lowess(SO2~manu))
plot(popul,SO2); abline(lm(SO2~popul));lines(lowess(SO2~popul))
plot(wind,SO2); abline(lm(SO2~wind));lines(lowess(SO2~wind))
plot(precip,SO2); abline(lm(SO2~precip));lines(lowess(SO2~precip))
plot(predays,SO2); abline(lm(SO2~predays));lines(lowess(SO2~predays))
plot(negtemp,SO2); abline(lm(SO2~negtemp));lines(lowess(SO2~negtemp))
detach(USairpollution)

#### 3.3 ####
convict<-matrix(c(1,0.402,0.396,0.301,0.305,0.339,0.34,0.402,1,0.618,0.15,0.135,0.206,0.183,0.396,0.618,1,0.321,0.289,0.363,0.345,0.301,0.15,0.321,1,0.846,0.759,0.661,0.305,0.135,0.289,0.846,1,0.797,0.8,0.339,0.206,0.363,0.759,0.797,1,0.736,0.34,0.183,0.345,0.661,0.8,0.736,1),ncol=7,nrow=7,byrow=T)


