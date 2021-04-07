library(MVA)
library(HSAUR2)
####2.1####
demo("Ch-Viz")
data("USairpollution", package = "HSAUR2")
par(mfrow=c(1,1))
a<-USairpollution[,c("SO2","temp")]
bvbox(a,xlab="SO2",ylab="temp")
text(a,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outa<-match(c("Miami","Providence","Chicago"),rownames(USairpollution))
cor(a)
with(a,cor(SO2[-aout],temp[-aout]))

b<-USairpollution[,c("SO2","manu")]
bvbox(b,xlab="SO2",ylab="manu")
text(b,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outb<-match(c("Detroit","Cleveland","Philadelphia","Providence","Chicago"),rownames(USairpollution))
cor(b)
with(b,cor(SO2[-outb],manu[-outb]))

c<-USairpollution[,c("SO2","popul")]
bvbox(c,xlab="SO2",ylab="popul")
text(c,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outc<-match(c("Detroit","Houston","Philadelphia","Providence","Chicago"),rownames(USairpollution))
cor(c)
with(c,cor(SO2[-outc],popul[-outc]))

d<-USairpollution[,c("SO2","wind")]
bvbox(d,xlab="SO2",ylab="wind")
text(d,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outd<-match(c("Providence","Chicago"),rownames(USairpollution))
cor(d)
with(d,cor(SO2[-outd],wind[-outd]))

e<-USairpollution[,c("SO2","precip")]
bvbox(e,xlab="SO2",ylab="precip")
text(e,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
oute<-match(c("Providence","Chicago","Phoenix","Albuquerque"),rownames(USairpollution))
cor(e)
with(e,cor(SO2[-oute],precip[-oute]))

f<-USairpollution[,c("SO2","predays")]
bvbox(f,xlab="SO2",ylab="predays")
text(f,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outf<-match(c("Providence","Chicago","Phoenix","Buffalo"),rownames(USairpollution))
cor(f)
with(f,cor(SO2[-outf],predays[-outf]))

g<-USairpollution[,c("temp","manu")]
bvbox(g,xlab="temp",ylab="manu")
text(g,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outg<-match(c("Philadelphia","Chicago"),rownames(USairpollution))
cor(g)
with(g,cor(temp[-outg],manu[-outg]))

h<-USairpollution[,c("temp","popul")]
bvbox(h,xlab="temp",ylab="popul")
text(h,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outh<-match(c("Philadelphia","Chicago","Detroit"),rownames(USairpollution))
cor(h)
with(h,cor(temp[-outh],popul[-outh]))

i<-USairpollution[,c("temp","wind")]
bvbox(i,xlab="temp",ylab="wind")
text(i,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outi<-match(c("Miami","Phoenix"),rownames(USairpollution))
cor(i)
with(i,cor(temp[-outi],wind[-outi]))

j<-USairpollution[,c("temp","precip")]
bvbox(j,xlab="temp",ylab="precip")
text(j,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outj<-match(c("Miami","Phoenix","Albuquerque","Denver"),rownames(USairpollution))
cor(j)
with(j,cor(temp[-outj],precip[-outj]))



k<-USairpollution[,c("temp","predays")]
bvbox(k,xlab="temp",ylab="predays")
text(k,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
l<-USairpollution[,c("manu","popul")]
bvbox(l,xlab="manu",ylab="popul")
text(l,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
m<-USairpollution[,c("manu","wind")]
bvbox(mxlab="manu",ylab="wind")
text(m,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
n<-USairpollution[,c("manu","precip")]
bvbox(n,xlab="manu",ylab="precip")
text(n,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
o<-USairpollution[,c("manu","predays")]
bvbox(o,xlab="manu",ylab="predays")
text(o,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
p<-USairpollution[,c("popul","wind")]
bvbox(p,xlab="popul",ylab="wind")
text(p,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))q<-USairpollution[,c("popul","precip")]
r<-USairpollution[,c("popul","predays")]
bvbox(r,xlab="popul",ylab="predays")
text(r,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
s<-USairpollution[,c("wind","precip")]
bvbox(s,xlab="wind",ylab="precip")
text(s,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
t<-USairpollution[,c("wind","predays")]
bvbox(t,xlab="wind",ylab="predays")
text(t,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
u<-USairpollution[,c("precip","predays")]
bvbox(u,xlab="precip",ylab="predays")
text(u,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))

####2.2####
####2.3####

####2.4####
####2.5####
####2.6####