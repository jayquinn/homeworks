library(MVA)
library(HSAUR2)
library(KernSmooth)
####2.1####
demo("Ch-Viz")
data("USairpollution", package = "HSAUR2")
par(mfrow=c(1,1))

a<-USairpollution[,c("SO2","temp")]
bvbox(a,xlab="SO2",ylab="temp")
text(a,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outa<-match(c("Miami","Providence","Chicago"),rownames(USairpollution))
cor(a)
with(a,cor(SO2[-outa],temp[-outa]))

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
outk<-match(c("Miami","Phoenix"),rownames(USairpollution))
cor(k)
with(k,cor(temp[-outk],predays[-outk]))

l<-USairpollution[,c("manu","popul")]
bvbox(l,xlab="manu",ylab="popul")
text(l,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outl<-match(c("Detroit","Philadelphia","Chicago","Cleveland"),rownames(USairpollution))
cor(l)
with(l,cor(manu[-outl],popul[-outl]))


m<-USairpollution[,c("manu","wind")]
bvbox(m,xlab="manu",ylab="wind")
text(m,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outm<-match(c("Detroit","Philadelphia","Chicago","Cleveland"),rownames(USairpollution))
cor(m)
with(m,cor(manu[-outm],wind[-outm]))


n<-USairpollution[,c("manu","precip")]
bvbox(n,xlab="manu",ylab="precip")
text(n,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outn<-match(c("Philadelphia","Chicago"),rownames(USairpollution))
cor(n)
with(n,cor(manu[-outn],precip[-outn]))


o<-USairpollution[,c("manu","predays")]
bvbox(o,xlab="manu",ylab="predays")
text(o,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outo<-match(c("Philadelphia","Chicago"),rownames(USairpollution))
cor(o)
with(o,cor(manu[-outo],predays[-outo]))


p<-USairpollution[,c("popul","wind")]
bvbox(p,xlab="popul",ylab="wind")
text(p,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outp<-match(c("Philadelphia","Chicago","Detroit"),rownames(USairpollution))
cor(p)
with(p,cor(popul[-outp],wind[-outp]))


q<-USairpollution[,c("popul","precip")]
bvbox(q,xlab="popul",ylab="wind")
text(q,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outq<-match(c("Philadelphia","Chicago","Detroit"),rownames(USairpollution))
cor(q)
with(q,cor(popul[-outq],precip[-outq]))


r<-USairpollution[,c("popul","predays")]
bvbox(r,xlab="popul",ylab="predays")
text(r,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outr<-match(c("Philadelphia","Chicago","Detroit"),rownames(USairpollution))
cor(r)
with(r,cor(popul[-outq],predays[-outq]))



s<-USairpollution[,c("wind","precip")]
bvbox(s,xlab="wind",ylab="precip")
text(s,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outs<-match(c("Phoenix","Albuquerque"),rownames(USairpollution))
cor(s)
with(s,cor(wind[-outs],precip[-outs]))


t<-USairpollution[,c("wind","predays")]
bvbox(t,xlab="wind",ylab="predays")
text(t,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outt<-match(c("Phoenix","Buffalo"),rownames(USairpollution))
cor(t)
with(t,cor(wind[-outt],predays[-outt]))


u<-USairpollution[,c("precip","predays")]
bvbox(u,xlab="precip",ylab="predays")
text(u,(labels=rownames(USairpollution)),cex=0.7,pos=(c(2,2,4,2,2)))
outu<-match(c("Phoenix","Albuquerque"),rownames(USairpollution))
cor(u)
with(u,cor(precip[-outu],predays[-outu]))


####2.2####
pairs(USairpollution)
plot(USairpollution)
chiplot(USairpollution[,1],USairpollution[,2])
chiplot(USairpollution[,1],USairpollution[,3])
chiplot(USairpollution[,1],USairpollution[,4])
chiplot(USairpollution[,1],USairpollution[,5])
chiplot(USairpollution[,1],USairpollution[,6])
chiplot(USairpollution[,1],USairpollution[,7])
chiplot(USairpollution[,2],USairpollution[,1])
chiplot(USairpollution[,2],USairpollution[,3])
chiplot(USairpollution[,2],USairpollution[,4])
chiplot(USairpollution[,2],USairpollution[,5])
chiplot(USairpollution[,2],USairpollution[,6])
chiplot(USairpollution[,2],USairpollution[,7])
chiplot(USairpollution[,3],USairpollution[,1])
chiplot(USairpollution[,3],USairpollution[,2])
chiplot(USairpollution[,3],USairpollution[,4])
chiplot(USairpollution[,3],USairpollution[,5])
chiplot(USairpollution[,3],USairpollution[,6])
chiplot(USairpollution[,3],USairpollution[,7])
chiplot(USairpollution[,4],USairpollution[,1])
chiplot(USairpollution[,4],USairpollution[,2])
chiplot(USairpollution[,4],USairpollution[,3])
chiplot(USairpollution[,4],USairpollution[,5])
chiplot(USairpollution[,4],USairpollution[,6])
chiplot(USairpollution[,4],USairpollution[,7])
chiplot(USairpollution[,5],USairpollution[,1])
chiplot(USairpollution[,5],USairpollution[,2])
chiplot(USairpollution[,5],USairpollution[,3])
chiplot(USairpollution[,5],USairpollution[,4])
chiplot(USairpollution[,5],USairpollution[,6])
chiplot(USairpollution[,5],USairpollution[,7])
chiplot(USairpollution[,6],USairpollution[,1])
chiplot(USairpollution[,6],USairpollution[,2])
chiplot(USairpollution[,6],USairpollution[,3])
chiplot(USairpollution[,6],USairpollution[,4])
chiplot(USairpollution[,6],USairpollution[,5])
chiplot(USairpollution[,6],USairpollution[,7])
chiplot(USairpollution[,7],USairpollution[,1])
chiplot(USairpollution[,7],USairpollution[,2])
chiplot(USairpollution[,7],USairpollution[,3])
chiplot(USairpollution[,7],USairpollution[,4])
chiplot(USairpollution[,7],USairpollution[,5])
chiplot(USairpollution[,7],USairpollution[,6])
####2.3####
measure <- structure(list(V1 = 1:20, V2 = c(34L, 37L, 38L, 36L, 38L, 43L,40L, 38L, 40L, 41L, 36L, 36L, 34L, 33L, 36L, 37L, 34L, 36L, 38L,35L), V3 = c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L,24L, 25L, 24L, 22L, 26L, 26L, 25L, 26L, 28L, 23L), V4 = c(32L,37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L, 35L, 37L, 37L, 34L,38L, 37L, 38L, 37L, 40L, 35L)), .Names = c("V1", "V2", "V3","V4"), class = "data.frame", row.names = c(NA, -20L))
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")
panel.bxp<-function(x, ...)
{
  usr<-par("usr"); on.exit(par(usr))
  par(usr=c(0,2,usr[3:4]))
  boxplot(x,add=TRUE)
}
pairs(measure[,c("chest","waist","hips")], ylim=c(20,45),xlim=c(20,45),
      diag.panel = panel.bxp,
      panel = function(x,y, ...){
        bvbox(cbind(x,y), xlim=c(30,42),ylim=c(30,42),add=TRUE)
      })
## 그림 2.17과 비교하고 어떤 그림이 데이터에 대해 정보를 더 제공하는지에 대해 설명하시오.

####2.4####
panel_l<-function(x, y){
  points(x,y, pch = 19, col=c("red", "blue")[measure$gender])}
pairs(measure[,c("chest","waist","hips")],lower.panel = panel_l,upper.panel = function(x,y){
  data<-data.frame(cbind(x,y))
  par(new=TRUE)
  den<-bkde2D(data,bandwidth=sapply(data,dpik))
  contour(x = den$x1, y = den$x2,
          z = den$fhat, axes = FALSE)
})
####2.5####
panel_l<-function(x, y){
  points(x,y, pch = 19, col=c("red", "blue", "green", "purple", "yellow")[pottery$kiln])}
pairs(pottery[,-10],lower.panel = panel_l,upper.panel = function(x,y){
  data<-data.frame(cbind(x,y))
  par(new=TRUE)
  den<-bkde2D(data,bandwidth=sapply(data,dpik))
  contour(x = den$x1, y = den$x2,
          z = den$fhat, axes = FALSE)
})
####2.6####
head(quakes)
par(mfrow=c(1,1))
ylim<-with(quakes,range(lat))
plot(lat ~ long, data= quakes,
     xlab = "long",
     ylab = "lat",
     ylim = ylim)
with(quakes,symbols(long, lat, circles = depth, inches = 0.125, add = TRUE))
# 4 ~ 4.7, 4.8~5.5, 5.6 ~ 6.4
mcircle<-ifelse(quakes$mag<4.8,quakes$mag,"0")
msquare<-ifelse(4.7<quakes$mag & quakes$mag<5.6,quakes$mag,"0")
mrectan<-ifelse(quakes$mag>5.5,quakes$mag,"0")

with(quakes,
     {symbols(long,lat,circles = mcircle,inches = 0.125, )
       symbols(long,lat,squares = msquare,inches = 0.125, add = TRUE)
       symbols(long,lat,circles = mrectan,inches = 0.125, add = TRUE,fg = 3)})


#par(new=TRUE) #겹치기

