#install.packages("ape")
#install.packages("scatterplot3d")
#install.packages("vegan")
library(scatterplot3d)
library(MVA);library(HSAUR2); library(ape); library(MASS)
library(vegan)

#### 4.1 ####
51*51
i<-1:51
j<-1:51
a<-matrix(rep(1:51,51), nrow = 51, ncol = 51)
b<-matrix(rep(1:51,51),nrow = 51, ncol = 51,byrow=T)
dat<-ifelse(a[1:51,]==b[,1:51],9,
       ifelse(1<=abs(a[1:51,]-b[,1:51])&abs(a[1:51,]-b[,1:51])<=3,8,
              ifelse(4<=abs(a[1:51,]-b[,1:51])&abs(a[1:51,]-b[,1:51])<=6,7,
                     ifelse(7<=abs(a[1:51,]-b[,1:51])&abs(a[1:51,]-b[,1:51])<=9,6,
                            ifelse(10<=abs(a[1:51,]-b[,1:51])&abs(a[1:51,]-b[,1:51])<=12,5,
                                   ifelse(13<=abs(a[1:51,]-b[,1:51])&abs(a[1:51,]-b[,1:51])<=15,4,
                                          ifelse(16<=abs(a[1:51,]-b[,1:51])&abs(a[1:51,]-b[,1:51])<=18,3,
                                                 ifelse(19<=abs(a[1:51,]-b[,1:51])&abs(a[1:51,]-b[,1:51])<=21,2,
                                                        ifelse(22<=abs(a[1:51,]-b[,1:51])&abs(a[1:51,]-b[,1:51])<=24,1,
                                                               ifelse(abs(a[1:51,]-b[,1:51])>=25,0,NA))))))))))

print(dat) #유사성 행렬
edit(dat)
new<-sqrt((dat+9)-2*dat)
print(new) #비유사성행렬
edit(new)
Q1<-cmdscale(new,k=2,eig=TRUE)
plot(Q1$points[,1],Q1$points[,2]) #이차원 해의 모양 
#### 4.3 ####
###################################################
### code chunk number 36: MDS-gardenflowers-tab
###################################################
data("gardenflowers", package = "HSAUR2")
gfnames <- attr(gardenflowers, "Labels")
attr(gardenflowers, "Labels") <- gsub(" \\(.*", "", gfnames)
tmp <- as.matrix(gardenflowers)
colnames(tmp) <- abbreviate(colnames(tmp), 3)
tmp <- as.data.frame(tmp) 
print(tmp) #비유사성행렬
#tmp <- HSAURtable(tmp, xname = "gardenflowers")
#tmp$data[upper.tri(tmp$data)] <- " "
#toLatex(tmp, pcol = 1,
#        caption = "Dissimilarity matrix of $18$ species of gardenflowers.",
#        label = "MDS-gardenflowers-tab",
#        rownames = TRUE)
metaMDS(tmp, distance="bray", k=4, trace=FALSE, trymax=100) #2차원에서 스트레스 .18로 적정수준으로 이해할 수 있음
gf_mds<-cmdscale(tmp,k=9,eig=TRUE)
print(gf_mds)
cumsum(abs(gf_mds$eig))/sum(abs(gf_mds$eig)) #차원 설정 기준 (1) 5-6개의 차원이 적정
cumsum((gf_mds$eig)^2)/sum((gf_mds$eig)^2) #차원 설정 기준 (2) 2-3개의 차원이 적정
st<-mst(gardenflowers)# 최소신장나무 적용
print(st)
x<-gf_mds$points[,1]
y<-gf_mds$points[,2]
plot(x,y,xlab="Coordinate 1", ylab = "Coordinate 2", xlim=range(x))
text(x,y,labels=rownames(tmp),cex=0.7)
for(i in 1:18){
  w1<-which(st[i,]==1)
  segments(x[i],y[1],x[w1],y[w1])
}
text(x,y,labels=rownames(tmp),cex=0.7)
#3차원 실험
z<-gf_mds$points[,3]
scatterplot3d(x,y,z,xlab = 1,ylab= 2,zlab=3)
#공간적인 정렬이 나타나지 않는다.
#2차원 해를 통해서는 전체 거리행렬에 대한 적절한 표현을 주지 못함을 확인할 수 있다
#비계량형 다차원 척도법 적용
gf_nm<-isoMDS(gardenflowers,k=3)
gf_nm<-isoMDS(gardenflowers,k=4)
print(gf_nm)
x<-gf_nm$points[,1]
y<-gf_nm$points[,2]
plot(x,y,xlab="Coordinate 1", ylab = "Coordinate 2", xlim=range(x))
text(x,y,labels=rownames(tmp),cex=0.7)
flr_sh<-Shepard(gardenflowers,gf_nm$points)
plot(flr_sh,pch=".",xlab="Dissimilarity",ylab="Distance")
lines(flr_sh$x,flr_sh$yf,type = "S")
stressplot(gf_nm)
