library(MVA);library(HSAUR2)
#### 4.1 ####
dat<-read.csv("C:/Users/mjay8/Dropbox/2021-1학기수업/다변량통계학습/숙제/dat.csv",header=T)
base<-matrix(18,nrow=51,ncol=51)
for (i in dat) {
  new<-sqrt(base-2*dat)
}
cmdscale(new,k=2,eig=TRUE)

sqrt(dat[1,1]+dat[1,1]-2*dat[1,1])
sqrt(dat[1,1]+dat[2,2]-2*dat[1,2])
sqrt(dat[1,1]+dat[3,3]-2*dat[1,3])
sqrt(dat[1,1]+dat[4,4]-2*dat[1,4])
sqrt(dat[1,1]+dat[5,5]-2*dat[1,5])
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
#tmp <- HSAURtable(tmp, xname = "gardenflowers")
#tmp$data[upper.tri(tmp$data)] <- " "
#toLatex(tmp, pcol = 1,
#        caption = "Dissimilarity matrix of $18$ species of gardenflowers.",
#        label = "MDS-gardenflowers-tab",
#        rownames = TRUE)
