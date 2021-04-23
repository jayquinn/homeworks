table1.1<-read.table("C:/git/homeworks/t1.1.txt",header=T)
table1.3<-read.table("C:/git/homeworks/t1.3.txt",header=T)
install.packages("MVA")###
library(MVA)
demo("Ch-MVA")
hypo <-structure(list(individual = 1:10, sex = structure(c(2L, 2L, 2L,2L, 2L, 1L, 1L, 1L, 1L, 1L), .Label = c("Female", "Male"), class = "factor"),age = c(21L, 43L, 22L, 86L, 60L, 16L, NA, 43L, 22L, 80L),IQ = c(120L, NA, 135L, 150L, 92L, 130L, 150L, NA, 84L, 70L), depression = structure(c(2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,1L, 1L), .Label = c("No", "Yes"), class = "factor"), health = structure(c(3L,3L, 1L, 4L, 2L, 2L, 3L, 1L, 1L, 2L), .Label = c("Average","Good", "Very good", "Very poor"), class = "factor"), weight = c(150L,160L, 135L, 140L, 110L, 110L, 120L, 120L, 105L, 100L)), .Names = c("individual","sex", "age", "IQ", "depression", "health", "weight"), class = "data.frame", row.names = c(NA, -10L))

#Q1
cor(hypo[,c(3,4,7)])
var(hypo[,c(3,4,7)])

#Q2
hypo$age <- ifelse(is.na(hypo$age), mean(hypo$age,na.rm = T), hypo$age)
hypo$IQ <- ifelse(is.na(hypo$IQ), mean(hypo$IQ,na.rm = T), hypo$IQ)
cor(hypo[,c(3,4,7)])

#Q3
x<-table1.3[,c(1:9)]
mean<-colMeans(x)
S<-cov(x)
d<-apply(x,1,function(x)
  (x-mean)%*%solve(S)%*%(x-mean))

par(mfrow=c(1,3))
qqnorm(table1.3[,1]);qqline(table1.3[,1])
qqnorm(table1.3[,2]);qqline(table1.3[,2])
qqnorm(table1.3[,3]);qqline(table1.3[,3])
qqnorm(table1.3[,4]);qqline(table1.3[,4])
qqnorm(table1.3[,5]);qqline(table1.3[,5])
qqnorm(table1.3[,6]);qqline(table1.3[,6])
qqnorm(table1.3[,7]);qqline(table1.3[,7])
qqnorm(table1.3[,8]);qqline(table1.3[,8])
qqnorm(table1.3[,9]);qqline(table1.3[,9])
par(mfrow=c(1,1))
plot(qchisq((1:nrow(x)-1/2)/nrow(x),df=9),sort(d),xlab=expression(paste(chi[9]^2,"Quantile")),
                                                                  ylab="Ordered Distances");abline(a=0,b=1)
#Q4
covmat<-matrix(c(3.8778,2.811,3.148,3.5062,2.811,2.121,2.2669,2.569,3.1480,2.2669,2.655,2.8341,3.5062,2.569,2.8341,3.2352),nrow=4,ncol=4)
cor(covmat)
#Q5
dataset<-matrix(c(3,4,4,6,1,5,1,1,7,3,6,2,0,2,6,1,1,1,0,3,4,7,3,6,2,2,2,5,1,0,0,4,1,1,1,0,6,4,3,5,7,6,5,1,4,2,1,4,3,1),ncol=5)
dsdist<-dist(scale(dataset,center = F))
as.dist(round(as.matrix(dsdist),2))
