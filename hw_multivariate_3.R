library(MVA)
library("lattice")
library(KernSmooth)
demo("Ch-PCA") ##R 코드 진행 전 실행 필수!!
#### 3.1. ####
data("heptathlon",package="HSAUR2")
pairs(heptathlon)
pairs(heptathlon,
      panel = function(x,y, ...){
        data<-data.frame(cbind(x,y))
        par(new=TRUE)
        den<-bkde2D(data,bandwidth=sapply(data,dpik))
        contour(x = den$x1, y = den$x2,
                z = den$fhat, axes = FALSE)
      })


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
convict_pca<-princomp(convict,cor = T)
summary(convict_pca,loading=T)
plot(convict_pca$sdev^2)
#Catell의 팔꿈치면 2개가 적절하지 않나
# 첫번째 주성분에 대한 선형결합을 확인하면 7가지의 변수가 모두 활용됨을 알 수 있음. 두번째 주성분은 머리폭과 얼굴폭의 음의 선형관계를 나타내고 있음. 세번째 주성분은 머리 길이가 양의 관계를 보이고 있음을 확인함. 결국 범죄자들을 머리폭과 얼굴폭이 작거나 머리의 길이가 긴 경우가 많았음을 알 수 있음.

#### 3.4 ####
#미친건가?
"headsize" <-
  matrix(c(191, 195, 181, 183, 176, 208, 189, 197, 188, 192, 179, 183, 174, 190, 188, 163, 195, 186, 181, 175, 192, 174,
           176, 197, 190, 155, 149, 148, 153, 144, 157, 150, 159, 152, 150, 158, 147, 150, 159, 151, 137, 155, 153,
           145, 140, 154, 143, 139, 167, 163, 179, 201, 185, 188, 171, 192, 190, 189, 197, 187, 186, 174, 185, 195,
           187, 161, 183, 173, 182, 165, 185, 178, 176, 200, 187, 145, 152, 149, 149, 142, 152, 149, 152, 159, 151,
           148, 147, 152, 157, 158, 130, 158, 148, 146, 137, 152, 147, 143, 158, 150)
         , nrow = 25, ncol = 4
         ,  dimnames = list(character(0)
                            , c("head1", "breadth1", "head2", "breadth2")))
x <- headsize
headsize <- as.data.frame(headsize)
toLatex(HSAURtable(headsize), pcol = 2,
        caption = "Head Size Data.",
        label = "ch:PCA:headsize:tab", rownames = FALSE)
headsize <- x

headsize
tail(headsize)
n = nrow(headsize)
q1 = 2
q2 = 2
headsize.std <- sweep(headsize, 2, 
                      apply(headsize, 2, sd), FUN = "/")
R <- cor(headsize.std)
r11 <- R[1:2, 1:2]
r22 <- R[-(1:2), -(1:2)]
r12 <- R[1:2, -(1:2)]
r21 <- R[-(1:2), 1:2]
(E1 <- solve(r11) %*% r12 %*% solve(r22) %*%r21)
(E2 <- solve(r22) %*% r21 %*% solve(r11) %*%r12)
(e1 <- eigen(E1))
(e2 <- eigen(E2))
lambda = cbind(e1$values)
Sigma<-sum(log(1-lambda))
statistic=-(n-(1/2*(q1+q2+1)))*Sigma # headsize 검정통계량 
dchisq(statistic,df=4) #headsize 검정통계량 기각 
LAdepr
depr <- c(
  0.212,
  0.124,  0.098,
  -0.164,  0.308,  0.044,
  -0.101, -0.207, -0.106, -0.208,
  -0.158, -0.183, -0.180, -0.192, 0.492)
LAdepr <- diag(6) / 2
LAdepr[upper.tri(LAdepr)] <- depr
LAdepr <- LAdepr + t(LAdepr)
rownames(LAdepr) <- colnames(LAdepr) <- c("CESD", "Health", "Gender", "Age", "Edu", "Income")
x <- LAdepr

LAdepr <- as.data.frame(LAdepr)
toLatex(HSAURtable(LAdepr), 
        caption = "Los Angeles Depression Data.",
        label = "ch:PCA:LAdepr:tab", rownames = FALSE)
LAdepr <- x
r11 <- LAdepr[1:2, 1:2]
r22 <- LAdepr[-(1:2), -(1:2)]
r12 <- LAdepr[1:2, -(1:2)]
r21 <- LAdepr[-(1:2), 1:2]
(E1 <- solve(r11) %*% r12 %*% solve(r22) %*%r21)
(E2 <- solve(r22) %*% r21 %*% solve(r11) %*%r12)
(e1 <- eigen(E1))
(e2 <- eigen(E2))

n = 294
q1 = 2
q2 = 4
lambda = cbind(e1$values)
Sigma<-sum(log(1-lambda))
statistic=-(n-(1/2*(q1+q2+1)))*Sigma # depress 검정통계량
dchisq(statistic,df=8) # derpess .05수준에서 기각
#### 3.5 ####
#교과서
usair_pca <- princomp(USairpollution[,-1], cor = TRUE)
summary(usair_pca, loadings = TRUE)
pairs(usair_pca$scores[,1:3], ylim = c(-6, 4), xlim = c(-6, 4),
      panel = function(x,y, ...) {
        text(x, y, abbreviate(row.names(USairpollution)), 
             cex = 0.6)
        bvbox(cbind(x,y), add = TRUE)
      }) 

# 회귀시킨거 그림 par(mfrow=c(3,2)) 쓰고쓰십쇼
par(mfrow=c(3,2))
out <- sapply(1:6, function(i) {
  plot(usair_pca$scores[,i],USairpollution$SO2,
       xlab = paste("PC", i, sep = ""), 
       ylab = "Sulphur dioxide concentration")
  abline(lm(USairpollution$SO2~usair_pca$scores[,i]))
})
par(mfrow=c(1,1))

#과제요청사항
usair_pca <- princomp(USairpollution[-c(2,5,6,30,31),-1], cor = TRUE)
summary(usair_pca, loadings = TRUE)
pairs(usair_pca$scores[,1:3], ylim = c(-6, 4), xlim = c(-6, 4),
      panel = function(x,y, ...) {
        text(x, y, abbreviate(row.names(USairpollution)), 
             cex = 0.6)
        bvbox(cbind(x,y), add = TRUE)
      }) #앨버커키, 버팔로, 찰스턴, 필라델피아, 피닉스 배제

par(mfrow=c(3,2))
out <- sapply(1:6, function(i) {
  plot(usair_pca$scores[,i],USairpollution$SO2[-c(2,5,6,30,31)],
       xlab = paste("PC", i, sep = ""), 
       ylab = "Sulphur dioxide concentration");
  abline(lm(USairpollution$SO2[-c(2,5,6,30,31)]~usair_pca$scores[,i]))})
par(mfrow=c(1,1))

