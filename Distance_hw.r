```R
# 資料為鮭魚之資料，將其區分為：阿拉斯加或加拿大兩產地之鮭魚。
# 利用此下面變數，區分出魚來自哪個產地。

# A. 選出區別變數
# B. 利用下列方法分群：
# 1. cutoff point
# 2. posterior probability
# 哪個方法判別能力較佳？

setwd("C:\\Users\\Sherry\\Google Drive\\OneDrive\\TMU\\_多變量")
salmon=read.table("_salmon.txt");salmon
names(salmon)=c("Origin","Gender","Freshwater growth","Marine growth")
salmon
##---plot---------------------------------
plot(salmon[,c(3:4)],col=salmon[,1])
library(psych)
pairs.panels(salmon[3:4],
             gap=0,
             bg=c("red","green")[salmon$group],
             pch=21)


mean.1<-apply(salmon[1:50,3:4],2,mean);mean.1
mean.2<-apply(salmon[51:100,3:4],2,mean);mean.2
var.1<-var(salmon[1:50,3:4]);var.1
var.2<-var(salmon[51:100,3:4]);var.2
(mean.2[1]+mean.1[1])/2  ##cutoff point 117.92
salmon.group=salmon[,3] > 117.92;salmon.group
salmon[salmon.group,1] 
salmon[!salmon.group,1] 

(mean.2[2]+mean.1[2])/2  ##cutoff point 398.14
salmon.group_1=salmon[,4] > 398.14;salmon.group_1
salmon[salmon.group_1,1] 
salmon[!salmon.group_1,1] 

##馬氏距離判別
#w2equal=function(x,mu1,mu2,S){(mahalanobis(x,mu2,S)-mahalanobis(x,mu1,S))/2}
#mu1=mean.1;mu1
# mu2=mean.2
# S=cov(salmon[,1:2]);S
# x=salmon[1:10,1:2];x
# w2equal(x,mu1,mu2,S)

##費雪判別
##t-testing: p value <0.05, y1&y2都是中藥判別函數
# y1=t.test(income~group, data=salmon);y1
# y2=t.test(lotsize~group, data=salmon)

library(MASS)
ld_1=lda(salmon[,1]~salmon[,2]+salmon[,3]+salmon[,4],data=salmon)
names(ld_1)
ld_1$svd

z_1=predict(ld_1)  ##計算posterior prop 及分類 落在事後機率大的那一類
names(z_1)  # "x" = score(新變數的判別計分，講義裡的z值)
## z_2=predict(ld_1,newdata=salmon[,c(1,2)])$class  ##newdata可將預測之資料帶入
newG_1=z_1$class  #class = 分群結果
cbind(salmon[,1], newG_1, z_1$x)
tabl_1=table(newG_1, salmon[,1]);tabl_1  ##驗證正確性 
```
