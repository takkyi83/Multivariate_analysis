```R
setwd("C:\\Users\\Sherry\\Google Drive\\OneDrive\\TMU\\_多變量")
mower=read.table("_mower.txt");mower
names(mower)=c("income","lotsize","group")

##---plot---------------------------------
plot(mower[,c(1:2)],col=mower[,3])
library(psych)
pairs.panels(mower[1:2],
             gap=0,
             bg=c("red","green")[mower$group],
             pch=21)


mean.1<-apply(mower[1:12,1:2],2,mean);mean.1
mean.2<-apply(mower[13:24,1:2],2,mean);mean.2
var.1<-var(mower[1:12,1:2]);var.1
var.2<-var(mower[13:24,1:2]);var.2
(mean.2[1]+mean.1[1])/2  ##cutoff point
mower.group=mower[,1] > 68.4375;mower.group
mower[mower.group,3] 
mower[!mower.group,3] 
class(mower)

(mean.2[2]+mean.1[2])/2  ##cutoff point
mower.group_1=mower[,2] > 18.95;mower.group_1
mower[mower.group_1,3] 
mower[!mower.group_1,3] 

##馬氏距離判別
w2equal=function(x,mu1,mu2,S){(mahalanobis(x,mu2,S)-mahalanobis(x,mu1,S))/2}
mu1=mean.1;mu1
mu2=mean.2;mu2
S=cov(mower[,1:2]);S
x=mower[1:10,1:2];x
w2equal(x,mu1,mu2,S)

##費雪判別
##t-testing: p value <0.05, y1&y2都是重要判別函數
y1=t.test(income~group, data=mower);y1
y2=t.test(lotsize~group, data=mower);y2

library(MASS)
ld_1=lda(group~income+lotsize,data=mower)
names(ld_1)
ld_1$svd

z_1=predict(ld_1);z_1  ##計算posterior prop 及分類 落在事後機率大的那一類
names(z_1)  # "x" = score(新變數的判別計分，講義裡的z值)
## z_2=predict(ld_1,newdata=mower[,c(1,2)])$class  ##newdata可將預測之資料帶入
newG_1=z_1$class;newG_1  #class = 分群結果
cbind(mower$group, newG_1, z_1$x)
tabl_1=table(newG_1, mower$group);tabl_1  ##驗證正確性 

# Panels of histograms and overlayed density plots
plot(ld_1,dimen=1, type="both")  #有overlap, 因此用presteoid做分類會比用score好
prop_1 = ld_1$svd^2/sum(ld_1$svd^2);prop_1

##---------------------------------------------
##iris為例
##data partition(將資料分成兩類,training and testing)
set.seed(2018);# set seed才部會每次跑出來結果都不同
ind=sample(2,nrow(iris),replace=T,prob=c(0.6,0.4)) # replace=T 抽後放回
training=iris[ind==1,]  # 60%
testing=iris[ind==2,]

ld=lda(Species~.,data=training) 
ld
z=predict(ld,data=training)
newG=z$class
cbind(training$Species, newG, z$x)
tabl=table(newG, training$Species)
ld$svd
prop = ld$svd^2/sum(ld$svd^2)
##histogram
ldahist(data=z$x[,1],g=training$Species)#對第一個判別函數的分群。overlap少 用score分群
ldahist(data=z$x[,2],g=training$Species)

##partition plot
install.packages('klaR')
library(klaR)
partimat(Species~.,data=training, method="lda") #變數根變數之間的關係，線性判別:顏色為分群結果
partimat(Species~.,data=training, method="qda")# 二次判別

##confusion matrix and accuracry
z_t=predict(ld,training)$class
tabl=table(predicted=z_t, Actual=training$Species);tabl
sum(diag(tabl))/sum(tabl)

z_te=predict(ld,testing)$class
tabl_te=table(predicted=z_te, Actual=testing$Species)
sum(diag(tabl_te))/sum(tabl_te)
##------------------------------------------------------------------
##Bayes判別法
ld_b=lda(group~income+lotsize,data=mower, prior=c(1/4,3/4))  ##費雪判別內設prior各類相等 預設為equal
z_b=predict(ld_b)  ##計算posterior prop
names(z_b)
newG_b=z_b$class
cbind(mower$group, newG_b, z_b$x)
tabl_b=table(newG_b, mower$group)
sum(diag(prop.table(tabl_b)))
z_b$post
plot(ld_b)


##----------------------------------
```
