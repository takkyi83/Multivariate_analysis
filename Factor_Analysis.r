```R
c1=c(1.0, 0.620, 0.540, 0.320, 0.284, 0.370)
c2=c(0.620, 1.0, 0.510, 0.380, 0.351, 0.430)
c3=c(0.540, 0.510, 1.0, 0.360, 0.336, 0.405)
c4=c(0.320, 0.380, 0.360, 1.0, 0.686, 0.730)
c5=c(0.284, 0.351, 0.336, 0.686, 1.0, 0.735)
c6=c(0.370, 0.430, 0.405, 0.730, 0.735, 1.0)
data=cbind(c1,c2,c3,c4,c5,c6)
class(data)
head(data)

##-----------------------------------------------------
library(psych)  
#r是相關係數矩陣或原始數據矩陣
#nfactors設定提取的因子數(內設值為1)
#n.obs是觀測值
#rotate旋轉法 : promax
#scores設定是否計算因子得分
#fm設定因子估計方法

pc=fa(data,nfactors=2,n.obs=100,rotate="varimax",covar=F);pc   ##有旋轉+pc估計  =F相關係數 h2=共通性u2=唯一性
pa=fa(data,nfactors=2,n.obs=100,rotate="varimax",covar=F,fm="pa");pa  ##有旋轉+pa估計(主軸迭代法)
mle = factanal(covmat=data,factors=2, fm="mle");mle  ##有旋轉+MLE估計
factor.congruence(list(mle,pa,pc))

pa_non = fa(data,2,rotate="none",fm="pa")  ##無旋轉+pa估計(主軸迭代法)
mle_non = fa(data,2,rotate="none",fm="mle")  ##無旋轉+MLE估計
pc_non=fa(data,nfactors=2,n.obs=100,rotate="none",covar=F)   ##無旋轉+pc估計
factor.congruence(list(mle_non,pa_non,pc_non))

pa_pro=fa(data,2,rotate="Promax",fm="pa");pa_pro  ##promax旋轉+pa估計(主軸迭代法) 斜交

factor.plot(pc,labels=rownames(pa$loadings))
factor.plot(pc_non,labels=rownames(pa$loadings))

factor.plot(pa,labels=rownames(pa$loadings))  
factor.plot(pa_non,labels=rownames(pa_non$loadings))  
factor.plot(pa_pro,labels=rownames(pa_pro$loadings))  

fa.diagram(pa,simple=F) 
fa.diagram(pa_non,simple=F)
fa.diagram(pa_pro,simple=F)
fa.diagram(pa,simple=T)

##-----------------------------------------------------------------------------

##Determine Number of Factors to Extract
library(boot)
library(MASS)
library(lattice)
library(nFactors)
ev = eigen(data);ev
ap = parallel(subject=nrow(data),var=ncol(data),
              rep=100,cent=.05)
nS = nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa.parallel(data,fa="both",n.iter=100,main="Scree plots with parallel analysis") 
```




