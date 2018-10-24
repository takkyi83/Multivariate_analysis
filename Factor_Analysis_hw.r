```R
data = read.csv('C:\\...\\FOODP.csv',header = T, sep=",")
head(data)

##-----------------------------------------------------
library(psych)  
#r是相關係數矩陣或原始數據矩陣
#nfactors設定提取的因子數(內設值為1)
#n.obs是觀測值
#rotate旋轉法 : promax
#scores設定是否計算因子得分
#fm設定因子估計方法

pc=fa(data[,2:6],nfactors=2,n.obs=100,rotate="varimax",covar=F);pc   ##有旋轉+pc估計  =F相關係數 h2=共通性u2=唯一性
pa=fa(data[,2:6],nfactors=2,n.obs=100,rotate="varimax",covar=F,fm="pa");pa  ##有旋轉+pa估計(主軸迭代法)
mle = factanal(covmat=cor(data[,2:6]),factors=2, fm="mle");mle  ##有旋轉+MLE估計
factor.congruence(list(mle,pa,pc))
?factanal
pa_non = fa(cor(data[,2:6]),1,rotate="none",fm="pa")  ##無旋轉+pa估計(主軸迭代法)
mle_non = fa(data[,2:6],2,rotate="none",fm="mle")  ##無旋轉+MLE估計
pc_non=fa(data[,2:6],nfactors=2,n.obs=100,rotate="none",covar=F)   ##無旋轉+pc估計
factor.congruence(list(mle_non,pa_non,pc_non))

pa_pro=fa(data[,2:6],2,rotate="Promax",fm="pa");pa_pro  ##promax旋轉+pa估計(主軸迭代法) 斜交

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
install.packages('nFactors')
library(nFactors)
cor = cor(data[,2:6])
ev = eigen(cor);ev # error: data不是行和列對稱矩陣
ap = parallel (subject=nrow(data[,2:6]),var=ncol(data[,2:6]),
              rep=100,cent=.05);ap
?Deprecated
nS = nScree(x=ev$values, aparallel=ap$eigen$qevpea);nS
plotnScree(nS)

fa.parallel(data[,2:6],fa="both",n.iter=100,main="Scree plots with parallel analysis") 
```




