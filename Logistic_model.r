```R
# 解釋寫法
# OR = exp(2.403)=11.06 
  #寫法1:長度為x+1單位時，吃f的可能性(非i),是x cm 的exp(2.403)倍
  #寫法2:當長度上升一單位，吃F(而非I)的可能性，會上升(OR-1)*100%

##----第一題: 用baseline model分析alligator資料------------------------##
setwd('C:\\Users\\Sherry\\Google Drive\\OneDrive\\TMU\\_多變量')
install.packages('VGAM')
library('VGAM')
alli <- read.table("Alligator food choice.txt",header=FALSE)
names(alli) <- c('Length','Food');alli
# 把"O"當ref group
alli$Food <- relevel(alli$Food,ref="O")
vglm(formula = Food ~ Length, family=multinomial, data=alli)

# 另一種寫法
library(nnet)
fit2 <- multinom(Food ~ Length, data=alli)
summary(fit2)

# ?怎麼檢定B1 B2是否為0?

## 計算log Odds:
  # log(吃魚/吃其他) = 1.61 - 0.11x
  # log(吃無瘠椎/吃其他) = 5.67 - 2.46x
  # log (吃魚/吃無瘠椎) = -4.06 + 2.35x
## 解釋(兩種寫法):
  # 1.鱷魚長度為x+1單位時，吃魚的可能性(非無瘠椎)，是x公尺的exp(2.35)=10.48倍
  #? 2.當長度上升一單位，吃魚(而非無瘠椎)的可能性，會上升(OR-1)*100% 

##--第二題: political: 用accumulative model解釋beta----------------------## 
pol <- read.table("political ideology.txt",header=TRUE);pol
# x=1 Democrats    x=0 Republicans
# y=ideology
attach(pol)
# produce the estimated probabilities for each category for each observation
fit <- vglm(ideology~party,family=cumulative(parallel=TRUE),weights=count,data=pol);summary(fit) 
# 參數fitted(模型)->看機率
fitted(fit)

## 解釋 
  # H0假設民主跟共和黨同樣liberal
  # 此OR的95%CI是exp(0.975+-1.96*0.129) =(2.1,3,4)
  # CI不包含1 拒絕虛無假設
  # OR =exp(0.975)=2.65 民主黨傾向liberal的勝算是共和黨的2.65倍
```
