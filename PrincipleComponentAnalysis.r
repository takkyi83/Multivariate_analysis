```R
### Q:(a) 利用主成分分析定義基於5種食物的物價指標
### Q:(b) 由物價指標指出物價最高與最低的城市。若改用相關矩陣萃取主成分則物價最高與最低的城市是否改變?
### 哪一種型態的資料較適宜用來計算物價指標?  為什麼?
### Q:(c) 利用主成分計分做散佈圖並將城市分群，群與群間有何不同?
data = read.csv('C:\\Users\\Sherry\\Google Drive\\OneDrive\\TMU\\_多變量\\FOODP.csv',header = T, sep=",")
head(data)

#計算平均
v<-c(0,0,0,0,0)
i = 1
for (n in data){
  v[[i]] = mean(data[,i])
  i = i + 1
}
v
#均值化
data = read.csv('C:\\Users\\Sherry\\Google Drive\\OneDrive\\TMU\\_多變量\\FOODP_MeanCorrected.csv',header = T, sep=",")
head(data)
data_cor=cor(data[,2:6]);data_cor # 相關係數矩陣
data_cov=cov(data[,2:6]);data_cov # 共變異數矩陣
# 主成分分析，sd^2 = eigen value = lamda ^2 
data_pca=princomp(x=data[,2:6],cor=F);data_pca 
summary(data_pca)
# Loadings
unclass(data_pca$loadings)
#主成分計分，即特徵向量
head(data_pca$scores)
biplot(data_pca)
#### ANS(b):畫圖後顯示Buffalo物價最高，Anchorage物價最低

### ANS(a):五個主成分 Comp.1變異數為216.67, 佔總變異52%
### Comp.1 = -0.453*Bread -0.715*Hamburger -0.339*Butter -0.220*Apples -0.347*Tomatoes
### 由上式權重可知Hamburger的價格影響Comp.1最大

#利用相關係數進行主成分分析
data_pca_r=princomp(x=data[,2:6], cor=T);data_pca_r
summary(data_pca_r,loading=T)
#主成分計分
head(data_pca_r$scores)
biplot(data_pca)
#### ANS(b):改用相關矩陣萃取主成分，畫圖後仍然顯示Buffalo物價最高，Anchorage物價最低
#### ANS(c): 由圖形可大致看出城市依物價分為兩群
```
