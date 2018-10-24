```R
getwd()
setwd
library(xlsx)
library(readxl)
CVD=read_xlsx("CVD_All_HW_EN.xlsx")
head(CVD)
names(CVD)
mean(CVD$age,na.rm=T)
CVD_a <- CVD[complete.cases(CVD),] ##將完整資料留下
names(CVD_a)
table(CVD_a[1:50,2])
X1=CVD_a[1:50,c(2,3,6,7)]
head(X1)
X1=scale(X1,center=T, scale=T)  ##標準化
summary(X1)
dj = dist(X1)  ##歐氏距離
dM = dist(X1,method="manhattan")  ##曼哈頓距離
par(mfrow=c(1,2))
h.E.cluster <- hclust(dj)
plot(h.E.cluster, xlab="歐式距離")
h.M.cluster <- hclust(dM)
plot(h.M.cluster, xlab="曼哈頓距離")

nearst=hclust(dj, method="single")   # 最近法
plot(nearst, xlab="最近法")
hclust(dj, method="complete") # 最遠法
hclust(dj, method="average")  # 平均法
hclust(dj, method="centroid") # 中心法

ward=hclust(dj, method="ward.D2")  # 華德法
plot(ward, xlab="華德法")
abline(h=10, col="red")

cut.cluster <- cutree(ward, k=2)  # 分成二群
cut.cluster                       # 分群結果
table(cut.cluster, X1[,1]) 
library(ggplot2)
X1=data.frame(X1);X1
colnames(X1)=c("CVD","age","waist","SBP")
names(X1)
qplot(SBP, age, data = X1, color = CVD)
##-----------------------------------------------------------------------------------------------------------
##以iris資料為例
names(iris)
attach(iris)
X = scale(iris[ , c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],center = T, scale = TRUE)
summary(X)
dj = dist(X, method="euclidean")
class(dj)
cc0 = hclust(dj)
plot(cc0)
cc = hclust(dj, "ward.D2")
plot(cc)
abline(h=9, col="red")

cluster3 = cutree(cc, k=3)
table(cluster3, iris$Species) 
library(ggplot2)
qplot(Petal.Length, Petal.Width, data = iris, color = Species)

##-----------------------------------------------------------------------------------------------------
##Kmeans
kmeans.cluster = kmeans(dj, centers=3) 
names(kmeans.cluster)
kmeans.cluster$withinss  ##群內變異數
kmeans.cluster$tot.withinss  ##群內變異數和
kmeans.cluster$totss  ##總變異數
kmeans.cluster$betweenss  ##群間變異數
table(kmeans.cluster$cluster, iris$Species)
kmeans.cluster$cluster

install.packages('factoextra')
library(factoextra)
require(factoextra)
fviz_cluster(kmeans.cluster,           # 分群結果
             data = dj,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")


##最佳分群數目
fviz_nbclust(data_a, 
             FUNcluster = hcut,  # "hcut":hierarchical clustering  ### "kmeans":K-Means    # "pam":K-Medoid
             method = "wss",     # total within sum of square  ## "silhouette" (for average silhouette width), "wss" (for total within sum of square) and "gap_stat" (for gap statistics).
             k.max = 12          # max number of clusters to consider
) + 
  labs(title="Elbow Method for HC") + geom_vline(xintercept = 3, linetype = 2)  


##應用在k-means:側影平均值(silhouette)
fviz_nbclust(data_a, 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 12             # max number of clusters
) +
  
  labs(title="Avg.Silhouette Method for K-Means")


##K-Medoid:robust version of K-means (中心點為最臨近平均數的真實值+object function)
library(cluster)
kmedoid.cluster <- pam(X, k=3) 
kmedoid.cluster$objective
table(kmedoid.cluster$clustering, iris$Species) 
require(factoextra)
fviz_cluster(kmedoid.cluster,       # 分群結果
             data = X,           # 資料
             geom = c("point"),     # 點 (point)
             frame.type = "norm")   # 框架型態
```

