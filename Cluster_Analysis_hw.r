```R
# 作業:
# 化樹狀圖決定要分幾群  然後用 k-means
# 試試看分群數目不同的結果不同
# 對分群結果做命名

getwd()
setwd('C:\\Users\\Sherry\\Google Drive\\OneDrive\\TMU\\_多變量')
library(xlsx)
library(readxl)
data=read_xlsx("TABLE 7.19.xlsx",2)
head(data)

data_a=scale(data[,2:6],center=T, scale=T);data_a  ##標準化
summary(data_a)

#距離
dj = dist(data_a)  ##歐氏距離
dM = dist(data_a,method="manhattan");dM  ##曼哈頓距離
h.E.cluster <- hclust(dj)
plot(h.E.cluster, xlab="歐氏距離") 
h.M.cluster <- hclust(dM)
plot(h.M.cluster, xlab="曼哈頓距離")

# 華德法
ward=hclust(dj, method="ward.D2")  
plot(ward, xlab="華德法")
abline(h=10, col="red")

# k-means
kmeans.cluster = kmeans(dj, centers=9);kmeans.cluster
names(kmeans.cluster)
kmeans.cluster$withinss  ##群內變異數
kmeans.cluster$tot.withinss  ##群內變異數和
kmeans.cluster$totss  ##總變異數
kmeans.cluster$betweenss  ##群間變異數
kmeans.cluster
#???? table(kmeans.cluster$cluster, data_a[])


library(factoextra)
require(factoextra)
fviz_cluster(kmeans.cluster,           # 分群結果
             data = dj,              # 資料????應該用dj還是data_a
             geom = c("point","text"),# 點和標籤(point & label)??如何show食物名
             show.clust.cent = TRUE, 
             frame.type = "norm",
             outlier.color = "black",
             outlier.shape = 19)
#分為三大群，九個小群
#對分群結果做命名:
  # 高卡路里高脂肪食物: 烤牛肉,牛心,烤羊腿,烤羊肩膀肉,燻火腿,烤豬肉
  # 中卡路里低脂肪食物:燙豬肉,炸牛肉, 炸鱈魚,罐裝鯖魚
  # 中卡路里低脂肪食物:罐裝蚌,罐裝蟹肉,烤鯖魚,炸鱸魚


##最佳分群數目:3群
fviz_nbclust(data_a, 
             FUNcluster = hcut,  # "hcut":hierarchical clustering  ### "kmeans":K-Means    # "pam":K-Medoid
             method = "wss",     # total within sum of square  ## "silhouette" (for average silhouette width), "wss" (for total within sum of square) and "gap_stat" (for gap statistics).
             k.max = 12          # max number of clusters to consider
) + 
  labs(title="Elbow Method for HC") + geom_vline(xintercept = 3, linetype = 2)  
#???? argument frame is deprecated

##應用在k-means:側影平均值(silhouette):11群
fviz_nbclust(data_a, 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 12             # max number of clusters
) +
  
  labs(title="Avg.Silhouette Method for K-Means")


##K-Medoid:robust version of K-means (中心點為最臨近平均數的真實值+object function):2群
library(cluster)
kmedoid.cluster <- pam(data_a, k=3);kmedoid.cluster
kmedoid.cluster$objective
#???table(kmedoid.cluster$clustering, iris$Species) 
require(factoextra)
fviz_cluster(kmedoid.cluster,       # 分群結果
             data = data_a,           # 資料
             geom = c("point",'text'),     # 點 (point)
             frame.type = "norm")   # 框架型態

#分為三大群
#對分群結果做命名:
  #高卡路里: 燙豬肉,炸牛肉,炸鱈魚,烤羊肩膀肉,燻火腿,燉牛肉,漢堡
  #低鐵: 罐裝雞肉,牛舌
  #其他剩下的為一類
```



