```R
foodp = read.fwf(file = 'F:/課程/大數據_碩班/FOOD.txt',
                 header = TRUE,
                 skip = 4,
                 n = 24,
                 widths = c(13,6,10,15,6,10),
                 col.names = c("City", "Bread", "Hamburger", "Butter", "Apples", "Tomatoes"))
head(foodp)
var(foodp)
##          City    Bread Hamburger   Butter    Apples  Tomatoes
##City        NA       NA        NA       NA        NA        NA
##Bread       NA 69.99906  63.17192 25.48874 22.288804 22.413116
##Hamburger   NA 63.17192 135.33042 26.27395 18.562989 47.838949
##Butter      NA 25.48874  26.27395 85.13557 18.135973 29.773953
##Apples      NA 22.28880  18.56299 18.13597 69.872880  8.249457
##Tomatoes    NA 22.41312  47.83895 29.77395  8.249457 54.743406

##食物項目	變異數	   佔總變異百分比
##Bread	69.999 	16.864 
##Hamburger	135.330 	32.603 
##Butter	85.136 	20.511 
##Apples	69.873 	16.834 
##Tomatoes	54.743 	13.189 

pca_food=princomp(x=foodp[,2:6],cor=F) #未作標準化
summary(pca_food,loadings=T)
biplot(pca_food)
pca_food$score

pca_food_s=princomp(x=foodp[,2:6],cor=T) #作標準化
summary(pca_food_s,loadings=T)
biplot(pca_food_s)
pca_food_score=pca_food_s$score
----------------------------------------------------------------------------
  names(foodp)
x=var(foodp[2:6])
f1=prcomp(~Bread+Hamburger+Butter+Apples+Tomatoes, data=foodp, center=TRUE, scale=TRUE)
f1
load=f1$rotation
sorted.loadings1=load[order(load[,1]),1]
dotchart(sorted.loadings1, main="Loading Plot for PC1", xlab="Variable Loadings", cex=1.5, col="red") #繪製因素負荷量點圖
sorted.loadings2=load[order(load[,1]),2]
dotchart(sorted.loadings2, main="Loading Plot for PC2", xlab="Variable Loadings", cex=1.5, col="blue") 
sorted.loadings3=load[order(load[,3]),3] 
dotchart(sorted.loadings3, main="Loading Plot for PC3", xlab="Variable Loadings", cex=1.5, col="green") 
sorted.loadings4=load[order(load[,3]),4] 
dotchart(sorted.loadings4, main="Loading Plot for PC4", xlab="Variable Loadings", cex=1.5, col="black") 
plot(f1, type="line", main="Screen Plot for food") 
f1$sdev^2 #因子的標準差平方就是特徵值
biplot(f1, choices=1:2)  #因素負荷量圖
summary(f1)
names(f1)

library(psych)
f2=principal(x,nfactors=3,rotate="varimax",scores=T)

library(psych)
f3=fa.parallel(foodp, fa = "pc",n.iter = 100, show.legend = FALSE)
----------------------------------------------------------------------------
  ##clustering
  cvd=read.csv("F:/KK/CVD_All.csv",header=T)
head(cvd)
cvd_comp=cvd[complete.cases(cvd),]
head(cvd_comp)
set.seed(9487)
cvd_s = scale(cvd_comp[sample(1:57000, size = 20, replace = FALSE) , 
                       c("age", "waist", "SBP")],center = F, scale = TRUE)
summary(cvd_s)
dj = dist(cvd_s)  ##計算距離
class(dj)
cc = hclust(dj, "ward.D")  ##分群ward.D  
# 最近法method="single"
# 最遠法method="complete"
# 平均法method="average"
# 中心法method="centroid"
plot(cc) 

cluster2 = cutree(cc, k=2)
cluster3 = cutree(cc, k=3)
cluster2
cluster3


fviz_nbclust(cvd_s, 
             FUNcluster = hcut,  # hierarchical clustering
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) + 
  
  labs(title="Elbow Method for HC") +
  
  geom_vline(xintercept = 3,       # 在 X=3的地方 
             linetype = 2)         # 畫一條虛線


cc_cen=hclust(dj, "centroid") ##中心法method="centroid"
plot(cc_cen)
cluster_cen4 = cutree(cc_cen, k=4)

cc_f=hclust(dj, "complete") ##最遠法method="complete"
plot(cc_f)
cluster_f3 = cutree(cc_f, k=3)

cc_s=hclust(dj, "single") ##最近法method="single"
plot(cc_s)

cc_ave=hclust(dj, "average") ##平均法method="average"
plot(cc_ave)

cvd_stard=scale(cvd_s,center=T, scale=T)  ##標準化
summary(cvd_stard)
dj_stard = dist(cvd_stard)  ##計算距離
cc1_s = hclust(dj_stard, "ward.D")  ##分群ward.D  
plot(cc1_s)
cc2_s = hclust(dj_stard, "centroid")  ##分群中心法  
plot(cc2_s)
cc3_s = hclust(dj_stard, "complete")  ##分群最遠法 
plot(cc3_s)
cc4_s = hclust(dj_stard, "single")  ##最近法 
plot(cc4_s)
cc5_s = hclust(dj_stard, "average")  ##平均法 
plot(cc5_s)
-----------------------------------------------------------------------------------
  ##利用層次法，予以分群。
  kmeans.cluster <- kmeans(cvd_s, centers=3)
kmeans.cluster$withinss
kmeans.cluster$cluster
library(factoextra)
fviz_cluster(kmeans.cluster,           # 分群結果
             data = cvd_s,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")      # 框架型態

fviz_nbclust(cvd_s, 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) +
  
  labs(title="Elbow Method for K-Means") +
  
  geom_vline(xintercept = 3,        # 在 X=3的地方 
             linetype = 2)          # 畫一條垂直虛線


# Avg. Silhouette(側影法) 應用在 K-Means
fviz_nbclust(cvd_s, 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 12             # max number of clusters
) +
  
  labs(title="Avg.Silhouette Method for K-Means") 

-----------------------------------------------------------------------------------
  fish=read.table("F:/課程/大數據學士後_多變量_10702/_salmon.txt",header=T)
head(fish)
colnames(fish)=c("Origin","sex","freshwater","marine")
class(fish$Origin)
fish$Origin=as.factor(fish$Origin)
class(fish$sex)
fish$sex=as.factor(fish$sex)
y2=t.test(freshwater~Origin, data=fish) # 若y分兩類(三類以上用anova)用t-test測試是否有顯著差異。有:表是重要判別變數
y3=t.test(marine~Origin, data=fish)
y1=chisq.test(table(fish$sex,fish$Origin))  # 類別跟類別變項相關性檢定用"卡方檢定"，無顯著差異。sex非重要判別變數
y1
y2
y3
##cut point
mean_fre=tapply(fish$freshwater, fish$Origin, mean)
mean_mar=tapply(fish$marin, fish$Origin, mean)
mean(mean_fre)
mean(mean_mar)
fish$group1=ifelse(fish$freshwater < mean(mean_fre), 1, 2)
head(fish)
fish$group2=ifelse(fish$marin < mean(mean_mar), 2, 1)
head(fish)
t1=table(fish$group1,fish$Origin)
sum(diag(t1))/sum(t1) 
t2=table(fish$group2,fish$Origin)
sum(diag(t2))/sum(t2)
table(fish$sex,fish$Origin)

#2. posterior probability
library(MASS)
head(fish)
ld=lda(fish$Origin~fish$freshwater+fish$marin,data=fish) # fish$freshwater+fish$marin 為重要判別變數
names(ld)

ld_1$svd
z=predict(ld)
names(z)  # "posterior"=事後機率
fish$group_post=z$class # class =分類結果
cbind(fish$Origin, fish$group_post, z$x)
boxplot(fish$Origin,z$x)
t3=table(fish$group_post, fish$Origin)
sum(diag(t3))/sum(t3)
```

