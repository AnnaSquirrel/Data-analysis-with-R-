#ch16 聚类分析
#选择合适变量
#缩放数据
df1 <- apply(mydata,2,function(x){(x-mean(x))/sd(x)}) 
#寻找异常点
#计算距离：曼哈顿、兰式距离、非对称二元、闵可夫斯基距离
#选择算法：层次聚类、嵌套聚类
#确定类的数目 NbClust() 
#最终方案：提取子群
#结果可视化 
#解读类：命名，组内相似，组间区别（变量汇总统计）

install.packages("flexclust") 
library(flexclust) 
data(nutrient,package="flexclust")
head(nutrient,4) 
d <- dist(nutrient) 
as.matrix(d)[1:4,1:4]

row.names(nutrient) <- tolower(row.names(nutrient)) 
nutrient.scaled <- scale(nutrient)
d <- dist(nutrient.scaled)
fit.average <- hclust(d,method="average") 
plot(fit.average,hang=-1,cex=.9,main="Average Linkage Clustering") 

install.packages("NbClust") 
library(NbClust) 
devAskNewPage(ask=TRUE) 
nc <- NbClust(nutrient.scaled,distance="euclidean",
              min.nc=2,max.nc=15,method="average") 
table(nc$Best.n[1,]) 
barplot(table(nc$Best.n[1,]),
        xlab="Number of clusters",
        ylab="Number of Crtieria" ,
        main="Number of Clusters Chosen by 26 Criteria") 

clusters <- cutree(fit.average,k=5) 
table(clusters) 
aggregate(nutrient,by=list(cluster=clusters),median) 
aggregate(as.data.frame(nutrient.scaled),by=list(cluster=clusters),median) 
plot(fit.average,hang=-1,cex=0.8,main="average linkage clustering\n5 cluster solution")
rect.hclust(fit.average,k=5) 

#k-means 
install.packages("rattle")
data(wine,package="rattle") 
head(wine)
#standarize 
df <- scale(wine[-1]) 
wssplot <- function(data, nc=15, seed=1234){ 
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){ 
    set.seed(seed) 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} 
  plot(1:nc, wss, type="b", xlab="Number of Clusters", 
       ylab="Within groups sum of squares")}
wssplot(df)
set.seed(1234)
devAskNewPage(ask=TRUE) 
nc <- NbClust(df,min.nc = 2,max.nc = 15,method = "kmeans") 
table(nc$Best.n[1,]) 
barplot(table(nc$Best.n[1,]),
        xlab="number of clusters",
        ylab="number of criteria",
        main="number of clusters chosen by 26 criteria") 
set.seed(1234)
fit.km <- kmeans(df,3,nstart = 25) 
fit.km$size  
fit.km$centers 
aggregate(wine[-1,],by=list(cluster=fit.km$cluster),mean)
ct.km <- table(wine$Type, fit.km$cluster)
library(flexclust) 
randIndex(ct.km) 


library(cluster) 
set.seed(1234) 
fit.pam <- pam(wine[-1],k=3,stand=TRUE) 
fit.pam$medoids 
clusplot(fit.pam,main="bivariate cluster plot") 
ct.pam <- table(wine$Type,fit.pam$clustering) 
randIndex(ct.pam) 

install.packages("fMultivar") 
set.seed(1234) 
df <- rnorm2d(1000,rho=0.5) 
df <- as.data.frame(df) 
plot(df,main="bivariate normal distribution with rho=0.5") 
wssplot(df)
nc <- NbClust(df,min.nc=2,max.nc=15,method="kmeans") 
dev.new()
barplot(table(nc$Best.n[1,]),
        xlab="number of clusters",
        ylab="number of criteria",
        main="number of clusters chosen by 26 criteria") 

