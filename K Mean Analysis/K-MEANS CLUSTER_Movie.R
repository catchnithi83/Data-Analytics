

#Reading the data

data <- read.csv ("clipboard", sep = "\t", header = TRUE)
data <- read.csv("K-Mean_cluster_movie.csv")
#NORMALISING THE DATA
View(data)
?scale
ndata <- scale (data[,c(2:6)])  ##scale except customer number other feild to be scaled


# RUN THE CLUSTER ANLAYSIS WITH 2 CLUSTER
#FIX THE SEED FIRST FOR REPLICABILITY
set.seed(123)
cluster_1<-kmeans(ndata,2)
cluster_1

data$cluster_2 <- cluster_1$cluster

data$cluster_2 <- factor(data$cluster_2, levels = c(1, 2), labels = c("H&A lovers", "F&RC&C lovers"))


cluster_1$totss
cluster_1$betweenss
cluster_1$tot.withinss
# total = 1450
# total of within = 765.4587
# between = 684.5413

765.4587+684.5413
(684.5413/1450)*100

data$two_cluster <- cluster_1$cluster
data$two_cluster <- factor(data$two_cluster, levels = c(1,2), labels = c("H&A Lovers", "R&F&C Lovers"))


(684.5413/1450)*100

#RUN THE REVISED CLUSTER SOLOUTION WITH 3 CLUSTER
set.seed(123)
cluster_2<-kmeans(ndata,3)
cluster_2
cluster_2$betweenss
data$three_cluster <- cluster_2$cluster


#RUN THE REVISED CLUSTER SOLOUTION WITH 4 CLUSTER
set.seed(123)
cluster_3<-kmeans(ndata,4)
cluster_3



##PLOT THE CLUSTER SOLUATIONS 

install.packages("factoextra")
library(factoextra) 

?fviz_cluster

fviz_cluster(cluster_1, data = ndata)

fviz_nbclust(ndata, kmeans, method = "wss")

fviz_nbclust (ndata, kmeans, method = "silhouette")

data$two_cluster <- cluster_1$cluster
data$three_cluster <- cluster_2$cluster
data$four_cluster <- cluster_3$cluster




## ANOTHER WAY TO PLOT
install.packages("cluster")
library("cluster")
?clusplot
clusplot(ndata, cluster_2$cluster,
         color = TRUE, shade = TRUE,
         labels = 5,lines = 0)


## HIGHERARCHIAL CLUSTERING 

# Calculate distance matrix  
distance <- dist(ndata)

# Hierarchical agglomerative clustering  

hclust <- hclust(distance)
plot(hclust)
plot(hclust,main='Default from hclust')
plot(hclust,hang= -1)

# Hierarchical agglomerative clustering using "average" linkage 
hclust1<-hclust(distance,method="average")

plot(hclust1,hang=-1)

hclust2<-hclust(distance,method="single")
plot(hclust2,hang=-1)

rect.hclust(hclust, k = 2, border = "green")
?hclust

# Cluster membership
member <- cutree(hclust,5)
table(member)

data$group <- member


# Characterizing clusters (with and without normlised data)
aggregate(ndata,list(member),mean)

aggregate(data[,-1],list(member),mean)

# Silhouette Plot
library(cluster) 
plot(silhouette(cutree(hclust,4), distance))
