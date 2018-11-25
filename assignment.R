library(readxl)
breakfast <- read_excel("F:/BITS 2-1/MATH F432 - ASM/breakfast.xlsx")
input <- breakfast[,c(1:7)]
mydata <- data.frame(input[,-1], row.names = input$ceeal)
mydata <- scale(mydata)

set.seed(50)

#------------ K MEANS CLUSTERING -------------------#

wss = (nrow(mydata)-1)*sum(apply(mydata, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:8) wss[i] = sum(kmeans(mydata, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit1 <- kmeans(mydata, 4)
fit1$cluster

library(cluster)
clusplot(mydata, fit1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
table(fit1$cluster, breakfast$ShelfLocation)
plot(silhouette(groups, d))
silhouette(fit1$cluster)



#----------------- HIERARCHICAL CLUSTERING -----------------------#

d <- dist(mydata, method = "euclidean")

fit2 <- hclust(d, method="ward.D") 
plot(fit2)
plot(fit2, hang = -1)
groups <- cutree(fit2, k=4)
rect.hclust(fit2, k=4, border="red")
table(groups, breakfast$ShelfLocation)
clusplot(mydata, groups, color=TRUE, shade=TRUE, labels=2, lines=0)
plot(silhouette(groups, d))


fit3 <- hclust(d, method="single") 
plot(fit3)
plot(fit3, hang = -1)
groups <- cutree(fit3, k=4)
rect.hclust(fit3, k=4, border="red")
table(groups, breakfast$ShelfLocation)
clusplot(mydata, groups, color=TRUE, shade=TRUE, labels=2, lines=0)
plot(silhouette(groups, d))


fit4 <- hclust(d, method="complete") 
plot(fit4)
plot(fit4, hang = -1)
groups <- cutree(fit4, k=4)
rect.hclust(fit4, k=4, border="red")
table(groups, breakfast$ShelfLocation)
clusplot(mydata, groups, color=TRUE, shade=TRUE, labels=2, lines=0)
plot(silhouette(groups, d))


fit5 <- hclust(d, method="average") 
plot(fit5)
plot(fit5, hang = -1)
groups <- cutree(fit5, k=4)
rect.hclust(fit5, k=4, border="red")
table(groups, breakfast$ShelfLocation)
clusplot(mydata, groups, color=TRUE, shade=TRUE, labels=2, lines=0)
plot(silhouette(groups, d))


fit6 <- hclust(d, method="median") 
plot(fit6)
plot(fit6, hang = -1)
groups <- cutree(fit6, k=4)
rect.hclust(fit6, k=4, border="red")
table(groups, breakfast$ShelfLocation)
clusplot(mydata, groups, color=TRUE, shade=TRUE, labels=2, lines=0)
plot(silhouette(groups, d))


fit7 <- hclust(d, method="centroid") 
plot(fit7)
plot(fit7, hang = -1)
groups <- cutree(fit7, k=4)
rect.hclust(fit7, k=4, border="red")
table(groups, breakfast$ShelfLocation)
clusplot(mydata, groups, color=TRUE, shade=TRUE, labels=2, lines=0)
plot(silhouette(groups, d))

