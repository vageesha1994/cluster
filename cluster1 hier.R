cluster<-read.csv("F://assignment 2//crime_data.csv")
View(cluster)
normal<-scale(cluster[,2:5])
dis<-dist(normal,method="euclidean")
fit<-hclust(dis,method = "complete")
?hclust
plot(fit)
plot(fit,hang=-1)
group<-cutree(fit,k=5)
?cutree
rect.hclust(fit,k=5,border = "green")
?rect.hclust
matrix<-as.matrix(group)
final<-data.frame(cluster,matrix)
last<-final[,c(ncol(final),1:(ncol(final)-1))]
write.csv(last,file="last.csv")
getwd()
