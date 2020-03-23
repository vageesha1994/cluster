install.packages("openxlsx")
library(openxlsx)
input<- EastWestAirlines
normal<-scale(input[,2:12])
dis<-dist(normal,method ="euclidean")
fit<-hclust(dis,method = "ward.D2")
plot(fit)
plot(fit,hang=-1)
group<-cutree(fit,k=6)
rect.hclust(fit,k=6,border="red")
matrix<-as.matrix(group,EastWestAirlines)
matrix
final<-data.frame(EastWestAirlines,group)
last<-final[,c(ncol(final),1:(ncol(final)-1))]
write.csv(last,file="dendgoram")
##k mean 
input<- EastWestAirlines
normal<-scale(input[,2:12])
plot(normal)
plot(normal,type = "n")
text(normal,rownames(normal))
km<-kmeans(normal,6)
str(km)
km$cluster
install.packages("animation")
library(animation)
km1<-kmeans.ani(normal,6)
wss = (nrow(normal)-1)*sum(apply(normal,2,var))
for(i in 2:12) wss[i] = sum(kmeans(normal,centers=i)$withinss)
plot(1:12,wss,type="b",xlab = "Number of clusters",ylab="within groups sum of squares")
title(sub="k-means clustering screen-plot")
fit<-kmeans(normal,6)
str(fit$cluster)
final2<-data.frame(input,fit$cluster)
final2
final3<-final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(input[,2:12],by=list(fit$cluster),FUN=mean)
write.csv(final3,file="kmean")
getwd()
