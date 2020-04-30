###GreedyKCenters###  
setwd("C:/Users/Lenovo/Desktop")
raw_data<-read.csv("ShapedData.csv",header = F)
k=6
n=nrow(raw_data)
plot(x=raw_data[,1],y=raw_data[,2],col="blue",pch=16)
#initialization:select a point randomly
initial_point<-as.matrix(raw_data[sample(row.names(raw_data), 1),])
distance_matrix<-matrix(data=NA,nrow=n,ncol=k)
point_matrix<-matrix(data=NA,nrow=k,ncol=2)
point_matrix[1,]<-initial_point
points(x=point_matrix[1,1],y=point_matrix[1,2],col="red",pch=16,cex=2)
i=1
distance_matrix[,i]<-(raw_data[,1]-point_matrix[1,1])^2+(raw_data[,2]-point_matrix[1,2])^2
y<-which.max(distance_matrix[,1])
for (i in 2:k) {
  point_new<-as.matrix(raw_data[y,])
  point_matrix[i,]<-point_new
  distance_matrix[,i]<-(raw_data[,1]-point_matrix[i,1])^2+(raw_data[,2]-point_matrix[i,2])^2
  min_dis=c()
  for (j in 1:n){
    min_dis[j]<-min(distance_matrix[j,1:i])
  }
  y<-which.max(min_dis)
  points(x=point_matrix[i,1],y=point_matrix[i,2],col="red",pch=16,cex=2)
}

clusters<-function(ct,data){
  distance<-matrix(data = NA,nrow=n,ncol=k)
  cluster<-c()
  for (j in 1:k){
    distance[,j]<-(data[,1]-ct[j,1])^2+(data[,2]-ct[j,2])^2
  }
  for (i in 1:n){
    cluster[i]<-which.min(distance[i,])
  }
  cluster
}

cluster<-clusters(point_matrix,raw_data)
data<-cbind(raw_data,cluster)
plot(x=raw_data[,1],y=raw_data[,2],col=data[,3],pch=16)
points(point_matrix,col="yellow",pch=20,cex=2.5)



