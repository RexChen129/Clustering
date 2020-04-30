###Lloyd's algorithm###
setwd("C:/Users/Lenovo/Desktop")
raw_data<-read.csv("ShapedData.csv",header = F)
k=6
n=nrow(raw_data)
plot(x=raw_data[,1],y=raw_data[,2],pch=16,col="blue")

##cluster algorithm
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

count_cent<-function(cluster_data){
  count_table<-table(cluster_data[,3])
  x<-c()
  y<-c()
  for (i in 1:k){
    x[i]<-(1/count_table[i])*sum(data[data[,3]==i,1])
    y[i]<-(1/count_table[i])*sum(data[data[,3]==i,2])
  }
  cent_point<-cbind(x,y)
  cent_point
}
#initialization:select k points randomly
initial_point<-as.matrix(raw_data[sample(row.names(raw_data), k),])
points(initial_point,col="red",pch=16)
#points(initial_point,col="red",pch=16)
cluster_int<-clusters(initial_point,raw_data)
data<-cbind(raw_data,cluster_int)
cent_points<-count_cent(data)
iteration_distance<-(cent_points-initial_point)
iteartion_rate<-sum(iteration_distance[,1]^2+iteration_distance[,2]^2)
record_iteation<-c()
while (iteartion_rate>=0.0001) {
  cluster<-clusters(cent_points,raw_data)
  data<-cbind(raw_data,cluster)
  cent_points_new<-count_cent(data)
  iteration_distance<-(cent_points_new-cent_points)
  iteartion_rate<-sum(iteration_distance[,1]^2+iteration_distance[,2]^2)
  print(iteartion_rate)
  cent_points<-cent_points_new
  append(record_iteation, iteartion_rate)
}

plot(x=raw_data[,1],y=raw_data[,2],col=data[,3],pch=16)
points(cent_points,col="yellow",pch=20,cex=2.5)

#km<-kmeans(raw_data,k)
#plot(x=raw_data[,1],y=raw_data[,2],col=km$cluster,pch=16)
#sum(data[,3]-km$cluster)
