###single-swap heuristic###  
setwd("C:/Users/Lenovo/Desktop")
raw_data<-read.csv("ShapedData.csv",header = F)
k=6
n=nrow(raw_data)
#plot(x=raw_data[,1],y=raw_data[,2],col="blue",pch=16)
#initialization:select a point randomly

greedy_centers<-function(data){
  initial_point<-as.matrix(data[sample(row.names(data), 1),])
  y1<-which(data==initial_point)
  distance_matrix<-matrix(data=NA,nrow=n,ncol=k)
  point_matrix<-matrix(data=NA,nrow=k,ncol=3)
  point_matrix[1,1:2]<-initial_point
  point_matrix[1,3]<-y1
  i=1
  distance_matrix[,i]<-(data[,1]-point_matrix[1,1])^2+(data[,2]-point_matrix[1,2])^2
  y<-which.max(distance_matrix[,1])
  
  for (i in 2:k) {
    point_new<-as.matrix(data[y,])
    point_matrix[i,1:2]<-point_new
    distance_matrix[,i]<-(data[,1]-point_matrix[i,1])^2+(data[,2]-point_matrix[i,2])^2
    min_dis=c()
    for (j in 1:n){
      min_dis[j]<-min(distance_matrix[j,1:i])
    }
    y<-which.max(min_dis)
    #points(x=point_matrix[i,1],y=point_matrix[i,2],col="red",pch=16,cex=2)
    point_matrix[i,3]<-y
  }
  point_matrix
}

count_cluster<-function(ct,data){
  distance<-matrix(data = NA,nrow=n,ncol=k)
  min_distance<-c()
  cluster<-c()
  for (j in 1:k){
    distance[,j]<-(data[,1]-ct[j,1])^2+(data[,2]-ct[j,2])^2
  }
  for (i in 1:n){
    cluster[i]<-which.min(distance[i,])
    min_distance[i]<-min(distance[i,])
  }
  new_distance<-cbind(cluster,min_distance)
  new_distance
}

point_matrix<-as.data.frame(greedy_centers(raw_data))
colnames(point_matrix)<-c("x","y","index")
plot(x=raw_data[,1],y=raw_data[,2],col="blue",pch=16)
points(point_matrix,col="red",pch=16)
cluster_data<-count_cluster(point_matrix[,1:2],raw_data)
data<-cbind(raw_data,cluster_data)
data_index<-as.data.frame(rownames(data))
data<-cbind(data,data_index)
colnames(data)<-c("x","y","cluster","min_distance","index")
data_swap<-data[-c(point_matrix[,3]),]
cost<-max(data_swap[,4])
##choose and swap point
for (i in 1:1000){
  m_new<-as.data.frame(data_swap[sample(row.names(data_swap), 1),c(1,2,5)])
  c_old<-as.data.frame(point_matrix[sample(row.names(point_matrix), 1),])
  point_matrix_new<-rbind(point_matrix[-which(point_matrix[,3]==c_old[1,3]),],m_new)
  cluster_data_new<-count_cluster(point_matrix_new[,1:2],raw_data)
  data_new<-cbind(raw_data,cluster_data_new,data_index)
  data_swap_new<-data[-as.numeric(c(point_matrix_new[,3])),]
  cost_new<-max(data_new[,4])
  if (cost_new/cost<=0.95){
    point_matrix<-point_matrix_new
    cost<-cost_new
    data_swap<-data_swap_new
    print(cost)
  }
}

plot(x=raw_data[,1],y=raw_data[,2],col=cluster_data_new[,1],pch=16)
points(point_matrix,col="yellow",pch=18,cex=2)

