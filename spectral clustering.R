###Spectral Clustering algorithm,###

setwd("C:/Users/Lenovo/Desktop")
raw_data<-read.csv("clustering.csv",header = F)
k=2
#raw_data<-raw_data[1:100,]
n=nrow(raw_data)
sigma<-0.9
plot(raw_data,col="blue",pch=16)
weight_matrix<-matrix(data=NA,nrow=n,ncol=n)
#compute weight matrix#
for (i in 1:n){
    weight_matrix[,i]<-(raw_data[,1]-raw_data[i,1])^2+(raw_data[,2]-raw_data[i,2])^2
}
weight_matrix_new<-exp(-weight_matrix/sigma)
sum_w<-c()
for (i in 1:n){
  sum_w[i]<-sum(weight_matrix_new[i,])
}
D<-diag(sum_w)
I<-diag(1,nrow=n)
L<-D-weight_matrix_new
#L<-I-D^(-.5)*weight_matrix_new*D^(-.5)
L_e<-eigen(L)

uk<-L_e$vectors[,(n-k+1):n]

#km<-kmeans(uk,k)
#data<-cbind(raw_data,km$cluster)
#plot(x=data[,1],y=data[,2],col=data[,3],pch=16,colnames("x"),rownames("y"))

clusters<-function(ct,data){
  d<-ncol(ct)
  distance<-matrix(data = 0,nrow=n,ncol=k)
  cluster<-c()
  for (j in 1:k){
    for (i in 1:d){
      distance[,j]<-(data[,i]-ct[j,i])^2+distance[,j]
    }
  }
  for (i in 1:n){
    cluster[i]<-which.min(distance[i,])
  }
  cluster
}

count_cent<-function(cluster_data){
  d<-ncol(cluster_data)
  print(d)
  count_table<-table(cluster_data[,d])
  cent_point<-matrix(data=0,ncol=(d-1),nrow=k)
  for (i in 1:k){
    for (j in 1:(d-1)){
      cent_point[i,j]<-(1/count_table[i])*sum(cluster_data[cluster_data[,d]==i,j])
    }
  }
  cent_point
}

#initialization:select k points randomly
uk<-as.data.frame(uk)
initial_point<-as.matrix(uk[sample(row.names(uk), k),])
#points(initial_point,col="red",pch=16)
cluster_int<-clusters(initial_point,uk)
data<-cbind(uk,cluster_int)
cent_points<-count_cent(data)
iteration_distance<-(cent_points-initial_point)
iteartion_rate<-0
for (i in 1:k){
  iteartion_rate<-iteartion_rate+sum(iteration_distance[,i]^2)
}

record_iteation<-c()
while (iteartion_rate>=0.0001) {
  cluster<-clusters(cent_points,uk)
  data<-cbind(uk,cluster)
  cent_points_new<-count_cent(data)
  iteration_distance<-(cent_points_new-cent_points)
  iteartion_rate<-0
  for (i in 1:k){
    iteartion_rate<-iteartion_rate+sum(iteration_distance[,i]^2)
  }
  print(iteartion_rate)
  cent_points<-cent_points_new
  append(record_iteation, iteartion_rate)
}

plot(x=raw_data[,1],y=raw_data[,2],col=data[,k+1],pch=16)
#points(cent_points,col="blue",pch=18,cex=1.5)

km<-kmeans(uk,k)
plot(x=raw_data[,1],y=raw_data[,2],col=km$cluster,pch=16)
#sum(data[,3]-km$cluster)
