euclideandistance<-function(p1,p2){
  return(sqrt(sum((p1-p2)^2)))
}

kmeans<-function(intial_centroids,data,maxiter=10){
  k<-nrow(intial_centroids)
  centroids<-intial_centroids
  clusters<-rep(0,nrow(data))
  for (iter in 1:maxiter){
    cat("Iteration:",iter,"\n")
    for (i in 1:nrow(data)){
      distances<-apply(centroids,1,euclideandistance,p2=data[i,])
      clusters[i]<-which.min(distances)
    }
    print(data.frame(as.matrix(data),cluster=clusters))
    
    newcentroids<-matrix(0,nrow=k,ncol=ncol(data))
    for(j in 1:k){
      cluster_points<-data[clusters==j,]
      if (nrow(cluster_points)>0){
        newcentroids[j,]<-colMeans(cluster_points)
      }
      else{
        newcentroids[j,]<-centroids[j,]
      }
    }
    if (all( newcentroids==centroids)){
      cat("converged")
      break
    }
    centroids<-newcentroids
    
  }
  return(list(Clusters=clusters,Centroids=centroids))
}



cat("Enter the number of datapoints:")
npoints<-as.integer(readline())
datapoints<-matrix(nrow = npoints,ncol=2)
for(i in 1:npoints){
  cat("enter ",i,"datapoint x and y:")
  values<-as.numeric(strsplit(readline()," ")[[1]])
  datapoints[i,]<-values
}
cat("Enter number of clusters:")
ncluster<-as.numeric(readline())
i_centroids<-matrix(nrow=ncluster,ncol=2)
for(j in 1:ncluster){
  cat("Enter x and y of centroid",j,":")
  value<-as.numeric(strsplit(readline()," ")[[1]])
  i_centroids[j,]<-value
}
cat("intial centroids:")
print(i_centroids)
data_df<-as.data.frame(datapoints)
colnames(data_df)<-c("x","y")
result<-kmeans(i_centroids,data_df)
print(result$Clusters)
print(result$Centroids)
plot(data_df$x,data_df$y,main="Kmeans clustering",xlab="x",ylab = "y",col="blue")
points(result$Centroids,col="black")
