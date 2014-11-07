# The Iterative Self-Organizing Data Analysis Technique that can automatically
# split or merge the clusters
ISODATA = function(data){
  completeData = normalization(data)
  #data = normalization(data)
  data = completeData[, -1]
  # NO. of clusters
  nclust <- 5
  # NO. of cases(points)
  npts <- nrow(data)
  # NO. of features
  nCol <- ncol(data)
  # NO. of iterations
  iters <- 10
  # threshold ?
  threshold <- 0.02
  # randomly choose points
  rpts.index = sample(1:npts, nclust)
  rpts = data[c(rpts.index), ]
  
  # matrix to store the centers of clusters after each iteration
  # 1st dimension: cluster id (1-15)
  # 2nd dimension: cluster name, x value, y value
  # 3rd dimension: iteration (1-1000)
  cluster.means <- array(dim=c(nclust, 1 + nCol, iters+1), 
                         dimnames=list(1:nclust, c('cluster', names(data)), NULL))
  
  # initial mean vector, randomly generated
  cluster.means[,,1] <- as.matrix(cbind(1:nclust, rpts)) 
  # generate 1000 random scattered points
  #pts <- matrix(c(runif(2*npts, 0, 10)), ncol=2)
  pts <- as.matrix(data)
  # each row represents the i-th iteration clustering result, (m, n) is the clustering result 
  # of the n-th (1 - 1000) point after the m-th iteration. 
  cluster <- matrix(nrow=npts, ncol=iters)
  # NO. of points in each cluster after the i-th iteration, nclust * npts
  inclust <- matrix(nrow=nclust, ncol=iters)
  
  # Quick peek at what's going on...
  checkpt <- 1
  x <- pts[checkpt, ] # interrogate this point
  closest <- which.min(dist(rbind(x, cluster.means[, -1, 1]))[1:nclust])
  
  for (i in 1:iters) {
    # the i-th row stores the label of all points after the i-th iteration
    cluster[,i] <- apply(pts, 1, function(x) {
      # calculate the distance between point x and the cluster centers
      # the cluster means after i-th iteration is store in cluster.means[, , 1]
      # [1:nclust] means only get the first column of distance, which is
      # x away from 1, 2, 3, ..., 15-th cluster mean
      which.min(dist(rbind(x, cluster.means[, 2:10, i]))[1:nclust])
    })
    
    # NO. of points in each cluster after the i-th iteration
    inclust[, i] <- sapply(1:nclust, function(x) length(which(cluster[, i]==x)))
    
    if (i > 1) {
      #if(all(na.omit(abs(1 - (inclust[, i] / inclust[, i-1])) < threshold))) break
    }
    # calculate the new cluster means of each cluster after i-th iteration
    new.means <- aggregate(pts, list(cluster[, i]), mean)
    colnames(new.means) <- c('cluster', names(data))
    
    # eliminate the null cluster center means, and then assign the new cluster means
    # to (i+1)-th iteration clustering result
    tmp <- as.matrix(merge(cluster.means[,, i], new.means, by='cluster', all.x=TRUE))
    tmp[, 11:19] <- tmp[, 2:10]
    cluster.means[,, i+1] <- tmp[, -(2:10)]
  }
  
  # we need to return 2 values calculated here, the first one is the clustering
  # result of each record, the other is the cluster means
  data = cbind(completeData, cluster[, iters])
  names(data)[nCol + 1] = "cluster"
  list("data" = data, "cluster.means" = cluster.means[,,iters])
}

findOutLiers = function(data, cluster.means){
  #indexID = unique(clusterIndex)
  
  #data = cbind(data, clusterIndex)
  outliers = data.frame(cluster = c(1:5), id = rep(0, 5), dis = rep(0, 5))
  for(i in 1:nrow(data)){
    distance = dist(rbind(data[i, -c(1, 11)], cluster.means[data[i, 11], ]))
    clust = data[i, 11]
    if(distance > outliers[clust, 3]){
      outliers[clust, ] = c(clust, i, distance)
    }
  }
  outliers
}

calculateAllDistances = function(data, cluster.means){
  allDistance = data.frame(country = data[, 1], 
                           distance = rep(0, nrow(data)), clust = rep(0, nrow(data)))
  
  for(i in 1:nrow(data)){
    dis = dist(rbind(data[i, -c(1, 11)], cluster.means[data[i, 11], ]))
    allDistance[i, 2:3] = c(dis, data[i, 11])
  }
  allDistance
}