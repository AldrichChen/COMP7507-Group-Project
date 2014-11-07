
# NO. of clusters
nclust <- 15
# NO. of cases(points)
npts <- 1000
# NO. of features
nCol <- 1000
# NO. of iterations
iters <- 1000
# threshold ?
threshold <- 0.02

# matrix to store the centers of clusters after each iteration
# 1st dimension: cluster id (1-15)
# 2nd dimension: cluster name, x value, y value
# 3rd dimension: iteration (1-1000)
cluster.means <- array(dim=c(nclust, 3, iters+1), 
                       dimnames=list(1:nclust, c('cluster', 'x', 'y'), NULL))

# initial mean vector, randomly generated
cluster.means[,,1] <- c(1:nclust, runif(2*nclust, 0, 10)) 
# generate 1000 random scattered points
pts <- matrix(c(runif(2*npts, 0, 10)), ncol=2)
#pts <- as.matrix(data)
# each row represents the i-th iteration clustering result, (m, n) is the clustering result 
# of the n-th (1 - 1000) point after the m-th iteration. 
cluster <- matrix(nrow=npts, ncol=iters)
# NO. of points in each cluster after the i-th iteration, nclust * npts
inclust <- matrix(nrow=nclust, ncol=iters)

# Quick peek at what's going on...
checkpt <- 1
x <- pts[checkpt, ] # interrogate this point
closest <- which.min(dist(rbind(x, cluster.means[, -1, 1]))[1:nclust])
# plot all the points, pch is the point type, cex is the size of point
plot(pts, pch=20, cex=0.8, col='gray40', xlim=c(0, 10), ylim=c(0, 10), xlab='x', 
     ylab='y', main=sprintf('Closest to point %s is cluster %s', checkpt, closest))
# plot cluster means point, bigger than the other points 
points(cluster.means[, -1, 1], cex=3, pch=20)
# draw lines from the 1st point x to the 15 cluster means, the length of lines
# denote the distance
segments(rep(pts[checkpt,1], nclust), rep(pts[checkpt,2], nclust), 
         cluster.means[, 2, 1], cluster.means[, 3, 1], lwd=2)
# give label to the 15 cluster means points
text(cluster.means[, 2, 1], cluster.means[, 3, 1], cex=0.7, col=0)

library(RColorBrewer)
library(plyr)
cols <- c(brewer.pal(8, 'Dark2'), brewer.pal(12, 'Paired'), brewer.pal(9, 'Set1'))

for (i in 1:iters) {
  # the i-th row stores the label of all points after the i-th iteration
  cluster[,i] <- apply(pts, 1, function(x) {
    # calculate the distance between point x and the cluster centers
    # the cluster means after i-th iteration is store in cluster.means[, , 1]
    # [1:nclust] means only get the first column of distance, which is
    # x away from 1, 2, 3, ..., 15-th cluster mean
    which.min(dist(rbind(x, cluster.means[, 2:3, i]))[1:nclust])
  })
  
  # NO. of points in each cluster after the i-th iteration
  inclust[, i] <- sapply(1:nclust, function(x) length(which(cluster[, i]==x)))
  
  if (i > 1) {
    if(all(na.omit(abs(1 - (inclust[, i] / inclust[, i-1])) < threshold))) break
  }
  # calculate the new cluster means of each cluster after i-th iteration
  new.means <- aggregate(pts, list(cluster[, i]), mean)
  colnames(new.means) <- c('cluster', 'x', 'y')
  
  # eliminate the null cluster center means, and then assign the new cluster means
  # to (i+1)-th iteration clustering result
  tmp <- as.matrix(merge(cluster.means[,, i], new.means, by='cluster', all.x=TRUE))
  tmp[is.na(tmp[,4]), 4:5] <- tmp[is.na(tmp[,4]), 2:3]
  cluster.means[,, i+1] <- tmp[, -(2:3)]
  
  # type='n' for no plotting, no points plotted at this step
  plot(pts, xlim=c(0, 10), ylim=c(0, 10), type='n', ylab='y', xlab='x')
  sapply(unique(cluster[,i]), function(x) {
    p <- pts[cluster[,i]==x,]
    hull <- chull(p)
    polygon(p[hull,1], p[hull,2], border=1, lwd=2)
  })
  points(pts, pch=21, bg=cols[cluster[, i]], xlim=c(0, 10), ylim=c(0, 10))
  points(cluster.means[,-1,i], pch=21, cex=2.5, bg=cols, col=1, lwd=3)
  text(cluster.means[, 2, i], cluster.means[, 3, i], cex=0.8, col=1, font=2)
}