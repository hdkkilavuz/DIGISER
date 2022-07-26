

setwd("C:/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/Project/Code")
data <- read.csv("indices.csv", head=T, sep = ";")
data <- data[,-1]
data_init <- data













#### TAKING INTO ACCOUNT THE INDICES IN THE DEEPEST LEVELS ####
#
## We cluster the cities based on the indices that are in the deepest levels.
#
#
#
#
#
# Indices considered:
#
## I1_1_1 Digitization
## I1_1_2 Innovative technologies
## I1_1_3 Advanced methods and principles
#
## I1_2_1 Scaling deep
## I1_2_2 Scaling out
## I1_2_3 Scaling up
#
## I2_1_1 Context empowerment
## I2_1_2 Replication and diffusion
## I2_1_3 Organizational readiness
#
## I2_2_1_1 Data Platform
## I2_2_1_2 Data Use
## I2_2_1_3 Data Strategy
## I2_2_1_4 Open Data
## I2_2_1_5 Big Data
## I2_2_2   Procurement
## I2_2_3_1 Co-creation
## I2_2_3_2 E-participation
## I2_2_3_3 Social Media Presence
## I2_2_4_1 Innovation strategy
## I2_2_4_2 Proneness to experiment
## I2_2_4_3 Skills






data <- data_init[,-c(1,2,3,7,11,12,16,17,22,26,32)]

n <- dim(data)[1]
p <- dim(data)[2]







### EUCLIDEAN DISTANCE


distance <- 'euclidean' # manhattan, canberra

linkages <- c('single', 'average', 'complete', 'ward.D2')



# distance matrix
# it contain the distances between all the units according to a specific distance metric

data.dist <- dist(data, method=distance)


# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])


# dendograms (2 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=2)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=2)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=2)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=2)




# dendograms (3 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=3)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=3)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=3)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=3)




# select the best linkage and best k:

# In the single linkage we have the chain effect:
# every small cluster attacks to a bigger one, and no bigger cluster attacks to another big one
# this generate always small distances between levels and it is complex to discriminate a good number 
# of clusters.

# We choose as linkage the WARD.D2, because it discriminate the structure of the data in a 
# good way.

# To choose k, we look at those vertical lines whose length between a level and another level
# is the maximum in the dendogram
# That would be a right number of clusters to divide the data with
# also because, if we have some perturbation, we have a large enough distance to handle this
# In this case, we can pick k=2, and divide the data in two clusters.


k <- 2
linkage <- 'ward-D2' # average, complete, ward.D2
data.hc1_eucl <- data.w #s, a, c, w
par(mfrow=c(1,1))
plot(data.hc1_eucl, main=paste('Deepest Level. Distance used: ', distance, ',  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc1_eucl, k=2)




# cut dendogram:
# for each observation (that are unordered and permuted) we assign a cluster
clusters1_eucl <- cutree(data.hc1_eucl, k=2)


#Centers of the Clusters
center1_eucl <- matrix(0,k,p)
for (i in 1:k) {
  center1_eucl[i,] <- colMeans(data[clusters1_eucl==i, ])
}
colnames(center1_eucl) <- colnames(data)




## CLUSTER 1: is grouping cities with average indices:
center1_eucl[1,]

## CLUSTER 2: is grouping cities with average indices:
center1_eucl[2,]












### MANHATTAN DISTANCE


distance <- 'manhattan'

linkages <- c('single', 'average', 'complete', 'ward.D2')



# distance matrix
# it contain the distances between all the units according to a specific distance metric

data.dist <- dist(data, method=distance)


# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])


# dendograms (2 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=2)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=2)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=2)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=2)




# dendograms (3 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=3)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=3)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=3)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=3)




# select the best linkage and best k:

# In the single linkage we have the chain effect:
# every small cluster attacks to a bigger one, and no bigger cluster attacks to another big one
# this generate always small distances between levels and it is complex to discriminate a good number 
# of clusters.

# We choose as linkage the WARD.D2, because it discriminate the structure of the data in a 
# good way.

# To choose k, we look at those vertical lines whose length between a level and another level
# is the maximum in the dendogram
# That would be a right number of clusters to divide the data with
# also because, if we have some perturbation, we have a large enough distance to handle this
# In this case, we can pick k=2, and divide the data in two clusters.


k <- 2
linkage <- 'ward-D2' # average, complete, ward.D2
data.hc1_manh <- data.w #s, a, c, w
par(mfrow=c(1,1))
plot(data.hc1_manh, main=paste('Deepest Level. Distance used: ', distance, ',  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc1_manh, k=2)




# cut dendogram:
# for each observation (that are unordered and permuted) we assign a cluster
clusters1_mahn <- cutree(data.hc1_manh, k=2)



#Centers of the Clusters
center1_mahn <- matrix(0,k,p)
for (i in 1:k) {
  center1_mahn[i,] <- colMeans(data[clusters1_mahn==i, ])
}
colnames(center1_mahn) <- colnames(data)





## CLUSTER 1: is grouping cities with average indices:
center1_mahn[1,]

## CLUSTER 2: is grouping cities with average indices:
center1_mahn[2,]





















### CANBERRA DISTANCE


distance <- 'canberra'

linkages <- c('single', 'average', 'complete', 'ward.D2')



# distance matrix
# it contain the distances between all the units according to a specific distance metric

data.dist <- dist(data, method=distance)


# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])


# dendograms (2 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=2)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=2)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=2)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=2)




# dendograms (3 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=3)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=3)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=3)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=3)




# select the best linkage and best k:

# In the single linkage we have the chain effect:
# every small cluster attacks to a bigger one, and no bigger cluster attacks to another big one
# this generate always small distances between levels and it is complex to discriminate a good number 
# of clusters.

# We choose as linkage the WARD.D2, because it discriminate the structure of the data in a 
# good way.

# To choose k, we look at those vertical lines whose length between a level and another level
# is the maximum in the dendogram
# That would be a right number of clusters to divide the data with
# also because, if we have some perturbation, we have a large enough distance to handle this
# In this case, we can pick k=2, and divide the data in two clusters.


k <- 2
linkage <- 'ward-D2' # average, complete, ward.D2
data.hc1_canb <- data.w #s, a, c, w
par(mfrow=c(1,1))
plot(data.hc1_canb, main=paste('Distance used: ', distance, ',  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc1_canb, k=2)




# cut dendogram:
# for each observation (that are unordered and permuted) we assign a cluster
clusters1_canb <- cutree(data.hc1_canb, k=2)




#Centers of the Clusters
center1_canb <- matrix(0,k,p)
for (i in 1:k) {
  center1_canb[i,] <- colMeans(data[clusters1_canb==i, ])
}
colnames(center1_canb) <- colnames(data)





## CLUSTER 1: is grouping cities with average indices:
center1_canb[1,]

## CLUSTER 2: is grouping cities with average indices:
center1_canb[2,]









































#### TAKING INTO ACCOUNT SOME POLITICAL RELATED INDICES ####
#
## We cluster the cities based on the indices that are in the deepest levels.
#
#
#
#
#
# Indices considered:
#
## I2_1_1 Context empowerment
## I2_1_2 Replication and diffusion
## I2_1_3 Organizational readiness
## I2_2_4_1 Innovation strategy







data <- data_init[,-c(1,2,3,4,5,6,7,8,9,10,11,12,16,17,18,19,20,21,22,23,24,25,26,28,29,30,31,32)]

n <- dim(data)[1]
p <- dim(data)[2]







### EUCLIDEAN DISTANCE


distance <- 'euclidean' # manhattan, canberra

linkages <- c('single', 'average', 'complete', 'ward.D2')



# distance matrix
# it contain the distances between all the units according to a specific distance metric

data.dist <- dist(data, method=distance)


# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])


# dendograms (2 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=2)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=2)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=2)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=2)




# select the best linkage and best k:

# In the single linkage we have the chain effect:
# every small cluster attacks to a bigger one, and no bigger cluster attacks to another big one
# this generate always small distances between levels and it is complex to discriminate a good number 
# of clusters.

# We choose as linkage the WARD.D2, because it discriminate the structure of the data in a 
# good way.

# To choose k, we look at those vertical lines whose length between a level and another level
# is the maximum in the dendogram
# That would be a right number of clusters to divide the data with
# also because, if we have some perturbation, we have a large enough distance to handle this
# In this case, we can pick k=2, and divide the data in two clusters.


k <- 2
linkage <- 'ward-D2' # average, complete, ward.D2
data.hc3_eucl <- data.w #s, a, c, w
par(mfrow=c(1,1))
plot(data.hc3_eucl, main=paste('Political indices. Distance used: ', distance, ',  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc3_eucl, k=2)




# cut dendogram:
# for each observation (that are unordered and permuted) we assign a cluster
clusters3_eucl <- cutree(data.hc3_eucl, k=2)


#Centers of the Clusters
center3_eucl <- matrix(0,k,p)
for (i in 1:k) {
  center3_eucl[i,] <- colMeans(data[clusters3_eucl==i, ])
}
colnames(center3_eucl) <- colnames(data)




## CLUSTER 1: is grouping cities with average indices:
center3_eucl[1,]

## CLUSTER 2: is grouping cities with average indices:
center3_eucl[2,]











































#### TAKING INTO ACCOUNT THE DPSVI INDEX ####
#
## We cluster the cities based on the DPSVI index.
#
#
#
#





data <- data_init[,-(1:30)]
data[,1] <- NULL

n <- dim(data)[1]
p <- dim(data)[2]







### EUCLIDEAN DISTANCE


distance <- 'euclidean' # manhattan, canberra

linkages <- c('single', 'average', 'complete', 'ward.D2')



# distance matrix
# it contain the distances between all the units according to a specific distance metric

data.dist <- dist(data, method=distance)


# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])


# dendograms (2 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=2)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=2)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=2)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=2)




# dendograms (3 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=3)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=3)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=3)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=3)




# select the best linkage and best k:

# In the single linkage we have the chain effect:
# every small cluster attacks to a bigger one, and no bigger cluster attacks to another big one
# this generate always small distances between levels and it is complex to discriminate a good number 
# of clusters.

# We choose as linkage the WARD.D2, because it discriminate the structure of the data in a 
# good way.

# To choose k, we look at those vertical lines whose length between a level and another level
# is the maximum in the dendogram
# That would be a right number of clusters to divide the data with
# also because, if we have some perturbation, we have a large enough distance to handle this
# In this case, we can pick k=3, and divide the data in two clusters.


k <- 3
linkage <- 'ward-D2' # average, complete, ward.D2
data.hc2_eucl <- data.w #s, a, c, w
par(mfrow=c(1,1))
plot(data.hc2_eucl, main=paste('Distance used: ', distance, ',  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc2_eucl, k=3)




# cut dendogram:
# for each observation (that are unordered and permuted) we assign a cluster
clusters2_eucl <- cutree(data.hc2_eucl, k=3)





#Centers of the Clusters
center2_eucl <- matrix(0,k,p)
for (i in 1:k) {
  center2_eucl[i,] <- mean(data[clusters2_eucl==i, ])
}
colnames(center2_eucl) <- colnames(data)





## CLUSTER 1: is grouping cities with average indices 0.4145287:
center2_eucl[1,]

## CLUSTER 2: is grouping cities with average indices 0.5751403:
center2_eucl[2,]

## CLUSTER 3: is grouping cities with average indices 0.2518065:
center2_eucl[3,]















### MANHATTAN DISTANCE


distance <- 'manhattan'

linkages <- c('single', 'average', 'complete', 'ward.D2')



# distance matrix
# it contain the distances between all the units according to a specific distance metric

data.dist <- dist(data, method=distance)


# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])


# dendograms (2 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=2)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=2)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=2)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=2)




# dendograms (3 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=3)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=3)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=3)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=3)




# select the best linkage and best k:

# In the single linkage we have the chain effect:
# every small cluster attacks to a bigger one, and no bigger cluster attacks to another big one
# this generate always small distances between levels and it is complex to discriminate a good number 
# of clusters.

# We choose as linkage the WARD.D2, because it discriminate the structure of the data in a 
# good way.

# To choose k, we look at those vertical lines whose length between a level and another level
# is the maximum in the dendogram
# That would be a right number of clusters to divide the data with
# also because, if we have some perturbation, we have a large enough distance to handle this
# In this case, we can pick k=3, and divide the data in two clusters.


k <- 3
linkage <- 'ward-D2' # average, complete, ward.D2
data.hc2_manh <- data.w #s, a, c, w
par(mfrow=c(1,1))
plot(data.hc2_manh, main=paste('Distance used: ', distance, ',  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc2_manh, k=3)




# cut dendogram:
# for each observation (that are unordered and permuted) we assign a cluster
clusters2_mahn <- cutree(data.hc2_manh, k=3)





#Centers of the Clusters
center2_mahn <- matrix(0,k,p)
for (i in 1:k) {
  center2_mahn[i,] <- mean(data[clusters2_mahn==i, ])
}
colnames(center2_mahn) <- colnames(data)




## CLUSTER 1: is grouping cities with average indices 0.4145287:
center2_mahn[1,]

## CLUSTER 2: is grouping cities with average indices 0.5751403:
center2_mahn[2,]

## CLUSTER 3: is grouping cities with average indices 0.2518065:
center2_mahn[3,]



















### CANBERRA DISTANCE


distance <- 'canberra'

linkages <- c('single', 'average', 'complete', 'ward.D2')



# distance matrix
# it contain the distances between all the units according to a specific distance metric

data.dist <- dist(data, method=distance)


# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])


# dendograms (2 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=2)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=2)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=2)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=2)




# dendograms (3 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=3)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=3)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=3)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=3)







# dendograms (4 clusters)
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.s, k=4)
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.a, k=4)
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.c, k=4)
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.w, k=4)






# select the best linkage and best k:

# In the single linkage we have the chain effect:
# every small cluster attacks to a bigger one, and no bigger cluster attacks to another big one
# this generate always small distances between levels and it is complex to discriminate a good number 
# of clusters.

# We choose as linkage the WARD.D2, because it discriminate the structure of the data in a 
# good way.

# To choose k, we look at those vertical lines whose length between a level and another level
# is the maximum in the dendogram
# That would be a right number of clusters to divide the data with
# also because, if we have some perturbation, we have a large enough distance to handle this
# In this case, we can pick k=2, and divide the data in two clusters.


k <- 2
linkage <- 'ward-D2' # average, complete, ward.D2
data.hc2_canb <- data.w #s, a, c, w
par(mfrow=c(1,1))
plot(data.hc2_canb, main=paste('Distance used: ', distance, ',  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc2_canb, k=2)




# cut dendogram:
# for each observation (that are unordered and permuted) we assign a cluster
clusters2_canb <- cutree(data.hc2_canb, k=2)




#Centers of the Clusters
center2_canb <- matrix(0,k,p)
for (i in 1:k) {
  center2_canb[i,] <- mean(data[clusters2_canb==i, ])
}
colnames(center2_canb) <- colnames(data)




## CLUSTER 1: is grouping cities with average indices 0.4720203:
center2_canb[1,]

## CLUSTER 2: is grouping cities with average indices 0.2518065:
center2_canb[2,]














#### COMPARISON ####


linkage <- 'ward-D2' # average, complete, ward.D2
par(mfrow=c(3,2))

plot(data.hc1_eucl, main=paste('Deepest Level. Distance used: Euclidean,  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc1_eucl, k=2)

plot(data.hc1_manh, main=paste('Deepest Level. Distance used: Manhattan,  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc1_manh, k=2)

plot(data.hc1_canb, main=paste('Deepest Level. Distance used: Canberra,  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc1_canb, k=2)

plot(data.hc2_eucl, main=paste('DPSVI. Distance used: Euclidean,  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc2_eucl, k=3)

plot(data.hc2_manh, main=paste('DPSVI.. Distance used: Manhattan,  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc2_manh, k=3)

plot(data.hc2_canb, main=paste('DPSVI.. Distance used: Canberra,  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc2_canb, k=2)


























#### MAP ACCORDING TO CLUSTERS ####





setwd("C:/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/Project/Code")

library(rworldmap)
library(sp)
library(readxl)
data_country<-read_excel("countries_excel.xlsx")
data_country2 <- data_country







### Map with respect to the Clustering using the Deepest variables, and the Euclidean distance



clusters_map <- clusters1_eucl
#clusters_map <- (clusters1_eucl/4) + cbind(rnorm(255, sd=0.00005))    
data_country <- cbind(data_country2, clusters_map)



sPDF<- joinCountryData2Map(data_country, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i")
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="clusters_map",
                      mapRegion='Europe',addLegend = TRUE, 
                      colourPalette = 'heat',
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Clustering using the Deepest variables, and the Euclidean distance',
                      catMethod='categorical') 


## CLUSTER 1: is grouping cities with average indices:
center1_eucl[1,]

## CLUSTER 2: is grouping cities with average indices:
center1_eucl[2,]














### Map with respect to the Clustering using the Deepest variables, and the Mahnattan distance



clusters_map <- clusters1_mahn
#clusters_map <- (clusters1_eucl/4) + cbind(rnorm(255, sd=0.00005))    
data_country <- cbind(data_country2, clusters_map)



sPDF<- joinCountryData2Map(data_country, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i")
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="clusters_map",
                      mapRegion='Europe',addLegend = TRUE, 
                      colourPalette = 'heat',
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Clustering using the Deepest variables, and the Mahnattan distance',
                      catMethod='categorical') 


## CLUSTER 1: is grouping cities with average indices:
center1_mahn[1,]

## CLUSTER 2: is grouping cities with average indices:
center1_mahn[2,]














### Map with respect to the Clustering using the Deepest variables, and the Canberra distance



clusters_map <- clusters1_canb
#clusters_map <- (clusters1_eucl/4) + cbind(rnorm(255, sd=0.00005))    
data_country <- cbind(data_country2, clusters_map)



sPDF<- joinCountryData2Map(data_country, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i")
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="clusters_map",
                      mapRegion='Europe',addLegend = TRUE, 
                      colourPalette = 'heat',
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Clustering using the Deepest variables, and the Canberra distance',
                      catMethod='categorical') 


## CLUSTER 1: is grouping cities with average indices:
center1_canb[1,]

## CLUSTER 2: is grouping cities with average indices:
center1_canb[2,]











### Map with respect to the Clustering using the DPSVI variable, and the euclidean distance



clusters_map <- clusters2_eucl
#clusters_map <- (clusters1_eucl/4) + cbind(rnorm(255, sd=0.00005))    
data_country <- cbind(data_country2, clusters_map)



sPDF<- joinCountryData2Map(data_country, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i")
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="clusters_map",
                      mapRegion='Europe',addLegend = TRUE, 
                      colourPalette = 'heat',
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Clustering using the DPSVI variable, and the euclidean distance',
                      catMethod='categorical') 



## CLUSTER 1: is grouping cities with average indices 0.4145287:
center2_eucl[1,]

## CLUSTER 2: is grouping cities with average indices 0.5751403:
center2_eucl[2,]

## CLUSTER 3: is grouping cities with average indices 0.2518065:
center2_eucl[3,]












### Map with respect to the Clustering using the DPSVI variable, and the Mahnattan distance



clusters_map <- clusters2_mahn
#clusters_map <- (clusters1_eucl/4) + cbind(rnorm(255, sd=0.00005))    
data_country <- cbind(data_country2, clusters_map)



sPDF<- joinCountryData2Map(data_country, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i")
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="clusters_map",
                      mapRegion='Europe',addLegend = TRUE, 
                      colourPalette = 'heat',
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Clustering using the DPSVI variable, and the Mahnattan distance',
                      catMethod='categorical') 



## CLUSTER 1: is grouping cities with average indices 0.4145287:
center2_mahn[1,]

## CLUSTER 2: is grouping cities with average indices 0.5751403:
center2_mahn[2,]

## CLUSTER 3: is grouping cities with average indices 0.2518065:
center2_mahn[3,]











### Map with respect to the Clustering using the DPSVI variable, and the Canberra distance



clusters_map <- clusters2_canb
#clusters_map <- (clusters1_eucl/4) + cbind(rnorm(255, sd=0.00005))    
data_country <- cbind(data_country2, clusters_map)



sPDF<- joinCountryData2Map(data_country, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i")
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="clusters_map",
                      mapRegion='Europe',addLegend = TRUE, 
                      colourPalette = 'heat',
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Clustering using the DPSVI variable, and the Canberra distance',
                      catMethod='categorical') 



## CLUSTER 1: is grouping cities with average indices 0.4720203:
center2_canb[1,]

## CLUSTER 2: is grouping cities with average indices 0.2518065:
center2_canb[2,]


#### Comparison: CLUSTERING ON DPSVI INDEX vs Political Orientation of the countries ####


# Based on the DPSVI, a clustering analysis has been made, dividing all the cities in 3 clusters.
# Based on the clusters centers, we can say that CLUSTER 1 is grouping cities with average digitization 0.4145287,
# CLUSTER 2 is grouping cities with average digitization 0.5751403, CLUSTER 3 is grouping cities with average digitization 0.2518065.

par(mar=c(8,8,8,8))
plot(data.hc2_eucl, cex.lab=1.8, cex.axis=1.3, cex.main=2.5, cex.sub=1.5, main=paste('DPSVI. Distance used: Euclidean,  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc2_eucl, k=3)




# Based on the clustering, the countries belong to 3 different groups:
# red are countries where the average digitization is around 0.25, so they are countries with poor digital awareness;
# yellow are countries where the average digitization is around 0.41, so they are countries with better digitizion
# than the yellow ones;
# orange are countries where the average digitization is around 0.57, so they are countries with a good degree of digital
# awareness.





setwd("C:/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/Project/Code")

library(rworldmap)
library(sp)
library(readxl)
data_country<-read_excel("countries_excel.xlsx")
data_country2 <- data_country




clusters_map <- clusters2_eucl
#clusters_map <- (clusters1_eucl/4) + cbind(rnorm(255, sd=0.00005))    
data_country <- cbind(data_country2, clusters_map)



sPDF<- joinCountryData2Map(data_country, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i", mar=c(8,8,8,8))
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="clusters_map",
                      mapRegion='Europe',addLegend = FALSE, 
                      colourPalette = 'heat',
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Clustering using the DPSVI variable',
                      catMethod='categorical') 






#data <- data[,-c(2,7,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53)]
#data <- data[-c(255,238,1),]

#data <- data[,35:36]


setwd('/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/My_Exam/mcshapiro.test')
load('mcshapiro.test.RData')

library(readxl)
setwd("C:/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/Project/Code")
data_political <- read.csv("political_data.csv", head=T, sep = ",")
data_country<-read_excel("countries_excel_2.xlsx")

data <- merge(x = data_country, y = data_political, by = "CCode", all.x = TRUE)




sPDF<- joinCountryData2Map(data, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i")
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="party_family",
                      mapRegion='Europe',
                      xlim=NA,
                      ylim=NA,
                      addLegend = TRUE, 
                      numCats=7,
                      colourPalette=rainbow(7),
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      catMethod="pretty",
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Political orientation in the countries') 



# We have the map with the political orientation for each country. We can see that the reusults in terms of digitalization
# are in line with the ANOVA

#### Comparison: CLUSTERING ON Political INDEX vs Political Orientation of the countries ####


# Based on 4 indices that may be related with the political situation, 
# a clustering analysis has been made, dividing all the cities in 3 clusters, according to them:
# Context empowerment, Replication and diffusion, Organizational readiness, Innovation strategy.



# Based on the clustering, the countries belong to 2 different groups:
# red are countries where Context empowerment, Replication and diffusion, Organizational readiness, Innovation strategy are 
# lower with respect to the other countries, so they are countries where there is not an important action
# coming from the authorities to help the digital era growing and spread in a good way;
# yellow are countries where Context empowerment, Replication and diffusion, Organizational readiness, Innovation strategy are 
# higher with respect to the other countries, so they are countries the action of the authorities is focused in the general improvement 
# and diffusion of the digitization in the country.

par(mar=c(8,8,8,8))
plot(data.hc3_eucl, cex.lab=1.8, cex.axis=1.3, cex.main=2.5, cex.sub=1.5, main=paste('Change management. Distance used: Euclidean,  Linkage used: ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc3_eucl, k=2)




setwd("C:/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/Project/Code")

library(rworldmap)
library(sp)
library(readxl)
data_country<-read_excel("countries_excel.xlsx")
data_country2 <- data_country




clusters_map <- clusters3_eucl
#clusters_map <- (clusters1_eucl/4) + cbind(rnorm(255, sd=0.00005))    
data_country <- cbind(data_country2, clusters_map)



sPDF<- joinCountryData2Map(data_country, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i", mar=c(8,8,8,8))
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="clusters_map",
                      mapRegion='Europe',addLegend = FALSE, 
                      colourPalette = 'heat',
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Clustering using the DPSVI variable',
                      catMethod='categorical') 






#data <- data[,-c(2,7,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53)]
#data <- data[-c(255,238,1),]

#data <- data[,35:36]


setwd('/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/My_Exam/mcshapiro.test')
load('mcshapiro.test.RData')

library(readxl)
setwd("C:/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/Project/Code")
data_political <- read.csv("political_data.csv", head=T, sep = ",")
data_country<-read_excel("countries_excel.xlsx")

data <- merge(x = data_country, y = data_political, by = "CCode", all.x = TRUE)




sPDF<- joinCountryData2Map(data, joinCode = "ISO2", nameJoinColumn = "CCode", nameCountryColumn = "country", suggestForFailedCodes = FALSE, mapResolution = "coarse", projection = NA, verbose = FALSE)
#CCode is the column name for the country codes in excel file.

par(mai=c(0.2,0,0.2,0),xaxs="i",yaxs="i", mar=c(8,8,8,8))
plot<-mapCountryData( sPDF,
                      nameColumnToPlot="party_family",
                      mapRegion='Europe',
                      xlim=NA,
                      ylim=NA,
                      addLegend = FALSE, 
                      numCats=7,
                      colourPalette=rainbow(7),
                      oceanCol = 'lightblue',
                      borderCol = 'black',
                      catMethod="pretty",
                      lwd = 1.5,
                      missingCountryCol = 'grey',
                      mapTitle = 'Political orientation in the countries') 




