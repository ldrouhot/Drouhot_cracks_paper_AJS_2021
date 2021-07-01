## Here we implement the preferred clustering solution withotu having to run file #1 in the iteration.

fuzzifier <- 1.5

my.fuzzy.cmeans <- myTeO %>% dplyr::select(-religiositysum,-ident) %>% cmeans (., centers=n_clusters, iter.max=100, 
                                                                         verbose=TRUE, dist="manhattan",
                                                                         method="cmeans", m=fuzzifier, 
                                                                         rate.par = NULL) # Creates fuzzy cmeans clusters

##### Validation

validation = list()

for (i in 2:6) {

	my.fuzzy.kmeans <- myTeO %>% dplyr::select(-religiositysum,-ident) %>% FKM (., k=i, m=fuzzifier, maxit=100)
	validation[[i]] <- myTeO %>% dplyr::select(-religiositysum,-ident) %>% Fclust.index (my.fuzzy.kmeans, ., "all")

}


index.PC <- sapply(validation[3:6],`[`,1)
index.PE <- sapply(validation[3:6],`[`,2)
index.MPC <- sapply(validation[3:6],`[`,3)
index.SIL <- sapply(validation[3:6],`[`,4)
index.SIL.F <- sapply(validation[3:6],`[`,5)
index.XB <- sapply(validation[3:6],`[`,6)

myTeO$cluster_cmeans <- my.fuzzy.cmeans$cluster # adds cluster membership to TeO

# Probs of belonging to each cluster for each individual

probs.cluster <- my.fuzzy.cmeans$membership

# Above, is the probability for each individual to belong to each cluster (see table). More technically, these are the parameters
# in a multinomial distribution. In a logit, the 1/0 outcome is a realization of a random variable. 

ncols <- ncol(myTeO)


myTeO <- cbind(myTeO,Hmisc::rMultinom(probs.cluster,draws))

setnames(myTeO, old = seq(ncols+1,ncols+draws,1), new = paste0("A",seq(1:draws)))

myTeO.native <- myTeO %>% mutate(native=1)

if (FALSE){

# Maximize: PC, MPC, SIL, SIL.F, 
# Minimize: PE, XB

par(mfrow=c(3,2))

xticks <- seq(3,6)
plot(index.PC, xlab="Number of Clusters", ylab="Partition Coefficient Index", axes = FALSE, frame.plot=TRUE)
lines(index.PC)
axis(2)
axis(1, at=1:4, labels=xticks)

dev.print(pdf, 'cl_valid_partition_coefficient.pdf')

plot(index.PE, xlab="Number of Clusters", ylab="Partition Entropy Index", axes = FALSE, frame.plot=TRUE)
lines(index.PE)
axis(2)
axis(1, at=1:4, labels=xticks)

dev.print(pdf, 'cl_valid_partition_entropy.pdf')

plot(index.MPC, xlab="Number of Clusters", ylab="Modified Partitition Coefficient Index", axes = FALSE, frame.plot=TRUE)
lines(index.MPC)
axis(2)
axis(1, at=1:4, labels=xticks)

dev.print(pdf, 'cl_valid_modified_partition.pdf')

plot(index.SIL, xlab="Number of Clusters", ylab="Silhouette Index", axes = FALSE, frame.plot=TRUE)
lines(index.SIL)
axis(2)
axis(1, at=1:4, labels=xticks)

dev.print(pdf, 'cl_valid_silhouette.pdf')

plot(index.SIL.F, xlab="Number of Clusters", ylab="Fuzzy Silhouette Index", axes = FALSE, frame.plot=TRUE)
lines(index.SIL.F)
axis(2)
axis(1, at=1:4, labels=xticks)

dev.print(pdf, 'cl_valid_fuzzy_silhouette.pdf')

plot(index.XB, xlab="Number of Clusters", ylab="Xie-Beni Index", axes = FALSE, frame.plot=TRUE)
lines(index.XB)
axis(2)
axis(1, at=1:4, labels=xticks)

dev.print(pdf, 'cl_valid_xie_beni.pdf')

}

#####

# Plot randomly drawn pairs across iterations:

plot(TeO.full$A53,TeO.full$A55) # this is useful to disualize stabiltiy across clustering assignment. 


# "draws" is the number of times a cluster assignment is done based on the membership function. Logically, it will be rather consistent
#  across draws when membership is clearly defined (in the .9+ probability range), far less stable in other situations. In turn, this is predicated on
#  the fuzzifier m. 


# This helps avoid the shortcomings associated with a closest hard clustering assignment, which results in the same problems of determinisitc
# assignment associated with k-means. For a truly probabilistic assignment, we cannot simply take the cluster with the highest membership
# coefficient the c-means algorithm comes up with (i.e non-random assignment that does not account for uncertainty, e.g. a membership coefficeint of 0.55 will result
# in the same assignment likelihood as a .9 membership coefficients). Using membership coefficients does not account for any fuziness.
# To do so, we employ a multinomial draw. The "RMultinom" function is a random draw based on the probabilities given by the membership coefficient.
# The "1" indicate there is going to be only one draw. 



