cities10k= read.csv("cities10k.csv", header = TRUE, sep=",")

population <- sum(cities10k$Population)

loc <- cities10k[,6:7]
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1)

set.seed(1234)
clust.k02 <- kmeans(loc, 2)
plot(loc$Longitude,loc$Latitude, pch = 20, cex = .1, col = clust.k02$cluster)

set.seed(1234)
clust.k05 <- kmeans(loc, 5)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = clust.k05$cluster)

set.seed(1234)
clust.k10 <- kmeans(loc, 10)
plot(loc$Longitude,loc$Latitude, pch = 20, cex = .1, col = clust.k10$cluster)

set.seed(1234)
clust.k25.1 <- kmeans(loc, 25)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = clust.k25.1$cluster)

set.seed(2345)
clust.k25.2 <- kmeans(loc, 25)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = clust.k25.2$cluster)

set.seed(3456)
clust.k25.3 <- kmeans(loc, 25)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = clust.k25.3$cluster)

install.packages("clue")
library(clue)
help(cl_boot)

set.seed(1234)
ensemble <- cl_boot(loc, 50, 25)
ensemble

cl_agreement(ensemble)

consensus.se <- cl_consensus(ensemble, method = "SE", control = list(k = 25))
help(cl_consensus)

consensus.se

consensus.he <- cl_consensus(ensemble, method = "HE", control = list(k = 25))

consensus.he

assignments <- cl_class_ids(consensus.he)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = assignments)
