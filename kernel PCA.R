shapes= read.csv("shapes.csv", header = TRUE, sep=",")

library(RColorBrewer)
display.brewer.all()
colors <- brewer.pal(10, "Spectral")

colors

plot(shapes$cornerX, shapes$cornerY, col=colors[shapes$cornerC])

pc.corner <- princomp(shapes[,1:2], cor=TRUE, scores=TRUE)
plot(pc.corner$scores, col=colors[shapes$cornerC])

library(kernlab)
kpc.corner <- kpca(~., data=shapes[,1:2], kernel="rbfdot", kpar=list(sigma=0.2), features=5)

kpc.corner
kpc.corner@eig

kpc.corner@pcv

plot(kpc.corner@pcv[,c(1,2)], col=colors[shapes$cornerC])

plot(kpc.corner@pcv[,c(1,3)], col=colors[shapes$cornerC])

plot(shapes$cincX, shapes$cincY, col=colors[shapes$cincC])
pc.cinc <- princomp(shapes[,1:2], cor=TRUE, scores=TRUE)
plot(pc.cinc$scores, col=colors[shapes$cincC])
kpc.cinc <- kpca(~., data=shapes[,1:2], kernel="rbfdot", kpar=list(sigma=0.1), features=5)
plot(kpc.cinc@pcv[,c(1,3)], col=colors[shapes$cincC])
plot(kpc.cinc@pcv[,c(1,4)], col=colors[shapes$cincC])

plot(shapes$moonsX, shapes$moonsY, col=colors[shapes$moonsC])
pc.moons <- princomp(shapes[,], cor=TRUE, scores=TRUE)
plot(pc.moons$scores, col=colors[shapes$moonsC])
kpc.cinc <- kpca(~., data=shapes[,1:2], kernel="rbfdot", kpar=list(sigma=0.1), features=5)
plot(kpc.cinc@pcv[,c(1,3)], col=colors[shapes$cincC])
plot(kpc.cinc@pcv[,c(1,4)], col=colors[shapes$cincC])
