item =c("A","B","C","D") 
x1 = c(5,-1,1,-3)
x2 = c(3,1,-2,-2)
data = cbind(item,x1,x2)
data

#CLUSTERS

set.seed(2)
X=matrix(data, ncol = 3)
View(X)
#x[1:4,1]=x[1:4,1]
X[1:4,1]=X[1:4,1]
X[1:4,2]=X[1:4,2]
km.out=kmeans(X,2,nstart=4)   # agrupamiento de k-medias con k=2
km.out
x11()
par(mfrow=c(1,2))
plot(X, col=(km.out$cluster+2), nmain="Resultados de agrupamiento de k medias con k=2", xlab="", ylab = "", pch=4, cex=2)
