#Online PCA

pca_dat = read.csv("Data/OPCA.csv",header =FALSE)
v = as.matrix(read.csv("True_eigvector.csv",header =FALSE))

#part1
n_ta = 0.01
w_i_1 = matrix(1/sqrt(20),nrow=20,ncol=1)
dist = rep(0,1000)

for (i in seq(1:1000)){
  A = as.matrix(pca_dat[(20*(i-1)+1):(20*(i)),])
  w_i = w_i_1 + n_ta * (A%*%w_i_1)
  w_i = w_i/norm(w_i,"2")
  dist[i] = 1- (t(w_i)%*%(v))^2
  w_i_1 = w_i
}

plot(dist, type = "l", main = "Plot of dist (W_i and V) against number of iterations", 
     xlab = "Number of iterations", ylab = "Distance between W and V")


#part2
w_i_1 = matrix(1/sqrt(20),nrow=20,ncol=1)
dist2 = rep(0,1000)

for (i in seq(1:1000)){
  n_ta = 1/(100+i)
  A = as.matrix(pca_dat[(20*(i-1)+1):(20*(i)),])
  w_i = w_i_1 + n_ta * (A%*%w_i_1)
  w_i = w_i/norm(w_i,"2")
  dist2[i] = 1- (t(w_i)%*%(v))^2
  w_i_1 = w_i
}
plot(dist2, type = "l", main = "Plot of dist (W_i and V) against number of iterations", 
     xlab = "Number of iterations", ylab = "Distance between W and V")
