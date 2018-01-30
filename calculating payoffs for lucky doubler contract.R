require(plot3D)

#  plot surface
N=300
payoffs = matrix(0,N,N)
for (k in 1:N){
  for (i in 1:k){
    sum=0
    for (j in i:k){
      sum=sum+1/(j+1)
    }
    sum = sum*1.25
    payoffs[i,k]=sum
  }
}
heatmap(payoffs)
payoffs
dim(payoffs)
payoffs[which(payoffs<1)]=0
persp3D(z = payoffs, x=seq(1,N),y=seq(1,N),theta = 30,phi=30, ticktype = "detailed",main = "expected payoffs", xlab = "your position", ylab = "final position")

