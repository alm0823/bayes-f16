set.seed(10222016)
library(mnormt) # rmnorm, dmnorm
library(MCMCpack) #riwish
################################
###simulate multivariate normal data
################################
n=100
p=3
true.mu <- rep(0,p)

# create correlation matrix
rho.12 <- .7
rho.23 <- .3
rho.13 <- 0
R <- matrix(c(1,rho.12,rho.13,rho.12,1,
                  rho.23,rho.13,rho.23,1),p,p)

y.vec <- rmnorm(n,true.mu,R)
colMeans(y.vec)
cor(y.vec)

################################
# Run Gibbs Sampler
################################
###initialize 
num.sims <- 10000
mu <- matrix(0,nrow=num.sims,ncol=p)
Sigma <- array(0,dim=c(p,p,num.sims)) 
Sigma[,,1] <- diag(p)
# note when p is large it makes sense to only save unique
# elements in Sigma

###priors
# theta
mu.0 <- rep(0,p)
Lambda.0 <- matrix(.3,p,p)
diag(Lambda.0) <- 1
Lambda.0 <- Lambda.0 * 10000
Lambda.0.inv <- solve(Lambda.0)
# visualize a draws from prior
rmnorm(5,mu.0,Lambda.0)

# Sigma
nu.0 <- p 
nu.n <- nu.0 + n
S.0 <- matrix(.2,p,p)
diag(S.0) <- 1
# visualize a few draws from prior
riwish(p-1,solve(S.0)) # note nu.0 must be atleast p
riwish(p,solve(S.0)) 
riwish(nu.0,solve(S.0))

for (i in 2:num.sims){
  #sample mu
  Sig.inv <- solve(Sigma[,,(i-1)])
  Lambda.n <- solve(n* Sig.inv + Lambda.0.inv )
  mu.n <- Lambda.n %*% (Sig.inv %*% colSums(y.vec) + Lambda.0.inv %*% mu.0)
  mu[i,] <- rmnorm(1,mu.n,Lambda.n)
  
  # Sample Sigma
  mu.matrix <- matrix(mu[i,],n,p,byrow=T)
  S.theta <- t(y.vec - mu.matrix) %*% (y.vec - mu.matrix)
  Sigma[,,i] <- riwish(nu.n,(S.theta + S.0)) # note the parameterization
}

# look at theta
colMeans(mu)
colMeans(y.vec)
par(mfcol=c(1,3))
for (i in 1:p){
  plot(mu[,i],type='l',ylab=expression(mu[i]))
  abline(h=true.mu[i],col='red',lwd=2)
}

# look at Sigma
var(y.vec)
matrix(c(mean(Sigma[1,1,]),mean(Sigma[1,2,]),mean(Sigma[1,3,]),
            mean(Sigma[2,1,]),mean(Sigma[2,2,]),mean(Sigma[2,3,]),
            mean(Sigma[3,1,]),mean(Sigma[3,2,]),mean(Sigma[3,3,])),p,p)
par(mfcol=c(3,3))
for(i in 1:p){
  for  (j in 1:p){
    plot(Sigma[i,j,],type='l',ylab=expression(sigma[i,j]))
    abline(h=R[i,j],col='firebrick4',lwd=3)
  }
}
