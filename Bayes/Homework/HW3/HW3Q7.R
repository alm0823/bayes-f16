#### Posterior Sampling with Normal Model
set.seed(10072014)

# import data
y.graf <- c(259.8,248.1,235.8,252.6,264.1,267.3,231.5,237.9,242,242.2,228.5,245.2,269.6,243.5,234.5)
num.obs.graf <- length(y.graf)
y.cherry <- c(240.3,246.3,249.8,236.1,252.8,265.7,250.9,225.7,276.2,262.3,262.2,253.5,254.1,260.3,256.6)
num.obs.cherry <- length(y.cherry)
num.obs <- 15
# specify terms for priors
nu.0 <- 1
sigma.sq.0 <- 500
mu.0 <- 250
kappa.0 <- 1

# compute terms in posterior
kappa.n <- kappa.0 + num.obs
nu.n <- nu.0 + num.obs
s.sq.graf<- var(y.graf) #sum((y - mean(y))^2) / (num.obs - 1)
sigma.sq.n.graf <- (1 / nu.n) * (nu.0 * sigma.sq.0 + (num.obs - 1) * s.sq.graf + (kappa.0*num.obs)/kappa.n * (mean(y.graf) - mu.0)^2)
mu.n.graf <- (kappa.0 * mu.0 + num.obs * mean(y.graf)) / kappa.n
s.sq.cherry<- var(y.cherry) #sum((y - mean(y))^2) / (num.obs - 1)
sigma.sq.n.cherry <- (1 / nu.n) * (nu.0 * sigma.sq.0 + (num.obs - 1) * s.sq.cherry + (kappa.0*num.obs)/kappa.n * (mean(y.cherry) - mu.0)^2)
mu.n.cherry <- (kappa.0 * mu.0 + num.obs * mean(y.cherry)) / kappa.n

# simulate from posterior
#install.packages("LearnBayes")
library(LearnBayes) # for rigamma
num.sims <- 10000
sigma.sq.sims.graf <- theta.sims.graf <- rep(0,num.sims)
sigma.sq.sims.cherry <- theta.sims.cherry <- rep(0,num.sims)
for (i in 1:num.sims){
  sigma.sq.sims.graf[i] <- rigamma(1,nu.n/2,sigma.sq.n.graf*nu.n/2)
  theta.sims.graf[i] <- rnorm(1, mu.n.graf, sqrt(sigma.sq.sims.graf[i]/kappa.n))

  sigma.sq.sims.cherry[i] <- rigamma(1,nu.n/2,sigma.sq.n.cherry*nu.n/2)
  theta.sims.cherry[i] <- rnorm(1, mu.n.cherry, sqrt(sigma.sq.sims.cherry[i]/kappa.n))
}

library(grDevices) # for rgb
plot(sigma.sq.sims.graf,theta.sims.graf,pch=16,col=rgb(.1,.1,.8,.05),ylab=expression(theta),
     xlab=expression(sigma[2]),main='Joint Posterior')
hist(sigma.sq.sims.graf,prob=T,main=expression('Marginal Posterior of' ~ sigma[2]),
     xlab=expression(sigma[2]))
hist(theta.sims.graf,prob=T,main=expression('Marginal Posterior of' ~ theta),xlab=expression(theta))

mean(theta.sims.graf > 250)

library(grDevices) # for rgb
plot(sigma.sq.sims.cherry,theta.sims.cherry,pch=16,col=rgb(.1,.1,.8,.05),ylab=expression(theta),
     xlab=expression(sigma[2]),main='Joint Posterior')
hist(sigma.sq.sims.cherry,prob=T,main=expression('Marginal Posterior of' ~ sigma[2]),
     xlab=expression(sigma[2]))
hist(theta.sims.cherry,prob=T,main=expression('Marginal Posterior of' ~ theta),xlab=expression(theta))

mean(theta.sims.cherry > 250)

mean(theta.sims.graf > theta.sims.cherry)
mean(y.graf)
mean(y.cherry)
