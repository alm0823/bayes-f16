##### Quiz 3

##Q1
num.sims <- 1000
mu <- c(-3,0,3)
# Simulate Delta
delta <- sample(3,num.sims,prob=c(.45,.1,.45),replace=T)

# Simulate theta conditional on delta
theta <- rnorm(num.sims,mu[delta],sqrt(1/3))

hist(theta,breaks=seq(-6,6,by=.2),probability = T,xlab=expression(theta))
x.seq <- seq(-6,6,by=.01)
dens <- .45 * dnorm(x.seq,-3,sqrt(1/3)) +  .1 * dnorm(x.seq,0,sqrt(1/3)) + .45 * dnorm(x.seq,3,sqrt(1/3))
lines(x.seq,dens,lwd=2,col='red')

##Q2
# a
num.sims <- 100
theta <- rep(0,num.sims)
delta <- rep(0,num.sims)

theta[1] <- 0
delta[1] <- 2
mu <- c(-3,0,3)
for (i in 2:num.sims){
  # simulate delta given theta
  p.1 <- .45 * dnorm(theta[i-1],mu[1],sqrt(1/3))
  p.2 <- .1 * dnorm(theta[i-1],mu[2],sqrt(1/3)) 
  p.3 <- .45 * dnorm(theta[i-1],mu[3],sqrt(1/3)) 
  prob.1 <- p.1 / (p.1+p.2+p.3)
  prob.2 <- p.2 / (p.1+p.2+p.3)
  prob.3 <- p.3 / (p.1+p.2+p.3)
  delta[i] <- sample(3,1,prob=c(prob.1,prob.2,prob.3))
                     
  # simulate theta given delta
  theta[i] <- rnorm(1,mu[delta[i]],sqrt(1/3))
}

hist(theta,breaks=seq(-6,6,by=.2),probability = T,xlim=c(-6,6))
x.seq <- seq(-6,6,by=.01)
dens <- .45 * dnorm(x.seq,-3,sqrt(1/3)) +  .1 * dnorm(x.seq,0,sqrt(1/3)) + .45 * dnorm(x.seq,3,sqrt(1/3))
lines(x.seq,dens,lwd=2,col='red')

plot(theta,type='l')
summary(as.factor(delta)) / num.sims

# b
num.sims <- 1000
theta <- rep(0,num.sims)
delta <- rep(0,num.sims)

theta[1] <- 0
delta[1] <- 2
mu <- c(-3,0,3)
for (i in 2:num.sims){
  # simulate delta given theta
  p.1 <- .45 * dnorm(theta[i-1],mu[1],sqrt(1/3))
  p.2 <- .1 * dnorm(theta[i-1],mu[2],sqrt(1/3)) 
  p.3 <- .45 * dnorm(theta[i-1],mu[3],sqrt(1/3)) 
  prob.1 <- p.1 / (p.1+p.2+p.3)
  prob.2 <- p.2 / (p.1+p.2+p.3)
  prob.3 <- p.3 / (p.1+p.2+p.3)
  delta[i] <- sample(3,1,prob=c(prob.1,prob.2,prob.3))
  
  # simulate theta given delta
  theta[i] <- rnorm(1,mu[delta[i]],sqrt(1/3))
}

hist(theta,breaks=seq(-6,6,by=.2),probability = T,xlim=c(-6,6))
x.seq <- seq(-6,6,by=.01)
dens <- .45 * dnorm(x.seq,-3,sqrt(1/3)) +  .1 * dnorm(x.seq,0,sqrt(1/3)) + .45 * dnorm(x.seq,3,sqrt(1/3))
lines(x.seq,dens,lwd=2,col='red')

plot(theta,type='l')
summary(as.factor(delta)) / num.sims

##c
num.sims <- 10000
theta <- rep(0,num.sims)
delta <- rep(0,num.sims)

theta[1] <- 100
delta[1] <- 2
mu <- c(-3,0,3)
for (i in 2:num.sims){
  # simulate delta given theta
  p.1 <- .45 * dnorm(theta[i-1],mu[1],sqrt(1/3))
  p.2 <- .1 * dnorm(theta[i-1],mu[2],sqrt(1/3)) 
  p.3 <- .45 * dnorm(theta[i-1],mu[3],sqrt(1/3)) 
  prob.1 <- p.1 / (p.1+p.2+p.3)
  prob.2 <- p.2 / (p.1+p.2+p.3)
  prob.3 <- p.3 / (p.1+p.2+p.3)
  delta[i] <- sample(3,1,prob=c(prob.1,prob.2,prob.3))
  
  # simulate theta given delta
  theta[i] <- rnorm(1,mu[delta[i]],sqrt(1/3))
}

hist(theta,breaks=seq(-6,6,by=.2),probability = T,xlim=c(-6,6))
x.seq <- seq(-6,6,by=.01)
dens <- .45 * dnorm(x.seq,-3,sqrt(1/3)) +  .1 * dnorm(x.seq,0,sqrt(1/3)) + .45 * dnorm(x.seq,3,sqrt(1/3))
lines(x.seq,dens,lwd=2,col='red')

plot(theta,type='l')
summary(as.factor(delta)) / num.sims

#$q3
#a
num.sims <- 100000
theta <- rep(0,num.sims)
delta <- rep(0,num.sims)

theta[1] <- 100
delta[1] <- 2
mu <- c(-3,0,3)
#install.packages('smcUtils')
library(smcUtils) # for renormalize
for (i in 2:num.sims){
  # simulate delta given theta
  log.p.1 <- log(.45) + dnorm(theta[i-1],mu[1],sqrt(1/3),log=T)
  log.p.2 <- log(.1) + dnorm(theta[i-1],mu[2],sqrt(1/3),log=T) 
  log.p.3 <- log(.45) + dnorm(theta[i-1],mu[3],sqrt(1/3),log=T) 
  probs <- renormalize(c(log.p.1,log.p.2,log.p.3),log=TRUE)
  delta[i] <- sample(3,1,prob=probs)
  
  # simulate theta given delta
  theta[i] <- rnorm(1,mu[delta[i]],sqrt(1/3))
}

hist(theta,breaks=seq(-100,100,by=.2),probability = T,xlim=c(-100,100))
x.seq <- seq(-100,100,by=.01)
dens <- .45 * dnorm(x.seq,-3,sqrt(1/3)) +  .1 * dnorm(x.seq,0,sqrt(1/3)) + .45 * dnorm(x.seq,3,sqrt(1/3))
lines(x.seq,dens,lwd=2,col='red')

plot(theta,type='l')
summary(as.factor(delta)) / num.sims
