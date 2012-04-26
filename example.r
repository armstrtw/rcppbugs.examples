library(rcppbugs)
library(rjags)

##NR <- 20
NR <- 1e2L
NC <- 2L
##y <- matrix(rnorm(NR,1) + 10,nr=NR,nc=1L)
y <- rnorm(NR,1) + 10
X <- matrix(nr=NR,nc=NC)
X[,1] <- 1
X[,2] <- y + rnorm(NR)/2 - 10
##x <- y + rnorm(NR)/2 - 10

lm.res <- lm.fit(X,y)
print(coef(lm.res))

b <- mcmc.normal(rnorm(NC),mu=0,tau=0.0001)
tau.y <- mcmc.uniform(sd(as.vector(y)),lower=0,upper=100)
##tau.y <- mcmc.gamma(sd(as.vector(y)),alpha=0.1,beta=0.1)
##y.hat <- deterministic(function(X,b) { X %*% b }, X, b)
y.hat <- linear(X,b)
y.lik <- mcmc.normal(y,mu=y.hat,tau=tau.y,observed=TRUE)
m <- create.model(b, tau.y, y.hat, y.lik)


iterations <- 1e5L
burn <- iterations
adapt <- 1e3L
thin <- 10L

cat("running rcppbugs model...\n")
rcppbugs.time <- system.time(ans <- run.model(m, iterations=iterations, burn=burn, adapt=adapt, thin=thin))
print(apply(ans[["b"]],2,mean))
print(rcppbugs.time)


cat("running jags model...\n")

jags.time1 <- system.time(
             jags <- jags.model('example2.bug',
                                data = list('X' = X,
                                'y' = y,
                                'NR' = NR,
                                'NC' = NC),
                                n.chains = 1,
                                n.adapt = adapt
                                ))
jags.time2 <- system.time(update(jags, n.iter=burn))
jags.time3 <- system.time(jags.trace <- jags.samples(jags,c('b','tau.y'),n.iter=iterations,thin=thin))

jags.time <- jags.time1 + jags.time2 + jags.time3
print(jags.time)
length(as.vector(jags.trace$a))
print(jags.trace)

print(jags.time/rcppbugs.time)
