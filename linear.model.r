library(rcppbugs)


##NR <- 20
NR <- 1e2L
NC <- 2L
y <- matrix(rnorm(NR,1) + 10,nr=NR,nc=1L)
X <- matrix(nr=NR,nc=NC)
X[,1] <- 1
X[,2] <- y + rnorm(NR)/2 - 10


lm.res <- lm.fit(X,y)
print(coef(lm.res))

## RCppBugs Model

b <- mcmc.normal(rnorm(NC),mu=0,tau=0.0001)
##tau.y <- uniform(sd(as.vector(y)),lower=0,upper=100)
tau.y <- mcmc.gamma(sd(as.vector(y)),alpha=0.1,beta=0.1)
##y.hat <- deterministic(function(X,b) { X %*% b }, X, b)
y.hat <- linear(X,b)
y.lik <- mcmc.normal(y,mu=y.hat,tau=tau.y,observed=TRUE)
m <- create.model(b, tau.y, y.hat, y.lik)

cat("running model...\n")
runtime <- system.time(ans <- run.model(m, iterations=1e5L, burn=1e4L, adapt=1e3L, thin=10L))
print(apply(ans[["b"]],2,mean))
cat("acceptance.ratio:",get.ar(ans),"\n")

print(runtime)
##cat("ar:",round(ans,2),"\n")
##print(b)

##plot(ans[[1]][,1],type="l")
