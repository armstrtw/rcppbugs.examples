library(rcppbugs)


## construct simulated data set
NR <- 1e2L
NC <- 5L

## actual alpha's and beta's
alpha <- rnorm(NC)
beta <- abs(rnorm(NC))

## simulate 1:NC individual stocks
## using alpha's a beta's above
spx <- rnorm(NR)
stocks <- list()
for(i in 1:NC) {
    stocks[[i]] <- alpha[i] + beta[i] * spx + rnorm(NR) / 2
}
stocks <- do.call(cbind,stocks)

J <- NC
group <- as.integer(rep(1:NC,each=NR))

y <- as.vector(stocks)

## repeating the spx returns for each stock
X <- rep(spx,NC)

## hyperparms for a -- alpha
mu.a <- mcmc.normal(rnorm(1),0,0.0001)
tau.a <- mcmc.gamma(runif(1),0.01,0.01)
a <- mcmc.normal(rnorm(J),mu.a,tau.a)

## hyperparams for b -- beta
mu.b <- mcmc.normal(rnorm(1),0,0.0001)
tau.b <- mcmc.gamma(runif(1),0.01,0.01)
b <- mcmc.normal(rnorm(J),mu.b,tau.b)

tau.y <- mcmc.gamma(runif(1),0.01,0.01)
y.hat <- deterministic(function(a, b, group, X) { a[group] +  b[group] * X}, a, b, group, X)
y.lik <- mcmc.normal(y,mu=y.hat,tau=tau.y,observed=TRUE)

m <- create.model(mu.a, tau.a, a, b, tau.y, y.hat, y.lik)

cat("running model...\n")
runtime <- system.time(ans <- run.model(m, iterations=1e5L, burn=1e5L, adapt=1e3L, thin=10L))

cat("alpha:\n")
print(data.frame(actual.alpha=alpha,est.alpha=apply(ans[["a"]],2,mean)))

cat("beta:\n")
print(data.frame(actual.beta=beta,est.beta=apply(ans[["b"]],2,mean)))

print(runtime)
