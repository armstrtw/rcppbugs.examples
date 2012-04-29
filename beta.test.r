library(rcppbugs)


NR <- 1e3L
y <- runif(NR,0,1)
y <- as.double(y > 0.10)

p <- mcmc.beta(runif(1),alpha=2,beta=2)
##y.lik <- mcmc.binomial(y,p=p,observed=TRUE)
y.lik <- mcmc.bernoulli(y,p=p,observed=TRUE)
m <- create.model(p, y.lik)

iterations <- 1e5L
burn <- iterations
adapt <- 1e3L
thin <- 2L

cat("running rcppbugs model...\n")
rcppbugs.time <- system.time(ans <- run.model(m, iterations=iterations, burn=burn, adapt=adapt, thin=thin))
print(mean(ans[["p"]]))
##print(rcppbugs.coefs)
print(rcppbugs.time)
