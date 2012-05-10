library(KLS)
library(rcppbugs)


spx <- get.bbg("SPXT Index")
spx <- spx[dates(spx) > as.POSIXct("2001-01-01"),]
dts <- dates(spx)[-1]
spx.ret <- as.vector((spx/lag(spx,1) - 1) * 100)

p <- mcmc.uniform(0.98,lower=0.95,upper=0.9999)
##q <- mcmc.uniform(0.80,lower=0.70,upper=p)
q <- mcmc.uniform(0.90,lower=0.85,upper=p)
state <- as.double(runif(length(spx.ret)) > .95)
state.p <- deterministic(function(state,p,q) { c((1-p),ifelse(state[-length(state)],q,1-p)) },state,p,q)
state <- mcmc.bernoulli(state,p=state.p)
a <- mcmc.normal(rnorm(2),mu=c(1,-1),tau=100)
y.hat <- deterministic(function(a, state) { a[state + 1] }, a, state)
tau.y <- mcmc.gamma(runif(1),0.01,0.01)
y.lik <- mcmc.normal(spx.ret,mu=y.hat,tau=tau.y,observed=TRUE)

m <- create.model(p, q, state, state.p, a,  y.hat, tau.y, y.lik)
runtime <- system.time(ans <- run.model(m, iterations=1e5L, burn=1e6L, adapt=3e3L, thin=10L))
cat("acceptance.ratio:",get.ar(ans),"\n")

cat("p:\n")
print(mean(ans[["p"]]))
cat("q:\n")
print(mean(ans[["q"]]))
cat("a:\n")
print(apply(ans[["a"]],2,mean))
colnames(ans[["a"]]) <- c("a s=0","a s=1")
boxplot(ans[["a"]])
state.mean <- fts(apply(ans[["state"]],2,mean),dates=dts)
state.p.mean <- fts(apply(ans[["state.p"]],2,mean),dates=dts)
plot(state.mean)
plot(state.p.mean)



## hist(ans[["a"]][,1],breaks="FD")
## par(new=TRUE)
## hist(ans[["a"]][,2],breaks="FD",col="lightgrey")
