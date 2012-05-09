library(rcppbugs)
library(lme4)
data(cbpp)

X <- matrix(0,nrow=nrow(cbpp),ncol=4)
X[1:nrow(X) + (as.integer(cbpp[,"period"]) -1) * nrow(cbpp)] <- 1
X[,1] <- 1.0

herd <- as.integer(cbpp[,"herd"])
NR <- nrow(cbpp)
J <- length(unique(herd))

tau.overdisp <- mcmc.uniform(runif(1),0,100)
overdisp <- mcmc.normal(rnorm(NR),0, tau.overdisp)
b <- mcmc.normal(rnorm(ncol(X)),mu=0,tau=0.0001)
tau.b.herd <- mcmc.uniform(runif(1),0,100)
b.herd <- mcmc.normal(rnorm(J),mu=0,tau=tau.b.herd)
phi <- deterministic(function(X,herd,b,b.herd,overdisp) { 1/(1 + exp(-(X %*% b + b.herd[herd] + overdisp))) }, X, herd, b, b.herd, overdisp)
incidence <- mcmc.binomial(cbpp[,"incidence"],n=cbpp[,"size"],p=phi,observed=TRUE)
m <- create.model(tau.overdisp,overdisp,b,tau.b.herd,b.herd,phi,incidence)

iterations <- 1e5L
burn <- 1e5L
adapt <- 1e3L
thin <- 10L

cat("running rcppbugs model...\n")
rcppbugs.time <- system.time(ans <- run.model(m, iterations=iterations, burn=burn, adapt=adapt, thin=thin))
cat("runtime:\n")
print(rcppbugs.time)

cat("ar:",get.ar(ans),"\n")

cat("b:\n")
print(apply(ans[["b"]],2,mean))

cat("b herd:\n")
print(as.matrix(apply(ans[["b.herd"]],2,mean)))

##cat("overdisp:")
##print(as.matrix(apply(ans[["overdisp"]],2,mean)))
