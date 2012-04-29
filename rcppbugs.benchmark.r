library(rcppbugs,quietly=TRUE,verbose=FALSE)
library(rjags,quietly=TRUE,verbose=FALSE)
library(MCMCpack,quietly=TRUE,verbose=FALSE)

##NR <- 20
NR <- 1e2L
NC <- 2L

y <- rnorm(NR,1) + 10
X <- matrix(nr=NR,nc=NC)
X[,1] <- 1
X[,2] <- y + rnorm(NR)/2 - 10


lm.time <- system.time(lm.res <- lm.fit(X,y))
##print(coef(lm.res))

b <- mcmc.normal(rnorm(NC),mu=0,tau=0.0001)
tau.y <- mcmc.uniform(sd(as.vector(y)),lower=0,upper=100)
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
rcppbugs.coefs <- apply(ans[["b"]],2,mean)
##print(rcppbugs.coefs)
print(rcppbugs.time)

cat("running MCMCpack model...\n")
mcmcpack.time <- system.time(mcmcpack.out <- MCMCregress(y ~ X.2,data=data.frame(y=y,X=X),burnin = burn, mcmc = iterations, thin=thin))
print(mcmcpack.time)

cat("running jags model...\n")

bug.model <- '
model {
	for (i in 1:NR){
		y[i] ~ dnorm(y.hat[i], tau.y)
		y.hat[i] <- inprod(b, X[i,])
	}
        for (j in 1:NC){
        	b[j] ~ dnorm(0, .0001)
        }
	tau.y ~ dunif(0, 100)
}
'
bug.file <- tempfile(fileext=".bug")
cat(bug.model,file=bug.file)

jags.time1 <- system.time(
             jags <- jags.model(bug.file,
                                data = list('X' = X,
                                'y' = y,
                                'NR' = NR,
                                'NC' = NC),
                                n.chains = 1,
                                n.adapt = adapt,
                                quiet=TRUE
                                ))
jags.time2 <- system.time(update(jags, n.iter=burn, progress.bar="none"))
jags.time3 <- system.time(jags.trace <- jags.samples(jags,c('b','tau.y'),n.iter=iterations,thin=thin,progress.bar="none"))

jags.time <- jags.time1 + jags.time2 + jags.time3
print(jags.time)
jags.coefs <- apply(jags.trace$b,1,mean)

##print(jags.trace)

cat("rcppbugs speedup:\n")
all.times <- rbind(lm.time["elapsed"],rcppbugs.time["elapsed"],mcmcpack.time["elapsed"],jags.time["elapsed"])
all.times.ratio <- cbind(all.times,all.times/rcppbugs.time["elapsed"])
colnames(all.times.ratio) <- c("time","ratio")
rownames(all.times.ratio) <- c("lm","rcppbugs","mcmcpack","jags")
print(round(all.times.ratio,2))

cat("coef comparison:\n")
coef.compare <- rbind(coef(lm.res),rcppbugs.coefs,summary(mcmcpack.out)$statistics[,"Mean"][1:2],jags.coefs)
rownames(coef.compare) <- c("lm","rcppbugs","mcmcpack","jags")
print(coef.compare)

