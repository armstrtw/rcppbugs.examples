require(rcppbugs)
require(MCMCpack)

wallet.dat <- read.csv('/home/gsinha/MyStuff/R/Whit/wallet.csv',header=TRUE)
wallet.dat[,'outcome'] <- ifelse(wallet.dat[,'wallet'] %in% 1:2,1,0)

X <- matrix(0, nrow=dim(wallet.dat)[1],ncol=dim(wallet.dat)[2]-1)
X[,1] <- as.double(1)

X[,2:5] <- as.matrix(wallet.dat[,c('male','business','punish','explain')])
X[] <- as.double(X)
y <- as.matrix(wallet.dat[,'outcome'])

## init to glm output (this is what MCMCpack does)
b.glm.est <- as.vector(coef(glm(outcome ~ male + business + punish + explain, family=binomial(link='logit'), data=wallet.dat)))

jj <- rnorm(100)

##b <- mcmc.normal(rnorm(ncol(X)),mu=0,tau=0.0001)
b <- mcmc.normal(b.glm.est,mu=0,tau=0.01)
y.hat <- logistic(X,b)
y.lik <- mcmc.bernoulli(y,p=y.hat,observed=TRUE)
m <- create.model(b,y.hat,y.lik)

iterations <- 1e5L
burn <- 1e5L
thin <- 10L

cat("running model...\n")
runtime <- system.time(ans <- run.model(m, iterations=iterations, burn=burn, adapt=0L, thin=thin))
print(apply(ans[["b"]],2,mean))

cat("rcppbugs runtime:\n")
print(runtime)

##fff <- glm(outcome ~ male + business + punish + explain, family=binomial(link='logit'), data=wallet.dat)
##print(summary(fff))

##debug(MCMClogit)
##mcmclogit.runtime <- system.time(l.mcmc <- MCMClogit(outcome ~ male + business + punish + explain, burnin = 1e5L, mcmc = 1e5L, thin=10L, data = wallet.dat))

mcmclogit.runtime <- system.time(l.mcmc <- MCMClogit(outcome ~ male + business + punish + explain, burnin = burn, mcmc = iterations, thin=thin, data = wallet.dat))

cat("mcmclogit.runtime:\n")
print(mcmclogit.runtime)

##print(summary(l.mcmc))
##print(apply(ans$b,2,median))

res <- data.frame(rcppbugs=apply(ans$b,2,median),mcmc.pack=summary(l.mcmc)$quantiles[,"50%"])
print(res)
