require(rcppbugs)

## # Bugs model file for 8 schools analysis from Section 5.5 of "Bayesian Data
## # Analysis". Save this into the file "schools.bug" in your R working directory.

## model {
##   for (j in 1:J){                          # J=8, the number of schools
##     y[j] ~ dnorm (theta[j], tau.y[j])      # data model:  the likelihood
##     tau.y[j] <- pow(sigma.y[j], -2)        # tau = 1/sigma^2
##   }
##   for (j in 1:J){
##     theta[j] ~ dnorm (mu.theta, tau.theta) # hierarchical model for theta
##   }
##   tau.theta <- pow(sigma.theta, -2)        # tau = 1/sigma^2
##   mu.theta ~ dnorm (0.0, 1.0E-6)           # noninformative prior on mu
##   sigma.theta ~ dunif (0, 1000)            # noninformative prior on sigma
## }


J <- 8
sigma.y <- c(15,10,16,11,9,11,10,18)
tau.y <- sigma.y^-2

mu.theta <- mcmc.normal(rnorm(1) , mu=0.0, tau=1.0E-6)            # noninformative prior on mu
sigma.theta <- mcmc.uniform(runif(1), lower=0, upper=1000)        # noninformative prior on sigma
tau.theta <- deterministic(function(x) { x^-2 },  sigma.theta)
theta <- mcmc.normal(rnorm(J),mu=mu.theta, tau=tau.theta)

y <- mcmc.normal(c(28,  8, -3,  7, -1,  1, 18, 12), theta, tau.y, observed=TRUE)

m <- create.model(mu.theta,sigma.theta,tau.theta,theta,y)

cat("running model...\n")
runtime <- system.time(ans <- run.model(m, iterations=1e5L, burn=1e6L, adapt=2e3L, thin=10L))
print(runtime)
cat("acceptance.ratio:",get.ar(ans),"\n")

cat("theta:\n")
print(apply(ans[["theta"]],2,mean))

cat("mu.theta:\n")
print(mean(ans[["mu.theta"]]))

cat("sigma.theta:\n")
print(mean(ans[["sigma.theta"]]))
