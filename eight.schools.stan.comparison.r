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
y <- c(28,  8, -3,  7, -1,  1, 18, 12)
sigma.y <- c(15,10,16,11,9,11,10,18)
tau.y <- sigma.y^-2

mu <- mcmc.normal(rnorm(1) , mu=0.0, tau=1.0e-6)            # noninformative prior on mu
tau <- mcmc.uniform(runif(1), lower=0, upper=1000)        # noninformative prior on tau
eta <- mcmc.normal(rnorm(J),mu=mu, tau=tau)
theta <- deterministic(function(X,a,b) { a + X * b; }, eta, mu, tau)
y.lik <- mcmc.normal(y, theta, tau.y, observed=TRUE)

m <- create.model(mu,tau,eta,theta,y.lik)

cat("running model...\n")
##runtime <- system.time(ans <- run.model(m, iterations=1e5L, burn=1e6L, adapt=2e3L, thin=10L))
runtime <- system.time(ans <- run.model(m, iterations=1e4L, burn=5e3L, adapt=2e3L, thin=10L))
print(runtime)
cat("acceptance.ratio:",get.ar(ans),"\n")

cat("eta:\n")
print(apply(ans[["eta"]],2,mean))

cat("theta:\n")
print(apply(ans[["theta"]],2,mean))

cat("mu:\n")
print(mean(ans[["mu"]]))

cat("tau:\n")
print(mean(ans[["tau"]]))
