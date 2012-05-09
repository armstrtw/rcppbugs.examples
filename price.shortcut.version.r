library(rcppbugs)

age <- c(13, 14, 14,12, 9, 15, 10, 14, 9, 14, 13, 12, 9,
         10, 15, 11, 15, 11, 7, 13, 13, 10, 9, 6, 11, 15,
         13, 10, 9, 9, 15, 14, 14, 10, 14, 11, 13, 14, 10)

price <- c(2950, 2300, 3900, 2800, 5000, 2999, 3950, 2995,
           4500, 2800, 1990, 3500, 5100, 3900, 2900, 4950,
           2000, 3400, 8999, 4000, 2950, 3250, 3950, 4600,
           4500, 1600, 3900, 4200, 6500, 3500, 2999, 2600,
           3250, 2500, 2400, 3990, 4600, 450,4700)

y <- price/1e3
X <- cbind(1,age)

b <- mcmc.normal(rnorm(ncol(X)),0,0.0001)
tau.y <- mcmc.gamma(runif(1),0.01,0.01)
y.hat <- linear(X,b)
y.lik <- mcmc.normal(y,mu=y.hat,tau=tau.y,observed=TRUE)
m <- create.model(b, tau.y, y.hat, y.lik)

cat("running model...\n")
runtime <- system.time(ans <- run.model(m, iterations=1e5L, burn=1e5L, adapt=1e3L, thin=10L))
cat("acceptance.ratio:",get.ar(ans),"\n")
cat("a",median(ans[["b"]][,1]),"\n")
cat("b",median(ans[["b"]][,2]),"\n")
cat("tau.y",mean(ans[["tau.y"]]),"\n")

print(runtime)

cat("lm output:\n")
print(lm(y ~ age)$coefficients)


