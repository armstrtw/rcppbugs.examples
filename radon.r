library(rcppbugs)

x <- read.csv("http://www.stat.columbia.edu/~gelman/arm/examples/radon/srrs2.dat",as.is=TRUE)
x <- x[x$state=="MN",]
x[,"county"] <- gsub(" .*$","",x[,"county"])
radon <- x$activity
log.radon <- log(ifelse (radon==0, .1, radon))
y <- as.matrix(log.radon)
basement <- as.matrix(x[,"floor"])

uniq.counties <- sort(unique(x[,"county"]))
J <- length(uniq.counties)
group <- as.integer(as.factor(x[,"county"]))

mu.a <- mcmc.normal(rnorm(1),0,0.0001)
tau.a <- mcmc.gamma(runif(1),0.01,0.01)
a <- mcmc.normal(rnorm(J),mu.a,tau.a)
b <- mcmc.normal(rnorm(1),0,0.0001)
tau.y <- mcmc.gamma(runif(1),0.01,0.01)
y.hat <- deterministic(function(a, b, group, basement) { a[group] +  b * basement}, a, b, group, basement)
y.lik <- mcmc.normal(y,mu=y.hat,tau=tau.y,observed=TRUE)

m <- create.model(mu.a, tau.a, a, b, tau.y, y.hat, y.lik)

cat("running model...\n")
runtime <- system.time(ans <- run.model(m, iterations=1e5L, burn=1e4L, adapt=1e3L, thin=10L))

cat("basement")
print(mean(ans[["b"]]))
cat("intercept by county\n")
print(data.frame(county=uniq.counties,intercept=apply(ans[["a"]],2,mean)))

print(runtime)
