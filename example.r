library('rjags')

##NR <- 20
NR <- 1e2L
NC <- 2L
##y <- matrix(rnorm(NR,1) + 10,nr=NR,nc=1L)
y <- rnorm(NR,1) + 10
## X <- matrix(nr=NR,nc=NC)
## X[,1] <- 1
## X[,2] <- y + rnorm(NR)/2 - 10
x <- y + rnorm(NR)/2 - 10

##lm.res <- lm.fit(x,y)
##print(coef(lm.res))

iterations <- 1e3L
burn <- iterations
adapt <- 1e3L
thin <- 10L


jags.time1 <- system.time(
             jags <- jags.model('example2.bug',
                                data = list('x' = x,
                                'y' = y,
                                'N' = NR),
                                n.chains = 1,
                                n.adapt = adapt
                                ))
jags.time2 <- system.time(update(jags, n.iter=burn))
jags.time3 <- system.time(jags.trace <- jags.samples(jags,c('a', 'b'),n.iter=iterations,thin=thin))


print(jags.time1 + jags.time2 + jags.time3)
length(as.vector(jags.trace$a))
print(jags.trace)
