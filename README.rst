************
Introduction
************

:Date: May 9, 2012
:Authors: Whit Armstrong
:Contact: armstrong.whit@gmail.com
:Web site: http://github.com/armstrtw/rcppbugs.examples
:License: GPL-3


Purpose
=======

This repo is a collection of examples of how to specify models in rcppbugs.  In some cases the scripts contain examples
of the same model sepcified in bugs, MCMCpack, or pymc.


Usage
=====

Here is a snip from the file linear.model.r as an example of a simple model.

::

	library(rcppbugs)
	NR <- 1e2L
	NC <- 2L
	y <- matrix(rnorm(NR,1) + 10,nr=NR,nc=1L)
	X <- matrix(nr=NR,nc=NC)
	X[,1] <- 1
	X[,2] <- y + rnorm(NR)/2 - 10
	
	lm.res <- lm.fit(X,y)
	print(coef(lm.res))
	
	## RCppBugs Model
	b <- mcmc.normal(rnorm(NC),mu=0,tau=0.0001)
	tau.y <- mcmc.gamma(sd(as.vector(y)),alpha=0.1,beta=0.1)
	y.hat <- linear(X,b)
	y.lik <- mcmc.normal(y,mu=y.hat,tau=tau.y,observed=TRUE)
	m <- create.model(b, tau.y, y.hat, y.lik)
	
	ans <- run.model(m, iterations=1e5L, burn=1e4L, adapt=1e3L, thin=10L)
	print(apply(ans[["b"]],2,mean))
	
