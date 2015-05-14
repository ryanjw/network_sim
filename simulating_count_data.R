# simulating count data

# first need to import the following libraries
library(clusterGeneration)
library(mvtnorm)
library(vegan)
#simulating a positive definite matrix of correlations and mu values
sig<-rcorrmatrix(100,.00001)
mu<-rep(0,100)


# simulating correlated variables with n=30 and converting to poisson
?qpois
vars<-mvrnorm(30,mu=mu,Sigma=sig)
pvars<-pnorm(vars)

# generating lambda (mean) values so that distribution looks like a right-skewed distribution (as is common)
fseries<-function(X,C){
	y<-(-1/log(1-C))*(C^X/X)
	return(y)
}

ranks<-seq(1,100,1)
C<-.9999
lambdas<-round(fseries(ranks,C)*1000)

# converting data to count data
poisvars<-qpois(pvars,lambda=lambdas)


