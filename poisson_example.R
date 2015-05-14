library(rjags)
load.module("glm")

# Generate count data using 2 covariates
nsites <- 200
alpha0 <- 1
alpha1 <- 2
alpha2 <- 3

A <- runif(n = nsites, -1, 1)   # Site covariate 1
B <- runif(n = nsites, -1, 1)   # Site covariate 2

lam <- exp(alpha0 + alpha1*A + alpha2*B)
y <- rpois(n = nsites, lambda = lam)

# Analyze with the glm function (without covariate B)
m <- glm(y ~ A, family = poisson)
m