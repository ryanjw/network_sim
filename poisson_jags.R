jagsmodel<-"model{
	alpha0 ~ dnorm(0,0.0001)
	alpha1 ~ dnorm(0, 0.0001)
	
	for(i in 1:N){
		y[i] ~ dpois(lambda[i])
		log(lambda[i])<-alpha0 +alpha1 * A[i]
	}

}"
