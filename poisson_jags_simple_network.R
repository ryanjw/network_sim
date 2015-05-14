dataset<-df
# Using rjags library that communicates to JAGS (Just Another Gibbs Sampler)
library("rjags")


# Set up variables for the model, SoilFrac is aggregate fractions that will be random terms
Ndata<-nrow(dataset)

# Using the following function to implement the Bayesian hierarchical model
bayezymes<-function(var1, var2){
	x<-var1
	y<-var2
	#x<-dataset[,1]
	#y<-dataset[,2]
Ndata<-nrow(dataset)

# centering variables

#hist(zy)
# calling model that is called frac_model, this object is initialized in the script frac_model.R
# Using 10 Chains
	model_impf<-jags.model(textConnection(jagsmodel), data=list("N"=Ndata,"A"=x,"y"=y),n.chains=3,quiet=TRUE)
	chainsf<-coda.samples(model=model_impf, c("alpha1"),n.iter=10000,thin=10)
	model_impr<-jags.model(textConnection(jagsmodel), data=list("N"=Ndata,"A"=y,"y"=x),n.chains=3,quiet=TRUE)
	chainsr<-coda.samples(model=model_impr, c("alpha1"),n.iter=10000,thin=10)
	

	results<-matrix(nrow=0,ncol=1)
# the initial 500 values are thrown away as burn in
	for(i in 1:3){
			burnedf<-as.matrix(chainsf[[i]][501:1000,])
			burnedr<-as.matrix(chainsr[[i]][501:1000,])
			burned<-rbind(burnedf,burnedr)
			results<-rbind(results, burned)	
	}
#head(results)
#mean(results)
#quantile(exp(results),c(0.025,0.975))
results<-exp(results)
# calculating mean, median, and 95% credible interval for each variable
# also maintaining the actual values of the distribution here, standard deviation was calculated later but can be included here
	means<-mean(results)
	sds<-sd(results)
	q95<-quantile(results,c(0.025,0.975))
	q999<-quantile(results,c(0.001,0.999))
	q9999<-quantile(results,c(0.0001,0.9999))
	
	

	result_stats<-c(means,sds,q95[[1]],q95[[2]],q999[[1]],q999[[2]],q9999[[1]],q9999[[2]])
	#result_stats
	return(result_stats)

}


# results are put into the following matrix
head(dataset[,1:10])
bayezymes_results<-matrix(nrow=(dim(dataset)[2])*(dim(dataset)[2]-1)/2,ncol=10)
counter<-1
for(a in 1:(dim(dataset)[2]-1)){
	
	for(b in (a+1):dim(dataset)[2]){
		#b<-6
		
		n1<-names(dataset)[a]
		n2<-names(dataset)[b]
		output<-c(n1,n2,bayezymes(dataset[,a],dataset[,b]))
		#output
		bayezymes_results[counter,]<-(output)
		
		counter<-counter+1
		
		
	}
	print(paste(a/dim(dataset)[2]*100,"% Done",sep=""))
}
head(bayezymes_results)