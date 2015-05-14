library(intergraph)
library(igraph)
library(dclone)
library(MASS)
library(matrixcalc)
verts<-10
edges<-45


g<-erdos.renyi.game(verts, edges,"gnm")
g<-simplify(g)
g<-delete.vertices(g,which(degree(g) < 1))
#plot(g)
m<-as.matrix(get.adjacency(g,"both"))
mu<-rep(0,dim(m)[1])

sig<-make.symmetric(m*matrix(runif(dim(m)[1],min=.75,max=1),nrow=dim(m)[1],ncol=dim(m)[1])+diag(dim(m)[1])*1)

while(is.positive.definite(sig)==FALSE){
	sig<-matrix(runif(dim(m)[1],.75,1),nrow=dim(m)[1],ncol=dim(m)[1])
	sig<-m*sig+diag(dim(m)[1])*1
	sig<-make.symmetric(sig)
}

vars<-mvrnorm(10,mu=mu,Sigma=sig)
pvars<-pnorm(vars)
#plot(vars[,1],vars[,2])
poisvars<-qpois(pvars,10)
plot(poisvars,vars)

df<-data.frame(matrix(nrow=10,ncol=100))
df[,1:verts]<-poisvars
names(df)[1:verts]<-c(LETTERS[1:verts])
head(df)
for(i in (verts+1):100){
	rand<-rpois(10,runif(1,min=min(poisvars),max=max(poisvars)))
	df[,i]<-rand
	names(df)[i]<-paste(sample(letters,5,replace=T),collapse="")

}

head(df)
write.table(df, "~/Google Drive/GIT/network_sim/simple_network.txt",sep=" ")


