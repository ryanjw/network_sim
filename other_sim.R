thing<-genRandomClust(3, sepVal=.75,numNonNoisy=10,numNoisy=20,numOutlier=5,numReplicate=1)
str(thing)
head(thing$datList)
plot(thing)
library(Hmisc)
library(clusterGeneration)
?rcorrmatrix

sig<-rcorrmatrix(100,.00001)
mu<-rep(0,100)
hist(sig)
vars<-mvrnorm(30,mu=mu,Sigma=sig)
pvars<-pnorm(vars)
poisvars<-qpois(pvars,10)
plot(poisvars,vars)

mantel(sig,cor(poisvars))
thing<-rcorr(poisvars)
dfr<-data.frame(thing$r)
dfr<-cbind(names(dfr),dfr)
names(dfr)[1]<-"n1"
dfP<-data.frame(thing$P)
dfP<-cbind(names(dfP),dfP)
names(dfP)[1]<-"n1"
dfr_melt<-melt(dfr)
dfP_melt<-melt(dfP)
head(dfP_melt)

dfP_sub<-subset(dfP_melt, is.na(value)==F)
df_merge<-merge(dfP_sub,dfr_melt,by=c("n1","variable"))

X<-c(poisvars[,1],poisvars[,3],poisvars[,5])
Y<-c(poisvars[,2],poisvars[,4],poisvars[,6])
BLK<-c(rep("A",30),rep("B",30),rep("C",30))
df<-data.frame(X,Y,BLK)

x<-scale(df$X)
y<-scale(df$Y)
plot(x,y)
anova(lm(y~x))

library(lme4)
library(lmerTest)

test<-lmer(scale(X)~scale(Y)+(BLK|BLK), data=df, REML=F)
test

glm(poisvars[,5]~poisvars[,1],family="poisson")

g<-graph.edgelist(as.matrix(df_merge[,1:2]),directed=F)
g<-simplify(g)
g

el<-data.frame(get.edgelist(g))
names(el)<-c("n1","n2")
names(dfP_melt)[1:2]<-c("n1","n2")
el_m<-merge(el,dfP_melt,by=c("n1","n2"))
head(el_m)

library(fdrtool)
?fdrtool
el_m$qval<-fdrtool(el_m$value,statistic="pvalue",plot=F,verbose=F)$qval

thing<-fdrtool(el_m$value,statistic="pvalue",plot=F,verbose=F)
plot(el_m$value,el_m$qval)



ps<-c(1,.05,.01,.001,.0001)
final_count<-data.frame()
counter<-0
pvals<-data.frame()
for(i in 1:1000){
	sig<-rcorrmatrix(100)
	mu<-rep(0,100)
	vars<-mvrnorm(30,mu=mu,Sigma=sig)
	pvars<-pnorm(vars)
	poisvars<-qpois(pvars,10)

	thing<-rcorr(poisvars)
	dfr<-data.frame(thing$r)
	dfr<-cbind(names(dfr),dfr)
	names(dfr)[1]<-"n1"
	dfP<-data.frame(thing$P)
	dfP<-cbind(names(dfP),dfP)
	names(dfP)[1]<-"n1"
	dfr_melt<-melt(dfr)
	dfP_melt<-melt(dfP)
	
	#head(dfr_melt)
	names(dfr_melt)[2:3]<-c("n2","r")
	#head(dfP_melt)
	names(dfP_melt)[2]<-c("n2")
	dfs_melt<-merge(subset(dfr_melt, r > 0),dfP_melt,by=c("n1","n2"))
	#head(dfs_melt)
	
	for(p in 1:length(ps)){
	
		dfP_sub<-subset(dfs_melt, is.na(value)==F & value < ps[p])
		g<-graph.edgelist(as.matrix(dfP_sub[,1:2]),directed=F)
		g<-simplify(g)
		Vcount<-length(V(g))
		Ecount<-length(E(g))
		rez<-data.frame(ps[p],Vcount,Ecount)
		final_count<-rbind(final_count,rez)
	}
	head(dfs_melt)
	dfs_melt<-na.omit(dfs_melt)
	dfs_melt$qval<-fdrtool(dfs_melt$value,statistic="pvalue",plot=F,verbose=F)$qval
	dfs_melt$iter<-i
	pvals<-rbind(pvals,dfs_melt[,3:6])
	counter<-counter+1
	if(counter > 25){
		print(i/1000)
		counter<-0
	}
}

head(final_count)

library(ggplot2)

ggplot(final_count)+geom_boxplot(aes(x=as.factor(ps.p.),y=Ecount))+scale_y_log10()
ggplot(pvals)+geom_point(aes(x=r,y=value))