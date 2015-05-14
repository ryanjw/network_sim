temp<-dataset
results<-matrix(nrow=(dim(temp)[2])*(dim(temp)[2]-1)/2,ncol=6)
row_count<-1
for(b in 1:(dim(temp)[2]-1)){
		#b<-10
		#every species will be compared to every other species, so there has to be another loop that iterates down the rest of the columns
		for(c in (b+1):(dim(temp)[2])){
			#c<-12
			#summing the abundances of species of the columns that will be compared
			species1.ab<-sum(temp[,b])
			species2.ab<-sum(temp[,c])
			results[row_count,c(1,2)]<-names(temp)[c(b,c)]
			#if the column is all 0's no co-occurrence will be performed
			if(species1.ab >1 & species2.ab >1){
				test_SP<-cor.test(temp[,b],temp[,c],method="spearman",na.action=na.rm)
				results[row_count,3]<-test_SP$estimate
				results[row_count,4]<-test_SP$p.value
				test_P<-cor.test(temp[,b],temp[,c],method="pearson",na.action=na.rm)
				results[row_count,5]<-test_P$estimate
				results[row_count,6]<-test_P$p.value
			}
			
			if(species1.ab <=1 | species2.ab <= 1){
				results[row_count,c(3:6)]<-NA
			}	
			
			row_count<-row_count+1		
			
		}
		print(paste(b/(dim(temp)[2]-1)*100,"% Done",sep=""))
		
}
	
results<-data.frame(results)
names(results)<-c("n1","n2","spearman_rho","spearman_p","pearson_r","pearson_p")

write.table(results,"simple_network_corr_results.txt",row.names=F)
# Reading in the results
results<-read.table("~/Google Drive/GIT/network_sim/simple_network_corr_results.txt",header=T,sep=" ")
library(fdrtool)
head(results)
# adding q values for spearmans and pearsons

results$spearman_q<-fdrtool(results$spearman_p, statistic="pvalue",plot=F,verbose=F)$qval
results$pearson_q<-fdrtool(results$pearson_p, statistic="pvalue",plot=F,verbose=F)$qval

#checking transformation to qval
library(ggplot2)
ggplot(subset(results, spearman_p <=0.05))+geom_point(aes(x=spearman_p,y=spearman_q))+geom_vline(xintercept=0.05)+geom_hline(yintercept=0.05)


# making the networks based on the qval

g_spearman<-graph.edgelist(as.matrix(subset(results, spearman_p < 0.05 & spearman_rho > 0)[,1:2]),directed=F)
E(g_spearman)$weight<-subset(results, spearman_p < 0.05 & spearman_rho > 0)$spearman_rho
g_spearman<-simplify(g_spearman)
g_spearman<-delete.vertices(g_spearman,which(degree(g_spearman) < 1))
plot(g_spearman) # 4 out of 45 edges 8% of relationships with q, 220/45 488% with pvalue


g_pearson<-graph.edgelist(as.matrix(subset(results, pearson_p < 0.05 & pearson_r > 0)[,1:2]),directed=F)
E(g_pearson)$weight<-subset(results, pearson_p < 0.05 & pearson_r > 0)$pearson_r
g_pearson<-simplify(g_pearson)
g_pearson<-delete.vertices(g_pearson,which(degree(g_pearson) < 1))
plot(g_pearson) # 4 out of 45 edges 8% of relationships with q, 197/45 440%

# trying poisson network

results<-read.table("~/Google Drive/GIT/network_sim/poisson_simple_network_results.txt",header=T,sep=" ")
head(results)

g_pois<-graph.edgelist(as.matrix(subset(results, lo9999 > 0)[,1:2],directed=F))
E(g_pois)$weight<-subset(results, lo9999 > 0)$mean
g_pois<-simplify(g_pois)
g_pois<-delete.vertices(g_pois,which(degree(g_pois) < 1))
plot(g_pois) # 4 out of 45 edges 8% of relationships with q, 220/45 488% with pvalue