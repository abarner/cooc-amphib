
# selected code from barner et al. 
# just as an example, code cannot be run



#### compare among the inferred networks ####

library(devtools)                     # to access r packages on github
library(betalink)                     # for network dissimilarity analysis
library(igraph);library(NetIndices)   # for general network analysis        
library(vegan)                        # for data manipulation

# read in adjacency matrices 
# turn into true 'adjacency' matrices (with only -1,1,0)

# create a list of matrices
mat_list<-list(vch5A,lne5A,crSA,crPA,crNA,bcc5A,rpr5A,sfn5A,mrh5A)
mat_list_names<-c("vch5A","lne5A","crSA","crPA","crNA","bcc5A",
                  "rpr5A","sfn5A","mrh5A")
names(mat_list)<-mat_list_names

# make igraph objects from adj. matrices
graph_list<-lapply(mat_list, graph.adjacency, mode="undirected", 
                   weighted=TRUE, diag=FALSE)

# the odds ratio method is actually "directed" so we need to fix that
graph_list[[2]]<-graph.adjacency(lne5A, mode="directed", weighted=TRUE, 
                                 diag=FALSE)

# could also use prepare_networks() function from "betalink" package
# that function seems to not be recognizing the column names in my matrix
# list so, can't use the function?

# calculate network statistics for each inferred network

# existing methods include GenInd() from the "NetIndices" package or 
# networklevel() from the "bipartite" package, both infer a 
# flow from row (i) to column (j). further, the calculation of
# indices doesn't seem to take into account empty columns/rows

# node richness (number of nodes)
noderich<-numeric()
for(i in 1:length(mat_list)) {
  noderich[i]<-sum(apply(mat_list[[i]], 1, function(x) max(abs(x))))
}

# number of links (should be the same as the number of pairs, except
# for the results from lane et al. 2014)

numlinks<-numeric()
for(i in 1:length(mat_list)) {
  if (i !=2){
    numlinks[i]<-sum(mat_list[[i]]!=0)/2
  } 
  else {
    numlinks[i]<-sum(mat_list[[i]]!=0)
  }
}
# check against number of significant pairs
numlinks[1]==nrow(vch5.pr)
numlinks[2]==nrow(lne5.pr)
numlinks[3]==nrow(ccr5.prS) 
numlinks[4]==nrow(ccr5.prP) 
numlinks[5]==nrow(ccr5.prN) 
numlinks[6]==nrow(bcc5_sig)
numlinks[7]==nrow(rpr5.pr)
numlinks[8]==nrow(sfen5.pr)
numlinks[9]==nrow(mrh5.pr)

# calculate the correct connectance for each method
# undirected connectance (L/(N*(N-1)))
connectn<-numeric()
for(i in 1:length(mat_list)) {
  connectn[i]<- numlinks[i]/(noderich[i]*(noderich[i]-1))
}

# directed connectance for odds ratio methods
connectn[2]<-numlinks[2]/(noderich[2]*noderich[2])

# make dataframe with all network stats
netstat<-data.frame(mat_list_names, noderich, numlinks, connectn)




# calculate turnover among networks

# from poisot et al. 2012 (ecology letters)
# similar to trojelsgaard et al. 2015 (prsb)

# measure overall species dissimilarity ("beta-s")
# measure dissimilarity of interactions in full web ("beta-wn")
# measure dissimilarity of interactions due to species turnover ("beta-st")
# measure network dissimilarity when only shared species 
# are considered ("beta-os")
# additive network dissimilarity: beta-wn = beta-st + beta-os

# use betalink package from poisot et al. 2012

# need to make a new set of adjacency matrices because betalink 
# seems to need no degenerate (empty) columns/rows

mat_list<-list(vch5A,lne5A,crSA,crPA,crNA,bcc5A,mrh5A,rpr5A,sfn5A)
mat_list_names<-c("vch5A","lne5A","crSA","crPA","crNA","bcc5A","mrh5A",
                  "rpr5A","sfn5A")
names(mat_list)<-mat_list_names

rm_degen<- function(mat) {
  deg_row<-which(rowSums(abs(mat))==0)
  deg_col<-which(colSums(abs(mat))==0)
  if (length(deg_row)>0|length(deg_col)>0){
    mat2<-mat[-which(rowSums(abs(mat))==0),-which(colSums(abs(mat))==0)]
  } else {
    mat2<-mat  
  }
}

mat_list_nd<-sapply(X = mat_list, FUN = rm_degen)
graph_list_nd<-lapply(mat_list_nd, graph.adjacency, mode="directed", 
                      weighted=TRUE, diag=FALSE)
# get a metaweb of all interactions
metaweb(graph_list_nd) 

# compare turnover
graph_list_nd_1<-graph_list_nd
(net_beta_1<-network_betadiversity(graph_list_nd_1))

conf<-function(avg, sdev, samp) {
  error <- qnorm(0.975)*sdev/sqrt(samp)
  left <- avg-error
  right <- avg+error
  return(c(left,right))
}

# mean species turnover among methods
mean(net_beta_1$S)
conf(mean(net_beta_1$S), sd(net_beta_1$S), length(net_beta_1$S))

# mean interaction turnover among methods
mean(net_beta_1$OS)
conf(mean(net_beta_1$OS), sd(net_beta_1$OS), length(net_beta_1$OS))

# mean whole network turnover among methods
mean(net_beta_1$WN)
conf(mean(net_beta_1$WN), sd(net_beta_1$WN), length(net_beta_1$WN))

#### what interaction pairs are consistently identified? #####

# make a list of the significant pairs
pair_list_nd<-list(vch=data.frame(vch5.pr$sp1_name,vch5.pr$sp2_name,vch5.pr$assoc),
                   lne=lne5.pr,
                   crS=ccr5.prS,
                   crP=ccr5.prP,
                   crN=ccr5.prN,
                   bcc=data.frame(bcc5_sig$sp1_name,bcc5_sig$sp2_name,bcc5_sig$mean),
                   mrh=mrh5.pr,
                   rpr=data.frame(rpr5.pr$fi,rpr5.pr$fj,rpr5.pr$mean),
                   sfn=data.frame(sfen5.pr$sp1,sfen5.pr$sp2,sfen5.pr$assoc))

# all possible pairs
mat<-matrix(nrow=97,ncol=97)
colnames(mat)<-colnames(dat5)
rownames(mat)<-colnames(dat5)
mat[upper.tri(mat)]<-1
dat <- as.data.frame(mat)
value <- stack(dat)$values
rnames <- rownames(dat)
namecol <- expand.grid(rnames, rnames)
colnames(namecol) <- c("col", "row")
res <- data.frame(namecol, value)
res2<-res[complete.cases(res$value),]
nrow(res);nrow(res2)
res2 # all the possible pairs (undirected)

# how many time does each interaction occur in each dataset
for (i in 1:length(pair_list_nd)){
  tmp<-pair_list_nd[[i]]
  for (j in 1:nrow(res2)){
    res2[j,i+3]<-ifelse(length(which(as.character(tmp[,1])==as.character(res2$col[j])
                                     &as.character(tmp[,2])==as.character(res2$row[j])))>0, 
                        tmp[which(as.character(tmp[,1])==as.character(res2$col[j])
                                  &as.character(tmp[,2])==as.character(res2$row[j])),3],
                        ifelse(length(which(as.character(tmp[,1])==as.character(res2$row[j])
                                            &as.character(tmp[,2])==as.character(res2$col[j])))>0, 
                               tmp[which(as.character(tmp[,1])==as.character(res2$row[j])
                                         &as.character(tmp[,2])==as.character(res2$col[j])),3], 
                               NA))
  }
}

res3<-abs(res2[,4:ncol(res2)])
res3[is.na(res3)]<-0
res3<-decostand(res3,method="pa")
res4<-data.frame(res2[,1:2],res3)
res4$tally<-rowSums(res4[,3:ncol(res4)])

res4_ordered<-res4[order(res4$tally, decreasing=TRUE),]
head(res4_ordered, n=20)

# how many times was an interaction 
# identified with same SIGN across datasets?
res5<-res2
res5[is.na(res5)]<-0
res6<-apply(res5[,4:ncol(res5)], 2, function(x) ifelse(x>0,1,ifelse(x<0,-1,0)))
rowSums(abs(res6))==res4$tally # just checking
res7<-data.frame(res2[,1:2],res6)
res7$tallypos<-apply(res7[,2:ncol(res7)],1,function(x) sum(x>0))
res7$tallyneg<-apply(res7[,2:ncol(res7)],1,function(x) sum(x<0))

max(res7$tallypos)
max(res7$tallyneg)
length(pair_list_nd)







