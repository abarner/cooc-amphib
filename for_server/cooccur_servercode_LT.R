
if (!require(BoolNet)){
  install.packages("BoolNet", repos="http://cran.rstudio.com/")
  library(BoolNet)
}
if (!require(vegan)){
  install.packages("vegan", repos="http://cran.rstudio.com/")
  library(vegan)
}
if (!require(magrittr)){
  install.packages("magrittr", repos="http://cran.rstudio.com/")
  library(magrittr)
}
if (!require(dplyr)){
  install.packages("dplyr", repos="http://cran.rstudio.com/")
  library(dplyr)
}
if (!require(readr)){
  install.packages("readr", repos="http://cran.rstudio.com/")
  library(readr)
}

Working.directory <-"/raid1/home/fw/hallmant/for_server" # This is for CGRB
setwd(Working.directory) 

hood_commat_chron <- read_csv(file = "hood_commat_chron.csv")
hood_ad_commat_chron <- read_csv(file = "hood_ad_commat_chron.csv")

# original data file has a "DATE" column, use this instead of "VISIT."
hood_original <- read_csv(file="HOOD_Amphibians_original.csv")
#View(hood_original)
hood_original <- hood_original[-c(1:2), ] # remove first two rows, which are all NAs for some reason
# I only want columns 3 (VISIT.) and 44 (DATE) of 74
hood_original <- hood_original[-c(1:2,4:43,45:74) ] 
# convert "DATE" to date format
hood_original$DATE<-as.Date(hood_original$DATE, format="%d-%b-%y")
#str(hood_original)
# convert date to numeric 
hood_original$DATE.num<-as.numeric(hood_original$DATE)
#View(hood_original)
# Scale the numeric date so that the earliest date starts at 1 (instead of 5738)
hood_original$DATE.num.scaled <- cbind(apply(hood_original[,3], 1, function(x) x + 1 - min(hood_original[,3], na.rm=T)))
# remove the DATE and DATE.num columns
hood_original <- hood_original[-c(2:3) ] 

# Join the "DATE.num.scaled" column from hood_original to hood_ad_commat_chron by "VISIT."
hood_ad_commat_chronv2 <- merge(hood_ad_commat_chron, hood_original, by="VISIT.") 
#View(hood_ad_commat_chronv2)
# rearrange so that site column is first
hood_ad_commat_chronv2 <- hood_ad_commat_chronv2[,c(ncol(hood_ad_commat_chronv2), 1:10)]  
hood_ad_commat_chronv2 <- hood_ad_commat_chronv2[-2 ] # remove VISIT.
# str(hood_ad_commat_chronv2)
hood_ad_commat_chronv2$SiteCode<-as.factor(hood_ad_commat_chronv2$SiteCode) # convert SitecCode to factor

# order the dataframe to keep DATE in sequential, numeric order
hood_ad_commat_chronv2<-hood_ad_commat_chronv2[order(hood_ad_commat_chronv2[,1]),] 

# export new csv file
# write.csv(hood_ad_commat_chronv2, "hood_ad_commat_chron_LT.csv")




#### Load data ####
hood_ad_commat_chronv2 <- read_csv(file = "hood_ad_commat_chron_LT.csv")


# split dataframe into a list of dataframes by levels of the factor SiteCode
hood_ad_commat_listv2 <- split(hood_ad_commat_chronv2, hood_ad_commat_chronv2$SiteCode)
# hood_ad_commat_listv2[8] # check numeric order is intact


hood_ad_bin_list <- list() # species x date matrices (binarized), each column is the row number
                          # corresponding to Date (in numerical order)
sites <- levels(factor(unique(hood_ad_commat_chronv2$SiteCode)))
for (s in sites){                 
  hood_ad_bin_list[[s]] <- decostand(select(hood_ad_commat_listv2[[s]], 
                                        AMGR:TAGR), method = "pa", na.rm = TRUE)
  tmp <- hood_ad_bin_list[[s]]
  tmp[is.na(tmp)] <- 0
  tmp <- t(tmp)
  hood_ad_bin_list[[s]] <- tmp
}



#### Construct networks ####
hood_ad_net_list <- list() # construct Boolean networks by site
for (i in 1:length(hood_ad_bin_list)) {
  net_tmp <- reconstructNetwork(hood_ad_bin_list[i], method = "bestfit", 
                                maxK = nrow(hood_ad_bin_list[[1]]), 
                                returnPBN = TRUE, # each alternative function has the same probability
                                readableFunctions  = TRUE)
  hood_ad_net_list[[i]] <- net_tmp
} 

ad_net_tot <- reconstructNetwork(hood_ad_bin_list, # construct 1 network for all sites combined
                                 method = "bestfit",
                                 returnPBN = TRUE, 
                                 maxK = nrow(hood_ad_bin_list[[1]]),
                                 readableFunctions = TRUE)


hood_ad_net_list_F <- list() # construct networks without probabilities
for (i in 1:length(hood_ad_bin_list)) {
  net_tmp <- reconstructNetwork(hood_ad_bin_list[i], method = "bestfit", 
                                maxK = nrow(hood_ad_bin_list[[1]]), 
                                returnPBN = FALSE,
                                readableFunctions  = TRUE)
  hood_ad_net_list_F[[i]] <- net_tmp
}

ad_net_tot_F <- reconstructNetwork(hood_ad_bin_list, # construct 1 network for all sites combined
                                 method = "bestfit",
                                 returnPBN = FALSE, # no probabilities associated
                                 maxK = nrow(hood_ad_bin_list[[1]]),
                                 readableFunctions = TRUE)

plotNetworkWiring(hood_ad_net_list[[1]])
plotNetworkWiring(hood_ad_net_list[[2]])
plotNetworkWiring(hood_ad_net_list[[3]])
plotNetworkWiring(hood_ad_net_list[[4]])
plotNetworkWiring(hood_ad_net_list[[5]])
plotNetworkWiring(hood_ad_net_list[[6]])
plotNetworkWiring(hood_ad_net_list[[7]])
plotNetworkWiring(hood_ad_net_list[[8]])


#### Construct networks individually (no for loops) ####

# By doing the for loops above, species are retained at sites even when never present
# this makes plotting difficult because some species, even when present, are not predicted
# to be linked to other species so you can't differentiate them from the truly absent species.

# split dataframe into a list of dataframes by levels of the factor SiteCode
hood_ad_commat_listv2 <- split(hood_ad_commat_chronv2, hood_ad_commat_chronv2$SiteCode)
# hood_ad_commat_listv2[8] # check numeric order is intact


hood_ad_bin_list <- list() # species x date matrices (binarized), each column is the row number
                          # corresponding to Date (in numerical order)
sites <- levels(factor(unique(hood_ad_commat_chronv2$SiteCode)))
for (s in sites){                 
  hood_ad_bin_list[[s]] <- decostand(select(hood_ad_commat_listv2[[s]], 
                                        AMGR:TAGR), method = "pa", na.rm = TRUE)
  tmp <- hood_ad_bin_list[[s]]
  tmp[is.na(tmp)] <- 0
  tmp <- t(tmp)
  hood_ad_bin_list[[s]] <- tmp
}

BRUR.hood.mat<-hood_ad_bin_list[[1]]
CAMAS.hood.mat<-hood_ad_bin_list[[2]]
DRY.hood.mat<-hood_ad_bin_list[[3]]
FROG.hood.mat<-hood_ad_bin_list[[4]]
FRY.hood.mat<-hood_ad_bin_list[[5]]
JACK.hood.mat<-hood_ad_bin_list[[6]]
LCM.hood.mat<-hood_ad_bin_list[[7]]
WEENS.hood.mat<-hood_ad_bin_list[[8]]

# Remove rows with all zeros (those are species that are never present at a site)  
BRUR.hood.mat <- BRUR.hood.mat[apply(BRUR.hood.mat[,-1], 1, function(x) !all(x==0)),]
CAMAS.hood.mat <- CAMAS.hood.mat[apply(CAMAS.hood.mat[,-1], 1, function(x) !all(x==0)),]
DRY.hood.mat <- DRY.hood.mat[apply(DRY.hood.mat[,-1], 1, function(x) !all(x==0)),]
FROG.hood.mat <- FROG.hood.mat[apply(FROG.hood.mat[,-1], 1, function(x) !all(x==0)),]
FRY.hood.mat <- FRY.hood.mat[apply(FRY.hood.mat[,-1], 1, function(x) !all(x==0)),]
JACK.hood.mat <- JACK.hood.mat[apply(JACK.hood.mat[,-1], 1, function(x) !all(x==0)),]
LCM.hood.mat <- LCM.hood.mat[apply(LCM.hood.mat[,-1], 1, function(x) !all(x==0)),]
WEENS.hood.mat <- WEENS.hood.mat[apply(WEENS.hood.mat[,-1], 1, function(x) !all(x==0)),]


BRUR.net<-reconstructNetwork(BRUR.hood.mat, method = "bestfit", 
                                maxK = nrow(BRUR.hood.mat), 
                                returnPBN = TRUE, # each alternative function has the same probability
                                readableFunctions  = TRUE)
CAMAS.net<-reconstructNetwork(CAMAS.hood.mat, method = "bestfit", 
                                maxK = nrow(BRUR.hood.mat), 
                                returnPBN = TRUE, # each alternative function has the same probability
                                readableFunctions  = TRUE)
DRY.net<-reconstructNetwork(DRY.hood.mat, method = "bestfit", 
                                maxK = nrow(BRUR.hood.mat), 
                                returnPBN = TRUE, # each alternative function has the same probability
                                readableFunctions  = TRUE)
FROG.net<-reconstructNetwork(FROG.hood.mat, method = "bestfit", 
                                maxK = nrow(BRUR.hood.mat), 
                                returnPBN = TRUE, # each alternative function has the same probability
                                readableFunctions  = TRUE)
FRY.net<-reconstructNetwork(FRY.hood.mat, method = "bestfit", 
                                maxK = nrow(BRUR.hood.mat), 
                                returnPBN = TRUE, # each alternative function has the same probability
                                readableFunctions  = TRUE)
JACK.net<-reconstructNetwork(JACK.hood.mat, method = "bestfit", 
                                maxK = nrow(BRUR.hood.mat), 
                                returnPBN = TRUE, # each alternative function has the same probability
                                readableFunctions  = TRUE)
LCM.net<-reconstructNetwork(LCM.hood.mat, method = "bestfit", 
                                maxK = nrow(BRUR.hood.mat), 
                                returnPBN = TRUE, # each alternative function has the same probability
                                readableFunctions  = TRUE)
WEENS.net<-reconstructNetwork(WEENS.hood.mat, method = "bestfit", 
                                maxK = nrow(BRUR.hood.mat), 
                                returnPBN = TRUE, # each alternative function has the same probability
                                readableFunctions  = TRUE)

# Panel plot of the four sites with "interactions"
par(mfrow=c(2,2))
par(mar=c(2,2,2,2), oma=c(1,0,1,0))

# CAMAS
g.CAMAS<-plotNetworkWiring(CAMAS.net, layout=layout.circle, plotIt = FALSE)
g.CAMAS<-simplify(g.CAMAS, remove.multiple = TRUE, remove.loops=FALSE, 
                  edge.attr.comb = c(weight="sum" ))
plot(g.CAMAS, 
     layout=layout.circle,
     vertex.size=40,
     vertex.frame.color="white",
     vertex.color="white",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.2,
     edge.color="black",
     edge.width=2, edge.curved=0.25)
mtext(text = "CAMAS",side = 3,line = 0, cex=1.5)


# JACK
g.JACK<-plotNetworkWiring(JACK.net, layout=layout.circle, plotIt = FALSE)
g.JACK<-simplify(g.JACK, remove.multiple = TRUE, remove.loops=FALSE, 
                  edge.attr.comb = c(weight="sum" ))
plot(g.JACK, 
     layout=layout.circle,
     vertex.size=40,
     vertex.frame.color="white",
     vertex.color="white",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.2,
     edge.color="black",
     edge.width=2, edge.curved=0.25)
mtext(text = "JACK",side = 3,line = 0, cex=1.5)

# LCM
g.LCM<-plotNetworkWiring(LCM.net, layout=layout.circle, plotIt = FALSE)
g.LCM<-simplify(g.LCM, remove.multiple = TRUE, remove.loops=FALSE, 
                  edge.attr.comb = c(weight="sum" ))
plot(g.LCM, 
     layout=layout.circle,
     vertex.size=40,
     vertex.frame.color="white",
     vertex.color="white",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.2,
     edge.color="black",
     edge.width=2, edge.curved=0.25)
mtext(text = "LCM",side = 3,line = 0, cex=1.5)

# WEENS
g.WEENS<-plotNetworkWiring(WEENS.net, layout=layout.circle, plotIt = FALSE)
g.WEENS<-simplify(g.WEENS, remove.multiple = TRUE, remove.loops=FALSE, 
                  edge.attr.comb = c(weight="sum" ))
plot(g.WEENS, 
     layout=layout.circle,
     vertex.size=40,
     vertex.frame.color="white",
     vertex.color="white",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.2,
     edge.color="black",
     edge.width=2, edge.curved=0.25)
mtext(text = "WEENS",side = 3,line = 0, cex=1.5)

# end

## Sites with no "interactions"
# BRUR
g.BRUR<-plotNetworkWiring(BRUR.net, layout=layout.circle, plotIt = FALSE)
g.BRUR<-simplify(g.BRUR, remove.multiple = TRUE, remove.loops=FALSE, 
                  edge.attr.comb = c(weight="sum" ))
plot(g.BRUR, 
     layout=layout.circle,
     vertex.size=40,
     vertex.frame.color="white",
     vertex.color="white",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.2,
     edge.color="black",
     edge.width=2, edge.curved=0.25)
mtext(text = "BRUR",side = 3,line = 0, cex=1.5)

# DRY
g.DRY<-plotNetworkWiring(DRY.net, layout=layout.circle, plotIt = FALSE)
g.DRY<-simplify(g.DRY, remove.multiple = TRUE, remove.loops=FALSE, 
                  edge.attr.comb = c(weight="sum" ))
plot(g.DRY, 
     layout=layout.circle,
     vertex.size=40,
     vertex.frame.color="white",
     vertex.color="white",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.2,
     edge.color="black",
     edge.width=2, edge.curved=0.25)
mtext(text = "DRY",side = 3,line = 0, cex=1.5)

# FROG
g.FROG<-plotNetworkWiring(FROG.net, layout=layout.circle, plotIt = FALSE)
g.FROG<-simplify(g.FROG, remove.multiple = TRUE, remove.loops=FALSE, 
                  edge.attr.comb = c(weight="sum" ))
plot(g.FROG, 
     layout=layout.circle,
     vertex.size=40,
     vertex.frame.color="white",
     vertex.color="white",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.2,
     edge.color="black",
     edge.width=2, edge.curved=0.25)
mtext(text = "FROG",side = 3,line = 0, cex=1.5)

# FRY
g.FRY<-plotNetworkWiring(FRY.net, layout=layout.circle, plotIt = FALSE)
g.FRY<-simplify(g.FRY, remove.multiple = TRUE, remove.loops=FALSE, 
                  edge.attr.comb = c(weight="sum" ))
plot(g.FRY, 
     layout=layout.circle,
     vertex.size=40,
     vertex.frame.color="white",
     vertex.color="white",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.2,
     edge.color="black",
     edge.width=2, edge.curved=0.25)
mtext(text = "FRY",side = 3,line = 0, cex=1.5)




## Plot of network where all sites are aggregated
g.all<-plotNetworkWiring(ad_net_tot, layout=layout.circle, plotIt = FALSE)
g.all<-simplify(g.all, remove.multiple = TRUE, remove.loops=FALSE, 
                  edge.attr.comb = c(weight="sum" ))
plot(g.all, 
     layout=layout.circle,
     vertex.size=40,
     vertex.frame.color="white",
     vertex.color="white",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.2,
     edge.color="black",
     edge.width=2, edge.curved=0.25)
mtext(text = "All sites aggregated",side = 3,line = 0, cex=1.5)




net <- generateRandomNKNetwork(n=8, k=8) # comparison of results to a random network
plotNetworkWiring(net)


#### original code...####

sites <- levels(factor(unique(hood_commat_chron$SiteCode)))

hood_ad_commat_list <- list() # breaks up dataframe into sites
for (s in sites){
  hood_ad_commat_chronv2 %>% # replaced with new version/date remains in correct order
    filter(SiteCode == s) -> hood_ad_commat_list[[s]]
}

hood_ad_bin_list <- list() # species x date matrices (binarized)
for (s in sites){
  hood_ad_bin_list[[s]] <- decostand(select(hood_ad_commat_list[[s]], 
                                        AMGR:TAGR), method = "pa", na.rm = TRUE)
  tmp <- hood_ad_bin_list[[s]]
  tmp[is.na(tmp)] <- 0
  tmp <- t(tmp)
  hood_ad_bin_list[[s]] <- tmp
}

hood_ad_net_list <- list() 
for (i in 1:length(hood_ad_bin_list)) {
  net_tmp <- reconstructNetwork(hood_ad_bin_list[i], method = "bestfit", 
                                maxK = nrow(hood_ad_bin_list[[1]]), 
                                returnPBN = TRUE,
                                readableFunctions  = TRUE)
  hood_ad_net_list[[i]] <- net_tmp
} 


ad_net_tot <- reconstructNetwork(hood_ad_bin_list, 
                                 method = "bestfit",
                                 returnPBN = TRUE, 
                                 maxK = nrow(hood_ad_bin_list[[1]]),
                                 readableFunctions = TRUE)

hood_ad_net_list_F <- list()
for (i in 1:length(hood_ad_bin_list)) {
  net_tmp <- reconstructNetwork(hood_ad_bin_list[i], method = "bestfit", 
                                maxK = nrow(hood_ad_bin_list[[1]]), 
                                returnPBN = FALSE,
                                readableFunctions  = TRUE)
  hood_ad_net_list_F[[i]] <- net_tmp
}

ad_net_tot_F <- reconstructNetwork(hood_ad_bin_list, 
                                 method = "bestfit",
                                 returnPBN = FALSE, 
                                 maxK = nrow(hood_ad_bin_list[[1]]),
                                 readableFunctions = TRUE)



hood_commat_list <- list()
for (s in sites){
  hood_commat_chron %>%
    filter(SiteCode == s) -> hood_commat_list[[s]]
}

hood_bin_list <- list()
for (s in sites){
  hood_bin_list[[s]] <- decostand(select(hood_commat_list[[s]], 
                                         AMGR_Adults:TAGR_Larvae), method = "pa", na.rm = TRUE)
  tmp <- hood_bin_list[[s]]
  tmp[is.na(tmp)] <- 0
  tmp <- t(tmp)
  hood_bin_list[[s]] <- tmp
}

hood_net_list <- list()
for (i in 1:length(hood_bin_list)) {
  net_tmp <- reconstructNetwork(hood_bin_list[i], method = "bestfit", 
                                maxK = nrow(hood_bin_list[[1]]), 
                                returnPBN = TRUE,
                                readableFunctions  = TRUE)
  hood_net_list[[i]] <- net_tmp
}

net_tot <- reconstructNetwork(hood_bin_list, 
                              method = "bestfit",
                              returnPBN = TRUE,
                              maxK = nrow(hood_bin_list[[1]]),
                              readableFunctions = TRUE)

hood_net_list_F <- list()
for (i in 1:length(hood_bin_list)) {
  net_tmp <- reconstructNetwork(hood_bin_list[i], method = "bestfit", 
                                maxK = nrow(hood_bin_list[[1]]), 
                                returnPBN = FALSE,
                                readableFunctions  = TRUE)
  hood_net_list_F[[i]] <- net_tmp
}

net_tot_F <- reconstructNetwork(hood_bin_list, 
                              method = "bestfit",
                              returnPBN = FALSE,
                              maxK = nrow(hood_bin_list[[1]]),
                              readableFunctions = TRUE)



save.image(file='myEnvironment.RData')
