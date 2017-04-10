
# reconstruct network from time series
# initially developed for gene regulation network reconstruction from gene expression data
# extended in Steinway et al. 2015 Plos Comp Bio for species interaction networks
library(tidyverse)  # for data manipulation
library(BoolNet)    # for interaction inference
library(vegan)      # to work with community data
library(igraph)     # for plotting networks

#### example boolnet implementation ####

# load example data
data(yeastTimeSeries)
# perform binarization with k-means
bin <- binarizeTimeSeries(yeastTimeSeries)
# reconstruct networks from binarized measurements (non probabalistic)
net <- reconstructNetwork(bin$binarizedMeasurements, method="bestfit", maxK=3, returnPBN=FALSE)
# print reconstructed net
print(net)
# plot reconstructed net
plotNetworkWiring(net)
# simulate states of system based on the boolean network
net <- reconstructNetwork(bin$binarizedMeasurements, method="bestfit", maxK=3, returnPBN=TRUE, readableFunctions = TRUE)
sim <- markovSimulation(net, numIterations = 10000, returnTable = TRUE)
getAttractors(net)
tbl <- as_tibble(cbind(initstate=as.vector(unlist(sim$table[[1]])), 
                       nxtstate=as.vector(unlist(sim$table[[2]])), 
                       prob=as.vector(unlist(sim$table[[3]]))))
tbl%>%
  arrange(-prob) %>%
  filter(prob > 0.65)

#### prep data ####

hood <- read.csv(file = "HOOD_Amphibians_Lentic_CSV_031017.csv", h=TRUE)
head(hood)
str(hood)

# alternately, read in as tibble (so easier to work with dplyr tools)
hood_tbl <- read_csv(file = "HOOD_Amphibians_Lentic_CSV_031017.csv")

# first need to rearrange "hood" so that each species is a row, each time point is a column
data("yeastTimeSeries") # <- this is the example data for the "BoolNet" method
# probably easier to do it in the other direction first (sample = row, sp = col)

# first read in original hood data to get the exact sample DATES (not in hood_tbl)
hood_or <- read_csv("HOOD_Amphibians_original.csv")
hood_or <- as_tibble(hood_or[-c(1:2),])
# note that at some point Excel changed some of the visit numbers in the original data
# to "date" format in Excel (GRRRRRRRR)
# must match a different way
hood_tbl %>%
  filter(hood_tbl$Obs %in% hood_or$OBS.) %>%
  select(VisitNum, Obs) -> hood_obs_vo
hood_or %>%
  select(VISIT., DATE, OBS.) %>%
  filter(OBS. %in% hood_obs_vo$Obs) -> hood_or_dates
hood_or_dates$OBS. <- as.numeric(hood_or_dates$OBS.)
hood_tbl_j <- left_join(hood_tbl, hood_or_dates, by = c("Obs" = "OBS."))
hood_tbl_j$DATE <- as.Date(hood_tbl_j$DATE, format = "%d-%b-%y")
# order the table by date (chronologically)
hood_tbl_j <- arrange(hood_tbl_j, DATE)

# next, reduce data to unique observation identifiers
# match the observation numbers to other identifiers (year & site)
hood_tbl_j %>%
  select(VISIT., Year, SiteCode) %>% # use VISIT. because it wasn't messed up by Excel (like VisitNum)
  group_by(VISIT.) %>%
  summarize_each(funs(max)) -> hood_obs

# visualize the time series:
# hood_obs %>%
#   ggplot(aes(Year, as.numeric(factor(SiteCode)), color = SiteCode)) +
#   geom_point()

# which sites have long time series data? (by year)
n <- 15 # length of time series you want to cut off at
hood_tbl_j %>%
  select(Year, SiteCode) %>%
  group_by(SiteCode, Year) %>%
  summarize_each(funs(max)) %>%
  group_by(SiteCode) %>%
  summarize(count = n()) %>%
  filter(count > n) -> hood_ts
# will only include the sites that have time series of > 15 years (in hood_ts)


# need to aggregate data into a community matrix
# arrange that matrix chronologically
# and split that into the different sites

# aggregate for the entire community (including eggs, juveniles, etc)
hood_tbl_j %>%
  filter(SiteCode %in% hood_ts$SiteCode) %>%
  select(VISIT., SpeciesCod, Adults, Juvs, Larvae, Eggs) %>%
  gather(Adults, Juvs, Larvae, Eggs, key = Stage, value = Abund) %>%
  unite(Sp_Stage, SpeciesCod, Stage) %>%
  spread(key = Sp_Stage, value = Abund) -> hood_commat
# organize chronologically
hood_tbl_j %>%
  select(DATE, VISIT.) %>%
  filter(VISIT. %in% hood_commat$VISIT.) %>%
  group_by(VISIT.) %>%
  arrange(DATE) %>%
  select(VISIT.) -> hood_commat_order
hood_commat_chron <- hood_commat[unique(match(hood_commat_order$VISIT., hood_commat$VISIT.)),]
# then need to separate into the different sites
hood_commat_chron<- add_column(hood_commat_chron, 
                               SiteCode = as_vector(hood_tbl_j[unique(match(hood_commat_chron$VISIT., 
                                                                            hood_tbl_j$VISIT.)), 4]))
hood_ts$SiteCode <- factor(hood_ts$SiteCode )
hood_commat_list <- list()
for (s in hood_ts$SiteCode){
  hood_commat_chron %>%
  filter(SiteCode == s) -> hood_commat_list[[s]]
}
# a list component for each site:
# hood_commat_list 


# aggregate for adults only
hood_tbl_j %>%
  filter(SiteCode %in% hood_ts$SiteCode) %>%
  select(VISIT., SpeciesCod, Adults) %>%
  spread(key = SpeciesCod, value = Adults) -> hood_ad_commat
# organize chronologically
hood_tbl_j %>%
  select(DATE, VISIT.) %>%
  filter(VISIT. %in% hood_ad_commat$VISIT.) %>%
  group_by(VISIT.) %>%
  arrange(DATE) %>%
  select(VISIT.) -> hood_ad_commat_order
hood_ad_commat_chron <- hood_ad_commat[unique(match(hood_ad_commat_order$VISIT., hood_ad_commat$VISIT.)),]
# then need to separate into the different sites
hood_ad_commat_chron<- add_column(hood_ad_commat_chron, 
                                  SiteCode = as_vector(hood_tbl_j[unique(match(hood_ad_commat_chron$VISIT., 
                                                                               hood_tbl_j$VISIT.)), 4]))
hood_ts$SiteCode <- factor(hood_ts$SiteCode )
hood_ad_commat_list <- list()
for (s in hood_ts$SiteCode){
  hood_ad_commat_chron %>%
    filter(SiteCode == s) -> hood_ad_commat_list[[s]]
}
# a list component for each site:
# hood_ad_commat_list



#### alter boolnet source code (not finished) ####

# when boolnet generates probabalistic networks, returns many possible interactions for 1 node
# but doesn't give a way to pick the best one
function(boolnet_interactions_1gene){
  interactors <- data.frame()
  for (j in seq_along(net1$interactions[[i]])){
    interactors[j,1] <- j
    interactors[j,2] <- paste(net1$interactions[[i]][[j]]$input, collapse = " ")
    interactors[j,3] <- net1$interactions[[i]][[j]]$probability
  }
  unique(interactors[,2]) ## unfinished
}
# feeds into next function

# from plotNetworkWiring(), generate an edge list for the network
# modified to select the highest probable links for plotting
net1$interactions$AMMA[[1]]$probability
gen_edge_list <- function(network) {
  edgeList <- c()
  if (inherits(network, "BooleanNetwork")) {
    for (i in seq_along(network$genes)) {
      if (network$interactions[[i]]$input[1] != 0) {
        edgeList <- rbind(edgeList, cbind(network$interactions[[i]]$input, 
                                          rep(i, length(network$interactions[[i]]$input))))
      }
    }
  } else if (inherits(network, "SymbolicBooleanNetwork")) {
    inputs <- lapply(network$interactions, getInputs, index = TRUE)
    for (i in seq_along(network$genes)) {
      edgeList <- rbind(edgeList, cbind(inputs[[i]], rep(i,
                                                         length(inputs[[i]]))))
    }
  }  else {
    for (i in seq_along(network$genes)) {
      seq_along(net1$interactions[[i]])
      seq_along(net1$interactions[[i]])
      for (j in seq_along(network$interactions[[i]])) {
        if (network$interactions[[i]][[j]]$input[1] !=0) {
          ## make "if" statement such that edgeList reflects the highest probability links
          # if (length(net1$interactions[[i]]) > 1) {
          #   
          # }
          #   else {
              edgeList <-rbind(edgeList, cbind(network$interactions[[i]][[j]]$input,
                                  rep(i, length(network$interactions[[i]][[j]]$input))))
          # }
        }
      }
    }
  }
  return(edgeList)
}


#### implement boolnet - for adult species only ####

# first step in network reconstruction, need to make abundance data binary
# boolnet uses the function "binarizeTimeSeries", which detects when a gene should be considered "on" or "off"
# can just use presence/absences (?)
# then replace NA with 0 (**** may not want to do this)
# finally, transpose matrix so runs with given functions


# example: run through steps for just 1 site
hood_example_comm <- hood_ad_commat_list[["BRUR"]]
hood_example_bin <- decostand(select(hood_example_comm, AMGR:TAGR), method = "pa", na.rm = TRUE)
hood_example_bin[is.na(hood_example_bin)] <- 0
hood_example_bin <- t(hood_example_bin)
net1 <- reconstructNetwork(hood_example_bin, method = "bestfit", maxK = nrow(hood_example_bin), returnPBN = FALSE, readableFunctions = TRUE)
# note that "returnPBN = TRUE" returns interaction lists that are ranked by probability
# should pick links with the highest probability & plot those (see code above)
plotNetworkWiring(net1, layout = layout.circle)
# this is a hack way until I figure it out
plotNetworkWiring(chooseNetwork(net1, c(1,1,1,1,1,1,1,1)), layout = layout.circle)

# now set up and run for all sites
hood_ad_bin_list <- list()
for (s in hood_ts$SiteCode){
  hood_ad_bin_list[[s]] <- decostand(select(hood_ad_commat_list[[s]], 
                                            AMGR:TAGR), method = "pa", na.rm = TRUE)
  tmp <- hood_ad_bin_list[[s]]
  tmp[is.na(tmp)] <- 0
  tmp <- t(tmp)
  hood_ad_bin_list[[s]] <- tmp
}

# run each site time series 1 by 1
plot.new()
png(filename = "timeseriesbysite.png", width = 8, height = 5, units = "in", res = 300)
par(mfrow=c(2,4))
par(mar=c(0,0,0,0), oma=c(0,0,1,0))
for (i in 1:length(hood_ad_bin_list)) {
  reconstructNetwork(hood_ad_bin_list[i], method = "bestfit", 
                     maxK = nrow(hood_example_bin), 
                     #returnPBN = TRUE, # this takes (too much?) computational time
                     readableFunctions  = TRUE) %>%
    plotNetworkWiring(, layout = layout.circle)
  mtext(side = 3, line = -0.5, paste(names(hood_ad_bin_list[i])))
}
dev.off()

hood_net_list <- list()
for (i in 1:length(hood_ad_bin_list)) {
  net_tmp <- reconstructNetwork(hood_ad_bin_list[i], method = "bestfit", 
                           maxK = nrow(hood_example_bin), 
                           #returnPBN = TRUE, # this takes (too much?) computational time
                           readableFunctions  = TRUE)
  hood_net_list[[i]] <- net_tmp
}

# use all time series to make 1 network
net_tot <- reconstructNetwork(hood_ad_bin_list, 
                   method = "bestfit",
                   #returnPBN = TRUE, # takes a lot of computational time
                   maxK = 7,
                   readableFunctions = TRUE)
net_tot # no information in the package about what the values returned here mean
plotNetworkWiring(net_tot) # plots the links between each node

### note! these network plots don't necessarily show the interactions - they are plotting all the possible
# interactions, without reference to weights, so some of these are positive and some are negative


#### implement boolnet - for all species ** takes too long to reconstruct right now #### 

# now set up and run for all sites
hood_bin_list <- list()
for (s in hood_ts$SiteCode){
  hood_bin_list[[s]] <- decostand(select(hood_commat_list[[s]], 
                                            AMGR_Adults:TAGR_Larvae), method = "pa", na.rm = TRUE)
  tmp <- hood_bin_list[[s]]
  tmp[is.na(tmp)] <- 0
  tmp <- t(tmp)
  hood_bin_list[[s]] <- tmp
}

# run each site time series 1 by 1
plot.new()
png(filename = "timeseriesbysite_all.png", width = 8, height = 5, units = "in", res = 300)
par(mfrow=c(2,4))
par(mar=c(0,0,0,0), oma=c(0,0,1,0))
for (i in 1:length(hood_bin_list)) {
  reconstructNetwork(hood_bin_list[i], method = "bestfit", 
                     maxK = nrow(hood_example_bin), 
                     #returnPBN = TRUE, # this takes (too much?) computational time
                     readableFunctions  = TRUE) %>%
    plotNetworkWiring(, layout = layout.circle)
  mtext(side = 3, line = -0.5, paste(names(hood_bin_list[i])))
}
dev.off()

hood_net_list_all <- list()
for (i in 1:length(hood_bin_list)) {
  net_tmp <- reconstructNetwork(hood_bin_list[i], method = "bestfit", 
                                maxK = nrow(hood_example_bin), 
                                #returnPBN = TRUE, # this takes (too much?) computational time
                                readableFunctions  = TRUE)
  hood_net_list_all[[i]] <- net_tmp
}

# use all time series to make 1 network
net_tot_all <- reconstructNetwork(hood_bin_list, 
                              method = "bestfit",
                              #returnPBN = TRUE, # takes a lot of computational time
                              maxK = 8,
                              readableFunctions = TRUE)
net_tot_all # no information in the package about what the values returned here mean
plotNetworkWiring(net_tot_all) # plots the links between each node



