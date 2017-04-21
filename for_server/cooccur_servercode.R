
install.packages(c("BoolNet", "vegan", "magrittr", "dplyr", "readr"))

library(BoolNet)
library(vegan)
library(magrittr)
library(dplyr)
library(readr)

# read in site x species matrix with...
# all species
hood_commat_chron <- read_csv(file = "hood_commat_chron.csv")
# adult only
hood_ad_commat_chron <- read_csv(file = "hood_ad_commat_chron.csv")

sites <- levels(factor(unique(hood_commat_chron$SiteCode)))

# make a list indexed by visit number and site, with species in columns
# each list element is a different site
hood_ad_commat_list <- list()
for (s in sites){
  hood_ad_commat_list[[s]] <- filter(hood_ad_commat_chron, SiteCode == s)
}

# binarize the data and flip it into the time series format that is 
# needed by BoolNet (1 column for each time point sampled)
# each list element is a different site
hood_ad_bin_list <- list()
for (s in sites){
  hood_ad_bin_list[[s]] <- decostand(dplyr::select(hood_ad_commat_list[[s]], AMGR:TAGR), method = "pa", na.rm = TRUE)
  tmp <- hood_ad_bin_list[[s]]
  tmp[is.na(tmp)] <- 0
  tmp <- t(tmp)
  hood_ad_bin_list[[s]] <- tmp
}

# reconstruct a different network for each site - will not run on local computer
hood_ad_net_list <- list()
for (i in 1:length(sites)) {
  net_tmp <- reconstructNetwork(hood_ad_bin_list[i], method = "bestfit", maxK = nrow(hood_ad_bin_list[[i]]), returnPBN = TRUE, readableFunctions  = TRUE)
  hood_ad_net_list[[i]] <- net_tmp
}

# reconstruct a single entire network using all site data - will not run on local computer
ad_net_tot <- reconstructNetwork(hood_ad_bin_list, method = "bestfit", returnPBN = TRUE, maxK = nrow(hood_ad_bin_list[[1]]), readableFunctions = TRUE)

# reconstruct a different network for each site, not using the "probabalistic" format - will not run on local computer
hood_ad_net_list_F <- list()
for (i in 1:length(hood_ad_bin_list)) {
  net_tmp <- reconstructNetwork(hood_ad_bin_list[i], method = "bestfit", maxK = nrow(hood_ad_bin_list[[i]]), returnPBN = FALSE,readableFunctions  = TRUE)
  hood_ad_net_list_F[[i]] <- net_tmp
}

# reconstruct a single network, not using the "probabalistic" format
ad_net_tot_F <- reconstructNetwork(hood_ad_bin_list, method = "bestfit", returnPBN = FALSE, maxK = nrow(hood_ad_bin_list[[1]]),readableFunctions = TRUE)



# do the same thing, for all species
hood_commat_list <- list()
for (s in sites){
  hood_commat_list[[s]] <- filter(hood_commat_chron, SiteCode == s)
}

hood_bin_list <- list()
for (s in sites){
  hood_bin_list[[s]] <- decostand(dplyr::select(hood_commat_list[[s]], AMGR_Adults:TAGR_Larvae), method = "pa", na.rm = TRUE)
  tmp <- hood_bin_list[[s]]
  tmp[is.na(tmp)] <- 0
  tmp <- t(tmp)
  hood_bin_list[[s]] <- tmp
}

hood_net_list <- list()
for (i in 1:length(sites)) {
  net_tmp <- reconstructNetwork(hood_bin_list[i], method = "bestfit", maxK = nrow(hood_bin_list[[i]]), returnPBN = TRUE,readableFunctions  = TRUE)
  hood_net_list[[i]] <- net_tmp
}

net_tot <- reconstructNetwork(hood_bin_list, method = "bestfit",returnPBN = TRUE,maxK = nrow(hood_bin_list[[1]]),readableFunctions = TRUE)

hood_net_list_F <- list()
for (i in 1:length(sites)) {
  net_tmp <- reconstructNetwork(hood_bin_list[i], method = "bestfit", maxK = nrow(hood_bin_list[[i]]), returnPBN = FALSE,readableFunctions  = TRUE)
  hood_net_list_F[[i]] <- net_tmp
}

net_tot_F <- reconstructNetwork(hood_bin_list, method = "bestfit",returnPBN = FALSE,maxK = nrow(hood_bin_list[[1]]),readableFunctions = TRUE)


save.image(file='myEnvironment.RData')
