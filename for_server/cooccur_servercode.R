
install.packages(c("BoolNet", "vegan", "magrittr", "dplyr", "readr"))

library(BoolNet)
library(vegan)
library(magrittr)
library(dplyr)
library(readr)

hood_commat_chron <- read_csv(file = "hood_commat_chron.csv")
hood_ad_commat_chron <- read_csv(file = "hood_ad_commat_chron.csv")

sites <- levels(factor(unique(hood_commat_chron$SiteCode)))

hood_ad_commat_list <- list()
for (s in sites){
  hood_ad_commat_chron %>%
    filter(SiteCode == s) -> hood_ad_commat_list[[s]]
}

hood_ad_bin_list <- list()
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
