
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
# write.csv(hood_ad_commat_chronv2, "hood_ad_commat_chron_LTedits.csv")


# split dataframe into a list of dataframes by levels of the factor SiteCode
hood_ad_commat_listv2 <- split(hood_ad_commat_chronv2, hood_ad_commat_chronv2$SiteCode)
# hood_ad_commat_listv2[8] # check numeric order is intact

hood_ad_bin_list <- list() # species x date matrices (binarized), each column is the row number
for (s in sites){                 # corresponding to Date (in numerical order)
  hood_ad_bin_list[[s]] <- decostand(select(hood_ad_commat_listv2[[s]], 
                                        AMGR:TAGR), method = "pa", na.rm = TRUE)
  tmp <- hood_ad_bin_list[[s]]
  tmp[is.na(tmp)] <- 0
  tmp <- t(tmp)
  hood_ad_bin_list[[s]] <- tmp
}

hood_ad_net_list <- list() # construct Boolean networks 
for (i in 1:length(hood_ad_bin_list)) {
  net_tmp <- reconstructNetwork(hood_ad_bin_list[i], method = "bestfit", 
                                maxK = nrow(hood_ad_bin_list[[1]]), 
                                returnPBN = TRUE,
                                readableFunctions  = TRUE)
  hood_ad_net_list[[i]] <- net_tmp
} 




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
