
# reconstruct network from time series
# initially developed for gene regulation network reconstruction from gene expression data
# extended in Steinway et al. 2015 Plos Comp Bio for species interaction networks
library(tidyverse)  # for data manipulation
library(BoolNet)    # for interaction inference
library(vegan)      # to work with community data


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
# look at hood_or$VISIT. vs. hood_obs$VisitNum
# must match a different way
hood_tbl %>%
  filter(hood_tbl$VisitNum %in% hood_obs$VisitNum) %>%
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

# visualize the time series
hood_obs %>%
  ggplot(aes(Year, as.numeric(factor(SiteCode)), color = SiteCode)) +
  geom_point()

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
hood_commat_list <- vector("list", length(hood_ts$SiteCode))
for (s in hood_ts$SiteCode){
  hood_commat_chron %>%
  filter(SiteCode == s) -> hood_commat_list[[s]]
}
# a list component for each site:
hood_commat_list 

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
hood_ad_commat_list <- vector("list", length(hood_ts$SiteCode))
for (s in hood_ts$SiteCode){
  hood_ad_commat_chron %>%
    filter(SiteCode == s) -> hood_ad_commat_list[[s]]
}
# a list component for each site:
hood_ad_commat_list


#### implement boolnet ####

# first step in network reconstruction, need to make abundance data binary
# boolnet uses the function "binarizeTimeSeries", which uses machine learning to detect when a gene should be considered
# "on" or "off"
# i think we can just use presence/absences
hood_example_comm <- hood_ad_commat_list[["BRUR"]]
hood_example_bin <- decostand(select(hood_example_comm, AMGR:TAGR), method = "pa", na.rm = TRUE)
hood_example_bin[is.na(hood_example_bin)] <- 0
hood_example_bin <- t(hood_example_bin)
net <- reconstructNetwork(hood_example_bin, method = "bestfit", maxK = nrow(hood_example_bin))
plotNetworkWiring(net)




