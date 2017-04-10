## Co-occurrence Chapter

#### Load required packages ####

install.packages('devtools', 'vegan', 'spaa', 'igraph')
library(FSA)
library(devtools)       # to download packages on github repositories
library(vegan)          # for community analysis & data manipulation
library(spaa)           # for data manipulation
library(igraph)         # to make association network plots
library(reshape2)
library(plyr)
library(dplyr)

#### Abbreviations and Info ####

# Lentic = standing water (primary habitat for pond-breeding species). They typically
# arrive in late spring to breed, laying eggs which hatch out after a couple of weeks. 
# Larvae (tadpoles) remain throughout the summer - basically all of the research on spp 
# interactions is done on larval life stage. A common community (local pond) contains
# multiple Anuran (frog) species that compete for algal food resources and refuge, in
# addition to 1 (or more) salamander. The salamander is the top predator in these systems
# when fish are absent. 
# At the end of the summer, most species metamorphose and emerge into their juvenile, 
# terrestrial form. Some species, such as the Long-toed salamander, overwinter in 
# their aquatic habitat. To my knowledge, no frog species overwinter. 
# Overwintering allows a species to continue to grow as 
# larvae and emerge at a larger body size the following year (or even up to 3 years 
# later). We hypothesize this to be a fitness advantage since they become top 
# predators in the aquatic environment and larger body size confers greater 
# reproductive success post-emergence. Interestingly, if conspecific densities 
# become too large relative to prey density in the pond some of the salamander species will 
# morph into a cannibilistic form and predate on smaller/younger conspecifics.  

# Lentic species of interest:
  # AMMA - Ambystoma macrodactylum (Long-toed salamander) - I have experimental data
    # on this species showing prey preference for Pacific chorus frogs, and 
    # opportunistis predation on Western toads and Cascades frogs (although Cascades frogs
    # can easily escape predation with a size advantage over their competitors)
  # AMGR - Ambystoma gracile (Northwestern salamander) - Not much work done on this species
    # experimentally, but expect this species to be a significant predator as they show 
    # similar life history strategies as AMMA (over-wintering as larvae that results in
    # very large, predatory individuals). Interestingly, AMMA and AMGR are similar enough
    # that other researchers have looked into their niche overlap and partitioning. Studies
    # have shown that these two species are quite often allopatric in their distributions.
  # PSRE - Pseudacris regilla (Pacific chorus frog) - smallest bodied Anuran, my 
    # experimental data shows this species to be the preferred prey item for AMMA as well
    # as a relatively weak competitor (compared to RACA and ANBO)
  # RACA - Rana cascadae (Cascades frog) - largest bodied Anuran, dominant competitor, 
    # least preferred prey item of AMMA 
  # ANBO - Anaxyrus boreas (Western toad) - significant population declines, can be 
    # found at a few sites in very high abundance (prolific breeders) where they dominate
    # in density relative to other species. Intermediate competitor. Anecdotally considered
    # unpalatable as a prey item due to bufotoxins, but my experiment showed that this did 
    # not deter AMMA from predating on them (and quite heavily)
  # TAGR - Taricha granulosa (Rough-skinned newt) - another Caudate/salamander in this system
    # but I don't expect much spatial overlap with this species and other salamanders. 
    # They typically exist at lower elevations. Not much known on this species as far as
    # interactions. Also contain tetrodotoxin, a neurotoxin that makes them unpalatable
    # and quite deadly to their predators (snakes, birds). Not sure how this would affect
    # other amphibians, but interesting nonetheless.
  # RAAU - Rana aurora (Northern red-legged frog) - very similar to RACA, they likely 
    # diverged from a recent common ancestor. RACA exists at higher elevations than RAAU, 
    # with some overlap at intermediate elevations. Similar life history strategies, but
    # not much known on this species as far as interactions.


#### Mt. Rainier (MORA) ####
#### Load [original] MORA amphibian co-occurrence data ####
MORA <- read.csv("MORA_Amphibians_joined_CSV_041916.csv", header=T)
View(MORA)
str(MORA)
colnames(MORA)
 [1] "ObjectID"        "RecordID"        "SourceDb"        "Program"        
 [5] "SurveyType"      "Date"            "Year"            "Park"           
 [9] "ParkCode"        "SiteName"        "AltSiteNam"      "X_UTME"         
[13] "Y_UTMN"          "UTMerror"        "Datum"           "Elevation"      
[17] "SpeciesCode"     "Genus"           "Species"         "LifeStage"      
[21] "OldLifeSta"      "Detection"       "Count"           "Notes"          
[25] "Notes2"          "Unique_SiteID"   "Centroid_X_UTME" "Centroid_Y_UTMN"

## don't need every column for the purposes of preliminary
## co-occurrence analyses 
## keep columns 1, 2, 7, 17-19, 26-28
MORA.v2 <- MORA[ , -c(3:6,8:16,20:25) ]
View(MORA.v2)
str(MORA.v2)
'data.frame':	6864 obs. of  9 variables:
 $ ObjectID       : int  1 2 3 4 5 6 7 8 9 10 ...
 $ RecordID       : int  1 2 3 4 5 6 7 8 9 10 ...
 $ Year           : Factor w/ 29 levels "1984","1985",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ SpeciesCode    : Factor w/ 18 levels "AMGR","AMMA",..: 1 5 5 8 9 9 15 17 17 18 ...
 $ Genus          : Factor w/ 10 levels "ambystoma","Ambystoma",..: 2 4 4 5 6 6 9 9 9 10 ...
 $ Species        : Factor w/ 15 levels "aurora","boreas",..: 6 13 13 12 5 5 1 11 11 7 ...
 $ Unique_SiteID  : Factor w/ 445 levels "Lentic1","Lentic10",..: 315 406 392 392 438 437 315 436 392 406 ...
 $ Centroid_X_UTME: num  584625 584643 583542 583542 586174 ...
 $ Centroid_Y_UTMN: num  5176584 5178842 5198029 5198029 5204704 ...


#### Load MORA Lentic amphibian co-occurrence data *** ####
## Use this data file for co-occurrence analyses
## Removed a few instances of PLVA, ASTR, & DITE (stream/river species)
MORA.Lentic <- read.csv("MORA_Amphibians_Lentic_CSV_033117.csv", header=T)
View(MORA.Lentic)
str(MORA.Lentic)
'data.frame':	4738 obs. of  22 variables:
 $ ObjectID       : int  1040 1041 1042 1043 1044 1045 1046 1047 1048 1049 ...
 $ RecordID       : int  6088 6089 6090 6091 6092 6093 6094 6095 6096 6097 ...
 $ Year           : int  1991 1991 1991 1991 1991 1991 1991 1991 1991 1991 ...
 $ SpeciesCod     : Factor w/ 7 levels "AMGR","AMMA",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Genus          : Factor w/ 5 levels "Ambystoma","Anaxyrus",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Species        : Factor w/ 7 levels "aurora","boreas",..: 4 4 4 4 4 4 4 4 4 4 ...
 $ Unique_Sit     : Factor w/ 235 levels "Lentic1","Lentic10",..: 6 6 7 197 137 180 230 229 195 196 ...
 $ Centroid_X_UTME: num  610284 610284 611591 594814 586871 ...
 $ Centroid_Y_UTMN: num  5178814 5178814 5179004 5181020 5203040 ...
 $ Centroid_Lat   : num  -122 -122 -122 -122 -122 ...
 $ Centroid_Long  : num  46.8 46.8 46.8 46.8 47 ...
 $ PRISM_ppt      : num  2017 2017 2017 3037 2171 ...
 $ PRISM_tmax     : num  11.7 11.7 11.7 7.92 8.8 ...
 $ PRISM_tmean    : num  6.58 6.58 6.58 3.59 4.49 ...
 $ PRISM_tmin     : num  1.454 1.454 1.454 -0.746 0.176 ...
 $ Site.Num       : int  6 6 7 200 140 183 233 232 198 199 ...
 $ Habitat        : Factor w/ 1 level "Lentic": 1 1 1 1 1 1 1 1 1 1 ...
 $ Sample         : Factor w/ 583 levels "Lentic1_1996",..: 14 14 19 456 331 420 571 560 447 451 ...
 $ Buffer_125m    : int  1 1 2 3 4 5 6 6 7 8 ...
 $ Buffer_250m    : int  1 1 2 3 4 5 6 6 7 8 ...
 $ Buffer_500m    : int  1 1 2 3 4 5 6 6 3 3 ...
 $ Buffer_1km     : int  1 1 1 2 3 4 5 5 2 2 ...

MORA.Lentic$Buffer_125m<-as.factor(MORA.Lentic$Buffer_125m) 
MORA.Lentic$Buffer_250m<-as.factor(MORA.Lentic$Buffer_250m)
MORA.Lentic$Buffer_500m<-as.factor(MORA.Lentic$Buffer_500m)
MORA.Lentic$Buffer_1km<-as.factor(MORA.Lentic$Buffer_1km)


#### Extracting climate data from PRISM by location ####
library(tidyr)
library(raster)
install_github(repo = "prism", username = "ropensci")
library(prism)

# http://eremrah.com/articles/How-to-extract-data-from-PRISM-raster/

# Get PRISM data (https://github.com/ropensci/prism)
# Parameter name	        Description
# tmean	                  Mean temperature
# tmax	                  Maximum temperature
# tmin	                  Minimum temperature
# ppt	                    Total precipitation (Rain and snow)

# Not sure if we want monthly means, or annual means. Daily is probably over-doing
# it at this point. Since the lentic (pond) surveys
# occur from about May-September, we could extract the monthly
# means for those months and then take the seasonal average. 
# For now, I will just get annual means...

options(prism.path = "~/prismtmp")
#get_prism_monthlys(type="tmean", year = 1984:2015, mon = 5:9, keepZip=F)
#get_prism_monthlys(type="tmax", year = 1984:2015, mon = 5:9, keepZip=F)
#get_prism_monthlys(type="tmin", year = 1984:2015, mon = 5:9, keepZip=F)
#get_prism_monthlys(type="ppt", year = 1984:2015, mon = 5:9, keepZip=F)
get_prism_annual(type="tmean", year = 1984:2015, keepZip=F)
get_prism_annual(type="tmax", year = 1984:2015, keepZip=F)
get_prism_annual(type="tmin", year = 1984:2015, keepZip=F)
get_prism_annual(type="ppt", year = 1984:2015, keepZip=F)

# have to delete all items in the folder "~/prismtmp" if you start over or 
# want different datasets
# or create a new folder

# Stack up the raster files together to get a raster stack 
mystack <- ls_prism_data() %>%  prism_stack(.)  # Stack files
mystack

# Convert our points into a spatial points data frame. 
# Be careful about projections! 

# First extract projection from raster stack and then convert points to 
# spatial points data frame using that projection.
# Get projection from raster stack
mycrs <- mystack@crs@projargs
mycrs
# [1] "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Convert UTM to lat long and project
library(rgdal)
# Prepare UTM coordinates matrix
utmcoor<-SpatialPoints(cbind(MORA.v2$Centroid_X_UTME, MORA.v2$Centroid_Y_UTMN), 
          proj4string=CRS("+proj=utm +zone=10")) # zone= UTM zone 10

# Convert to Lat/Long and project to same CRS as raster data
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat +datum=NAD83 +no_defs 
                                     +ellps=GRS80 +towgs84=0,0,0")) 
MORA.spatial <- SpatialPointsDataFrame(longlatcoor, MORA.v2) # attach new lat/long to dataframe
str(MORA.spatial) # currently a spatial data frame (contains info on projection, etc.)

# For other analyses, might be worth converting back to a normal dataframe...
MORA.v2<-as.data.frame(MORA.spatial) # convert back to normal data frame with 
                                      # new lat/long points (if desired!)
str(MORA.v2)
names(MORA.v2)[10] <- "Centroid_Lat" # rename coords.x1 (Latitude)
names(MORA.v2)[11] <- "Centroid_Long" # rename coords.x2 (Longitude)


# After adding projection to our spatial points data frame, now we can 
# extract values from the raster stack for our points 
# Extract data from raster by coordinates (and ObjectID):
Raster_dataframe <- data.frame(coordinates(MORA.spatial), MORA.spatial$ObjectID, 
                   extract(mystack, MORA.spatial))
View(Raster_dataframe)
str(Raster_dataframe)
# not sure why it renamed ObjectID, but that needs to remain the same so 
# we can merge this climate dataframe with the occurrence data frame
names(Raster_dataframe)[3] <- "ObjectID"
names(Raster_dataframe)[1] <- "Centroid_Lat" # rename coords.x1 (Latitude)
names(Raster_dataframe)[2] <- "Centroid_Long" # rename coords.x2 (Longitude)

## Merge Raster_dataframe (containing climate data) with MORA.v2 (occurrence data)
MORA.climate<-merge(x = MORA.v2, y = Raster_dataframe, by = "ObjectID", all = TRUE)
View(MORA.climate) # same number of rows, so that's good

# Export MORA.climate
write.csv(MORA.climate, "MORA_Amphibians_ClimateSeries_CSV_111516.csv")
## This works great, but each row (location with species observation) has all of
## the climate data for that location from 1984-2015. We don't need the entire
## time series for each row since each row is unique to a given year. 
## So let's try this again...




#### Extracting climate data from PRISM by year and location ####
## Subset MORA.v2 by year, then attach climate data for each year, then change
## the name of the climate variables so that they match across all datasets, then 
## merge the datasets back into one. We will end up with 4 columns containing
## each climate variable with values unique to that site and year.

## I ended up just doing it manually in excel to avoid fighting with R for now

MORA.climate2 <- read.csv("MORA_Amphibians_ClimateCondensed_CSV_111616.csv", header=T)
View(MORA.climate2)
str(MORA)
colnames(MORA)




#### Identifying sites surveyed Before/After 2000 ####

## Subset the dataframe into one df pre 2000 and one df post 2000 (incl. 2000)
# Remove "1993-1995"
MORA.v5<-Subset(MORA, !Year=="1993-1995") # include all levels except "1993-1995"
levels(MORA.v5$Year)
str(MORA.v5$Year)

# Convert Year from factor to date
MORA.v5$Year<-as.character(MORA.v5$Year)
MORA.v5$Year <- as.Date(MORA.v5$Year, "%Y")
MORA.v5$Year<- as.numeric(format(MORA.v5$Year,'%Y')) 
head(MORA.v5$Year)
View(MORA.v5)

# Subset df to pre and post 2000
str(MORA)
MORA.pre2000<-Subset(MORA.v5, Year < 2000) # before 2000
View(MORA.pre2000)

MORA.post2000<- Subset(MORA.v5, Year >= 2000) # after (and incl.) 2000
View(MORA.post2000)

## Compare pre and post 2000 dfs for Site/Species overlap
library(compare)
pre2000 <- MORA.pre2000[c(17,26)] # object 1 to compare
                                  # keep Species code, and Site ID
View(pre2000)

post2000 <- MORA.post2000[c(17,26)] # object 2 to compare
                                    # keep Species code, and Site ID
View(post2000)

library(dplyr)
Common<-semi_join(pre2000,post2000) # find common rows, alternatively...
                                    # anti_join finds different rows
View(Common) # repeated observations for speciesxsite combos
Common.v2<- unique(Common[,c('SpeciesCode','Unique_SiteID')])
# there are 258 species x site combinations that are sampled before AND after
# year 2000

Common.v3<- unique(Common["Unique_SiteID"])
View(Common.v3) # 140 sites sampled before AND after 2000

View(Common.v2)
library(xlsx)
write.xlsx(Common.v2, file = "Common_2000.xlsx")

## Which sites were sampled pre and post 2000 (can be used to determine if species
## were not detected at a site after 2000)
pre2000.unique1<-unique(pre2000[,c('SpeciesCode','Unique_SiteID')]) # sites x spp
View(pre2000.unique1) # 629 species x site combos
pre2000.unique2<-unique(pre2000["Unique_SiteID"]) # sites only
View(pre2000.unique2) # 265 sites sampled before 2000
                      # so, 125 sites were NOT revisited after 2000
write.xlsx(pre2000.unique1, file = "Pre2000_Unique.xlsx")

post2000.unique1<-unique(post2000[,c('SpeciesCode','Unique_SiteID')]) # sites x spp
View(post2000.unique1) # 802 species x site comboms
post2000.unique2<-unique(post2000["Unique_SiteID"]) # sites only
View(post2000.unique2) # 330 sites sampled after 2000
                       # so, 190 sites new to sampling after 2000
write.xlsx(post2000.unique1, file = "Post2000_Unique.xlsx")


MORA.uniquesites<-unique(MORA.v5["Unique_SiteID"]) ## 445 total unique sites



#### Subsetting the data ####

#### ... Into species groups based on breeding habitat ####
## Going to subset the dataframe into species-habitat groups (i.e. lentic-breeding spp,
## terrestrial species, lotic species) becuase of the confusion of "Mixed" sites.
## Some of these Mixed sites, for example, have both terrestrial and lentic-breeding 
## spp in the same "site", which is confusing since they probably don't interact they 
## just happened to be found together during the same survey. 
## Mostly just interested in the lentic-breeders.  
## Then, assign a new categorical variable to each of these subsetted dfs to 
## identify these new grouping. Recombine the dfs to reload into QGIS with new
## column identifing "BreedingHabitat".

levels(MORA.climate2$SpeciesCode)
[1] "AMGR" "AMMA" "AMSP" "ANBO" "ASTR" "DICO" "DISP" "DITE" "ENES" "PLLA" "PLSP"
[12] "PLVA" "PLVE" "PSRE" "RAAU" "RACA" "RASP" "TAGR"

## Lentic-breeding species 
MORA.lentic<-Subset(MORA.climate2, SpeciesCode=="AMGR" | SpeciesCode=="AMMA"| 
                      SpeciesCode=="ANBO" | SpeciesCode=="PSRE" | SpeciesCode=="RAAU"
                    | SpeciesCode=="RACA" | SpeciesCode=="TAGR")   

# create new column for Breeding Habitat 
# (to be used to join together each of these dfs)
MORA.lentic$BreedingHabitat <- "Lentic"
MORA.lentic$BreedingHabitat<-as.factor(MORA.lentic$BreedingHabitat)

str(MORA.lentic)
'data.frame':	5316 obs. of  10 variables:
 $ ObjectID       : int  91 93 211 213 321 518 541 877 900 1609 ...
 $ RecordID       : int  468 470 588 590 699 898 921 1273 1296 6881 ...
 $ Year           : Factor w/ 28 levels "1984","1985",..: 10 13 10 13 10 13 13 10 10 16 ...
 $ SpeciesCode    : Factor w/ 7 levels "AMGR","AMMA",..: 1 1 1 1 1 3 6 7 7 1 ...
 $ Genus          : Factor w/ 6 levels "ambystoma","Ambystoma",..: 2 2 2 2 2 3 5 6 6 2 ...
 $ Species        : Factor w/ 8 levels "aurora","boreas",..: 4 4 4 4 4 2 3 5 5 4 ...
 $ Unique_SiteID  : Factor w/ 324 levels "Lentic1","Lentic10",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Centroid_X_UTME: num  583348 583348 583348 583348 583348 ...
 $ Centroid_Y_UTMN: num  5192838 5192838 5192838 5192838 5192838 ...
 $ BreedingHabitat: Factor w/ 1 level "Lentic": 1 1 1 1 1 1 1 1 1 1 ...


## Terrestrial-breeding species
MORA.terr<-Subset(MORA.climate2, SpeciesCode=="ENES" | SpeciesCode=="PLLA"| 
                      SpeciesCode=="PLVA" | SpeciesCode=="PLVE")   

# create new column for Breeding Habitat 
# (to be used to join together each of these dfs)
MORA.terr$BreedingHabitat <- "Terrestrial"
MORA.terr$BreedingHabitat<-as.factor(MORA.terr$BreedingHabitat)

str(MORA.terr)

'data.frame':	551 obs. of  10 variables:
 $ ObjectID       : int  2535 3217 3218 3219 2773 2356 2343 2344 2345 2346 ...
 $ RecordID       : int  7834 69998595 69998599 69998600 8635 7654 7641 7642 7643 7644 ...
 $ Year           : Factor w/ 19 levels "1984","1985",..: 9 12 13 13 14 6 6 6 6 6 ...
 $ SpeciesCode    : Factor w/ 4 levels "ENES","PLLA",..: 1 3 3 3 4 3 3 3 3 3 ...
 $ Genus          : Factor w/ 2 levels "Ensatina","Plethodon": 1 2 2 2 2 2 2 2 2 2 ...
 $ Species        : Factor w/ 4 levels "eschscholtzii",..: 1 3 3 3 4 3 3 3 3 3 ...
 $ Unique_SiteID  : Factor w/ 67 levels "Lentic230","Mixed1",..: 36 1 1 1 8 3 4 4 4 4 ...
 $ Centroid_X_UTME: num  610579 585479 585479 585479 583118 ...
 $ Centroid_Y_UTMN: num  5180101 5200599 5200599 5200599 5176677 ...
 $ BreedingHabitat: Factor w/ 1 level "Terrestrial": 1 1 1 1 1 1 1 1 1 1 ...


## Lotic-breeding species
MORA.lotic<-Subset(MORA.climate2, SpeciesCode=="ASTR" | SpeciesCode=="DICO"| 
                      SpeciesCode=="DITE") 

# create new column for Breeding Habitat 
# (to be used to join together each of these dfs)
MORA.lotic$BreedingHabitat <- "Lotic"
MORA.lotic$BreedingHabitat<-as.factor(MORA.lotic$BreedingHabitat)


str(MORA.lotic)
'data.frame':	627 obs. of  10 variables:
 $ ObjectID       : int  2054 3130 5720 2130 2131 2913 4388 4390 2986 976 ...
 $ RecordID       : int  7340 69998508 70002046 7427 7428 69998197 70000664 70000666 69998272 1392 ...
 $ Year           : Factor w/ 18 levels "1984","1985",..: 11 14 17 7 7 13 16 16 13 5 ...
 $ SpeciesCode    : Factor w/ 3 levels "ASTR","DICO",..: 1 1 1 1 1 1 1 1 3 1 ...
 $ Genus          : Factor w/ 2 levels "Ascaphus","Dicamptodon": 1 1 1 1 1 1 1 1 2 1 ...
 $ Species        : Factor w/ 3 levels "copei","tenebrosus",..: 3 3 3 3 3 3 3 3 2 3 ...
 $ Unique_SiteID  : Factor w/ 137 levels "Lentic131","Lentic133",..: 1 2 2 3 3 4 4 4 57 5 ...
 $ Centroid_X_UTME: num  611775 613086 613086 612493 612493 ...
 $ Centroid_Y_UTMN: num  5190963 5191533 5191533 5193684 5193684 ...
 $ BreedingHabitat: Factor w/ 1 level "Lotic": 1 1 1 1 1 1 1 1 1 1 ...


## Recombine all dfs
MORA.v6 <- rbind(MORA.lentic, MORA.terr, MORA.lotic)
head(MORA.v6)

write.csv(MORA.v6, "MORA_Amphibians_joinedV2_CSV_092016.csv")

#### ... Aggregated across habitats, but separated by year ####
## This section will determine how many sites were sampled each year and which
## sites were repeatedly sampled (and how often) over the course of the study

levels(MORA.v2$Year)
 [1] "1984"      "1985"      "1986"      "1991"      "1992"      "1993"     
 [7] "1993-1995" "1994"      "1995"      "1996"      "1997"      "1998"     
[13] "1999"      "2000"      "2001"      "2002"      "2003"      "2004"     
[19] "2005"      "2006"      "2007"      "2008"      "2009"      "2010"     
[25] "2011"      "2012"      "2013"      "2014"      "2015"  
# apparently there is no data from 1987-1990 :(


library(plyr)
## How many observations per year?
ddply(MORA.v2, .(Year), summarize, no_rows=length(Year), .drop=FALSE)
# 1993-1995 has 21 rows 
# We should just remove (or ignore) this level for the purposes of 
# subsetting by year
# Otherwise it looks like every year is sampled at least once from 1984-2015

## Are any of the sites repeatedly sampled?
detach("package:FSA", unload=TRUE) 
detach("package:plyr", unload=TRUE) # packages conflict with parts of dplyr
library(dplyr)
byYearSite <- group_by(MORA.v2, Unique_SiteID, Year )
sumYearSite <- summarise(byYearSite, count=n())
View(sumYearSite)
# 944 site x year combinations
# getting close

# find unique Site/Year combos - those will be the sites that are only sampled once
sumYearSite<-as.data.frame(sumYearSite)
single_obs<- unique(sumYearSite["Unique_SiteID"])
View(single_obs)

repeated_visits <-  sumYearSite %>% 
                        group_by(Unique_SiteID) %>% 
                            filter(n()>1)
View(repeated_visits)
# 708 (out of 944) observations. 
# So of the 445 sites, 236 sites were only sampled once

single_visits <-  sumYearSite %>% 
                        group_by(Unique_SiteID) %>% 
                            filter(n()<2)
View(single_visits) # list of sites visited only once
single_visits$Unique_SiteID


unique_sites<- unique(repeated_visits["Unique_SiteID"])
# 209 repeatedly sampled sites 
 
counts_bysite <-  repeated_visits %>% 
                              group_by(Unique_SiteID) %>% 
                                    summarise(count=n())  
View(counts_bysite)
# Sites with visits in the double digits: 
    # Lentic105 = Laughingwater Creek Pond (11 years)
    # Lentic133 = Tipsoo Lake (11 years) not including little pond
    # Lentic 93 = Upper Marsh Lakes (11 years)

# shows how many times a site was visited. Looks like most sites only visited 
# 2 or 3 times. Could be useful if these visits were spread out over time.
# I should repeat this analysis for the HOOD data to see if Char has more
# temporal consistency. Looks like MORA data will only be useful for spatial 
# analyses.
# Should determine if certain regions were sampled at different times over the 
# course of the study. For instance, if the NW portion of the park was only 
# surveyed during the late 90's. 
# Since there is little temporal consistency, if I restricted the data to the 
# last 10 years, would I lose a large marjority of the sites?
# Alternative idea: Broaden the spatial extent of what is considered a "site" 
# Could look at making more "regional" comparisons or local metapopulations -- 
# Marsh Lakes and Tipsoo are good examples of when this would be useful/accurate 

counts_byyear <-  repeated_visits %>% 
                              group_by(Year) %>% 
                                    summarise(count=n()) 
View(counts_byyear)


## Subset dataframe into multiple dataframes by year
# rename 1993-1995
levels(MORA.v2$Year)[levels(MORA.v2$Year)=="1993-1995"] <- "199395"

MORA.Year.subsets<-split(MORA.v2, factor(MORA.v2$Year), drop=TRUE) 
                                                     # split dataframe by Years
                                                     # into subsetted dataframes
# Now the data is in an array of dataframes

View(MORA.Year.subsets[[1]]) # Subset 1 = 1984

## Loop the naming of the new dataframes
new_names <- as.character(levels(MORA.v2$Year))
for (i in 1:length(MORA.Year.subsets)) {
  assign(paste("MORA.",new_names[i], sep=""), MORA.Year.subsets[[i]])
}
View(MORA.1984) # for example
str(MORA.1984)
# why is it still showing all levels of Year even though we specified drop=TRUE?
# when I run functions using the subset dataframes it only recognizes the 
# factor as having one level (i.e. the year it was subset to). I don't understand!

levels(MORA.1984$Year)
levels(factor(MORA.1984$Year))
[1] "1984"
# So you have to reapply factor() to all of the factors in order for them to no 
# longer contain unused levels?!

MORA.1985<-drop.levels(MORA.1985)
str(MORA.1985)
# fixed! Now how do I do this automatically across all of the dataframes?

df.list <- list(MORA.1984, MORA.1985, MORA.1986, MORA.1991, MORA.1992, MORA.1993,  
                MORA.199395, MORA.1994, MORA.1995, MORA.1996, MORA.1997, MORA.1998, 
                MORA.1999, MORA.2000, MORA.2001, MORA.2002, MORA.2003, MORA.2004, 
                MORA.2005, MORA.2006, MORA.2007, MORA.2008, MORA.2009, MORA.2010, 
                MORA.2011, MORA.2012, MORA.2013, MORA.2014, MORA.2015)

lapply(df.list, function(x) drop.levels(x)) # why isn't this working!?
lapply(MORA.Year.subsets, function(x) drop.levels(x)) # why isn't this working!?
str(MORA.1994)
lapply(df.list, function(x) if(is.factor(x)) factor(x) else x) # why isn't this working!?
lapply(df.list, function(x) as.factor(x$Year)) # why isn't this working!?
lapply(df.list, function(x) drop.levels(x$Year)) # why isn't this working!

# I'll just do it manually *eye roll*
MORA.1984<-drop.levels(MORA.1984); MORA.1985<-drop.levels(MORA.1985)
MORA.1986<-drop.levels(MORA.1986); MORA.1991<-drop.levels(MORA.1991)
MORA.1992<-drop.levels(MORA.1992); MORA.1993<-drop.levels(MORA.1993)
MORA.199395<-drop.levels(MORA.199395); MORA.1994<-drop.levels(MORA.1994)
MORA.1995<-drop.levels(MORA.1995); MORA.1996<-drop.levels(MORA.1996)
MORA.1997<-drop.levels(MORA.1997); MORA.1998<-drop.levels(MORA.1998)
MORA.1999<-drop.levels(MORA.1999); MORA.2000<-drop.levels(MORA.2000)
MORA.2001<-drop.levels(MORA.2001); MORA.2002<-drop.levels(MORA.2002)
MORA.2003<-drop.levels(MORA.2003); MORA.2004<-drop.levels(MORA.2004)
MORA.2005<-drop.levels(MORA.2005); MORA.2006<-drop.levels(MORA.2006)
MORA.2007<-drop.levels(MORA.2007); MORA.2008<-drop.levels(MORA.2008)
MORA.2009<-drop.levels(MORA.2009); MORA.2010<-drop.levels(MORA.2010)
MORA.2011<-drop.levels(MORA.2011); MORA.2012<-drop.levels(MORA.2012)
MORA.2013<-drop.levels(MORA.2013); MORA.2014<-drop.levels(MORA.2014)
MORA.2015<-drop.levels(MORA.2015)





#### ... By Species (e.g. ANBO) ####
# ANBO = Anaxyrus boreas = Western Toad

MORA.ANBO <- Subset(MORA.v5, SpeciesCode=="ANBO")

ANBO.sites<-unique(MORA.ANBO["Unique_SiteID"]) ## found at 22 sites

## Subset the dataframes from the previous section (MORA.v5 -> MORA.pre2000 & 
## MORA.post2000) by species = ANBO 
MORA.pre2000.ANBO<-Subset(MORA.pre2000, SpeciesCode == "ANBO") # ANBO before 2000
View(MORA.pre2000.ANBO) # 12 observations

pre2000.ANBO <- MORA.pre2000.ANBO[c(26)] # object 1 to compare
                                           # keep just Site ID
View(pre2000.ANBO)

MORA.post2000.ANBO<-Subset(MORA.post2000, SpeciesCode == "ANBO") # ANBO after (&incl.) 2000
View(MORA.post2000.ANBO) # 217 observations

post2000.ANBO <- MORA.post2000.ANBO[c(26)] # object 2 to compare
                                             # keep just Site ID
View(post2000.ANBO)

library(dplyr)
Common<-semi_join(pre2000.ANBO,post2000.ANBO) # find common rows, alternatively...
                                              # anti_join finds different rows
View(Common) # revisited sites
Common.v2<- unique(Common['Unique_SiteID'])
View(Common.v2)
# there are 4 sites surveyed before AND after 2000 for this species (out of a 
# total of 22 sites)


pre2000.ANBO.sites<-unique(pre2000.ANBO["Unique_SiteID"]) # sites only
View(pre2000.ANBO.sites) # 10 sites, 4 of which were revisited after 2000
# revisited meaning ANBO was detected at these sites after 2000

pre2000.ANBO.notrevisited<-anti_join(pre2000.ANBO.sites, Common.v2)
View(pre2000.ANBO.notrevisited) # these are the 6 sites that were NOT revisited

# Compare the above list of pre-2000 sites that were NOT revisited after 2000 to the overall
# list of sites post 2000 to see if ANBO disappeared from any sites
ANBO.disappear<-semi_join(pre2000.ANBO.notrevisited, post2000.unique2)
View(ANBO.disappear)
# There were 6 sites where ANBO was detected before 2000 but NOT after 2000
# confirmed that these sites were revisited (for other species) after 2000



#### ... Aggregated across all years, but separated by habitat ####

## Dataframe for each habitat type (Lentic, Lotic, Terrestrial, Mixed) representing 
## presence and absence of each species across all years. This is what we will use 
## for the first round of analyses (specifically looking at just Lentic species)

## Return to MORA.climate2 dataframe. Need to split column 7 ("Unique_SiteID) into 
## seperate columns containing habitat type and a unique number. 
## First step is to give each unique factor level (i.e. site id) a number.
## Second step is to extract the habitat part of the site id. 

## Assign unique number to each level of the factor: Unique_SiteID
# Each site will have a unique ID
MORA.v4 <- transform(MORA.climate2, Site.Num=as.numeric(factor(Unique_SiteID)))
View(MORA.v4)
str(MORA.v4$Site.Num)

## Extract the habitat designation from Unique_SiteID
library(stringr)
MORA.v4$Habitat <- (str_extract(MORA.v4$Unique_SiteID, "[aA-zZ]+"))
View(MORA.v4)
str(MORA.v4) # reads Habitat as a character variable
MORA.v4$Habitat<- as.factor(MORA.v4$Habitat) # change to factor variable
levels(MORA.v4$Habitat)
[1] "Lentic"      "Lotic"       "Mixed"       "Terrestrial"

## Subset the dataframe by Habitat
MORA.Lentic<-Subset(MORA.v4, Habitat=="Lentic") # Lentic
str(MORA.Lentic)
# 13 of 15 spp?? 
levels(MORA.Lentic$Species)
# eschscholtzii, tenebrosus, and vandykei should probably not be considered lentic
# Manually changed a few things in the excel file:
# eschscholtzii - Obj ID: 2535, Record ID: 7834 
    # Polygon changed from Lentic104 to Mixed97 
# tenebrosus - changed polygons to "Mixed#" when appropriate
# vandykei - left as Lentic, found on shoreline of lake
# Reload data
levels(MORA.Lentic$Species)
 [1] "aurora"        "boreas"        "cascadae"      "gracile"      
 [5] "granulosa"     "macrodactylum" "regilla"       "spp."         
 [9] "tenebrosus"    "truei"         "vandykei" 

# I'll go ahead and remove "spp." too
MORA.Lentic<-Subset(MORA.Lentic, !Species=="spp.") # include all species except ".spp"
levels(MORA.Lentic$Species)
str(MORA.Lentic$Species)
View(MORA.Lentic)

# Create a new variables that merges "Unique_SiteID" and "Year" so that we don't
# artifically inflate co-occurrences. Species A and B could co-occur at Site 1, but
# if they occur at different years then that is meaningful. So having Site1_1990 
# and Site1_1995 might be more indicative of their co-occurrence patterns. 
MORA.Lentic$Sample <- paste(MORA.Lentic$Unique_SiteID, MORA.Lentic$Year, sep="_")

# export to csv
write.csv(MORA.Lentic, "MORA_Amphibians_Lentic_CSV_111716.csv", row.names=FALSE)



# In case we want the observations in other habitats...
MORA.Lotic <-Subset(MORA.v4, Habitat=="Lotic") # Lotic
str(MORA.Lotic)
levels(MORA.Lotic$Species)
# Plethodon species should not be considered lotic
# vandykei - changed all Lotic to Mixed
# vehiculum - changed Lotic to Mixed
# Reload data
levels(MORA.Lotic$Species)
[1] "aurora"     "cascadae"   "copei"      "gracile"    "regilla"    "spp."      
[7] "tenebrosus" "truei"    

MORA.Terr<-Subset(MORA.v4, Habitat=="Terrestrial") # Terrestrial
str(MORA.Terr)

MORA.Mixed<-Subset(MORA.v4, Habitat=="Mixed") # Mixed
str(MORA.Mixed)



## Now we can create community matrices for each of the habitat types... 


#### ...  ... Lentic community matrix ***####
View(MORA.Lentic) # see previous section on how this dataframe was subset & created
                  # This file can be loaded directly into R instead of going through
                  # those steps (see section: "Load Lentic amphibian co-occurrence data")

## Step 1: reduce the dataframe to just columns of Sample and SpeciesCode
## columns 4 and 18 of 18
MORA.Lentic2 <- MORA.Lentic[ , -c(1:3,5:17) ] # 4738 observations (all samples)
View(MORA.Lentic2)
colnames(MORA.Lentic2)
[1] "SpeciesCode" "Sample" 

## Step 2: make the boolean matrix   
library(reshape2)

w <- melt(MORA.Lentic2) # 4738 observations (all samples), doesn't really change anything
w <- dcast(w, Sample~SpeciesCode) # 583 samples (aggregated)
w[is.na(w)] <- 0        # assigns all NA values a 0
w2 <- cbind(apply(w[,2:8], 2, function(w) as.numeric(w > 0)), w[1])  
                                            # 7 species columns
                                            # recode as 0/1
                                            # applies function only to numeric columns
                                            # binds first column (Sample) 
MORA.Len.matrix <- w2[,c(8, 1:7)]  # rearrange so that site column is first
                                 # rename 
View(MORA.Len.matrix)
head(MORA.Len.matrix)
         Sample AMGR AMMA ANBO PSRE RAAU RACA TAGR
1  Lentic1_1996    1    0    0    0    0    0    1
2  Lentic1_1999    1    0    1    0    0    1    0
3  Lentic1_2002    1    1    0    1    0    1    0
4  Lentic1_2010    1    0    0    0    0    1    0
5 Lentic10_1992    0    0    0    0    0    1    0
6 Lentic10_1996    0    0    0    1    0    1    0

#### ... ... Environmental data*** ####
# load environmental data, if you want to "correct for the effect of the environment"
# data object "ENV" should have sites in rows, columns are the environmental variables
# at that site (numeric)
ENV <- MORA.Lentic[ , -c(1:11,16,17) ] # climate data and Sample (unique site-year combo)
ENV <- melt(ENV)
ENV <- dcast(ENV, Sample~variable, fun.aggregate = mean) # multiple observations per sample
                                                          # i.e. same ppt value for multiple
                                                          # observations (samples) per site
                                                          # values are the same, so taking
                                                          # mean works perfect to just 
                                                          # reduce it to one value per sample
View(ENV) # 583 samples

#### Determining sampling frequency ####
## This section will determine how often species were sampled AND how 
## many sites were sampled each year AND which sites were repeatedly 
## sampled (and how often) over the course of the study

# Sum of # of sites where each species was observed (at least once)
MORA.Lentic3 <- MORA.Lentic[ , -c(1:3,5:6,8:18) ] # need just SiteCode and SpeciesCod
x <- melt(MORA.Lentic3) # all observations,  doesn't really change anything
x <- dcast(x, Unique_SiteID~SpeciesCode) # 235 sites
x[is.na(x)] <- 0        # assigns all NA values a 0
x2 <- cbind(apply(x[,2:8], 2, function(x) as.numeric(x > 0)), x[1])  
                                            # 7 species columns
                                            # recode as 0/1
                                            # applies function only to numeric columns
                                            # binds first column (Site) 
colSums(x2[,c(1:7)]) # sum species presence/absence columns to get
                    # frequencies of observations per site (235 sites total)
AMGR AMMA ANBO PSRE RAAU RACA TAGR 
 169  112   19   33   13  212   45 

#### Vizualizing co-occurrence ####

X <- MORA.Len.matrix[ , -1 ] 
total_occurrences <- colSums(X)
total_occurrences
AMGR AMMA ANBO PSRE RAAU RACA TAGR 
 407  165   50   60   20  455   74 
data_matrix <- as.matrix(X)
co_occurrence <- t(data_matrix) %*% data_matrix # transpose the matrix
co_occurrence
     AMGR AMMA ANBO PSRE RAAU RACA TAGR
AMGR  407  110   39   45   17  304   67
AMMA  110  165   15   17    3  128   18
ANBO   39   15   50   10    3   39    9
PSRE   45   17   10   60    9   38   21
RAAU   17    3    3    9   20    6    9
RACA  304  128   39   38    6  455   47
TAGR   67   18    9   21    9   47   74

# to convert this matrix to proportions, have to calculate
# their joint proportions of co-occurrence (i.e. the proportion of shared samples out of
# total possible samples for each species pair). 
# e.g. AMGR and AMMA 
# AMGR-only occurrences:
407-110 # total AMGR occurrences minus their shared co-occurrence 
# AMMA-only occurrences:
165-110 # total AMGR occurrences minus their shared co-occurrence 
# where AMGR and AMMA co-occur
110
# joint probability of co-occurrence
110/((407-110)+(165-110)+110)
# or...
110/(407+165-110) # 0.2380952

# now do this automatically and create a joint proportion of occurrence matrix

## co-occurrence as a joint proportion between each species pair...
cooccurr_prop <-  matrix(, nrow = 7, ncol = 7) # create an empty 7x7 matrix

for (j in 1:7) { 
    for (i in 1:7) {
  cooccurr_prop[i,j] <- co_occurrence[i,j]/(co_occurrence[i,i] + 
                        co_occurrence[j,j] - co_occurrence[i,j]) 
}}
colnames(cooccurr_prop)<-colnames(co_occurrence)
rownames(cooccurr_prop)<-colnames(co_occurrence)
print(cooccurr_prop)


plot.new()
pdf(file="mora_cooccurrence.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot from method "A"
g<-graph.adjacency(cooccurr_prop, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*5
E(g)[E(g)$weight>0]$color<-"black" # all species co-occur at least once
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
#mtext(text = "Raw co-occurrences",side = 3,line = 2, cex=1.5) # this line controls how big the arrow is
dev.off()



#### Co-occurrence Analyses ####
X <- MORA.Len.matrix[ , -1 ] # remove Sample (factor) column

#### ... Odds ratio (Lane et al. 2014) method ####
install_github('mjwestgate/sppairs')
library(sppairs)
# note that this is the only method that creates "directed" links among species


#### ... ... without environ correction ####

# 'clean' data as per lane et al. recommendation
# (no very rare or very abundant species)
X.clean<-clean.dataset(X, cutoff.min=0.1, cutoff.max=0.95)
dim(X);dim(X.clean)
# got rid of 2 out of 7 spp: ANBO and RAAU (unexpectedly rare)

# calculate odds ratio from logistic regression
lne.pr<-spaa(X.clean, method="or.glm")

# this is the matrx of significant pairs, though unclear how
# significance was determined.
# in lane et al. 2014, they give a cutoff for "significance", but not
# discussed in documentation & difficult to discern from the code
# for spaa()
lne.pr
# value = odds ratio, degree of association
    sp1  sp2     value
1  AMGR AMMA 0.9382716 # Salamander predators/competitors (neg) ** studies show niche partitioning/allopatry
2  AMGR PSRE 1.0835635 # Salamander predator and Anuran prey (pos)
3  AMGR RACA 0.8302998 # Salamander predator and Anuran prey (neg)
4  AMGR TAGR 1.3554451 # Salamander predators (pos)
5  AMMA PSRE 1.0012387 # Salamander predator and Anuran prey (pos) ** my experimental data
6  AMMA RACA 0.9732106 # Salamander predator and Anuran prey (neg) ** my experimental data
7  AMMA TAGR 0.8422504 # Salamander predators (neg)
8  PSRE RACA 0.4859141 # Anuran competitors (neg) ** my experimental data - signif. competitors
9  PSRE TAGR 3.7037422 # Anuran prey and Salamander predator (pos)
10 RACA TAGR 0.7923622 # Anuran prey and Salamander predator (neg)

# how many significant pairs?
(sigpair.lne<-nrow(lne.pr))
[1] 10 # so all are significant!?
# which pairs are positive associations?
which(lne.pr$value > 1)
[1] 2 4 5 9
# what proportion are positive? 
length(which(lne.pr$value > 1))/sigpair.lne
[1] 0.4
# which pairs are negative associations?
which(lne.pr$value < 1)
[1]  1  3  6  7  8 10
# what proportion are negative? 
length(which(lne.pr$value < 1))/sigpair.lne
[1] 0.6

## Note: Lane et al suggest that an odds ratio > or < 3 (rather than 1) 
## represents an "ecologically substantial association" 
## In this case, the only *significantly* positive or "indicated" association 
## is between PSRE and TAGR

# make a full association ("A") matrix with significant & nonsig assoc
ncol(X) 
# how many unique species are listed in the pairwise list?
lnestk<-data.frame()
lnestk[1:nrow(lne.pr),1]<-lne.pr$sp1
lnestk[(nrow(lne.pr)+1):((nrow(lne.pr)+1)+(nrow(lne.pr))-1),1]<-lne.pr$sp2
length(unique(lnestk$V1))
[1] 5
# need to add on extra species that weren't significant
length(which(colnames(X) %in% unique(lnestk$V1)==FALSE))
colnames(X)[which(colnames(X) %in% unique(lnestk$V1)==FALSE)]
lne.pr.df<-data.frame(lne.pr$sp1,lne.pr$sp2, lne.pr$value, stringsAsFactors = FALSE)
#  add on NA values
lne.pr.df[(nrow(lne.pr.df)+1):
             (nrow(lne.pr.df)+length(which(colnames(X) %in% unique(lnestk$V1)==FALSE))),
           1:2]<- cbind(colnames(X)[which(colnames(X) %in% unique(lnestk$V1)==FALSE)],
                        rev(colnames(X)[which(colnames(X) %in% unique(lnestk$V1)==FALSE)]))

# for directed links (e.g., lane et al. 2014 method)
list2mat_dir<-function (dat){ 
  dat.name1 <- as.character(dat[, 1])
  dat.name2 <- as.character(dat[, 2])
  dat.value <- dat[, 3]
  names1 <- sort(unique(as.character(dat[, 1])))
  names2 <- sort(unique(as.character(dat[, 2])))
  total.names <- unique(c(names1, names2))
  elements <- rep(NA, length(total.names)^2)
  dim(elements) <- c(length(total.names), length(total.names))
  rownames(elements) <- total.names
  colnames(elements) <- total.names
  for (k in 1:length(dat.name1)) {
    elements[dat.name1[k], dat.name2[k]] <- dat.value[k]}
  return(elements)
}

lne.A<-as.matrix(list2mat_dir(lne.pr.df))
dim(lne.A)
# replace NAs with 0s
lne.A[is.na(lne.A)]<-0 

# check: is the A matrix symmetric? (it shouldn't be)
isSymmetric(lne.A) # no

View(lne.A)

plot.new()
pdf(file="oddsratio_method.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot from method "B"
g<-graph.adjacency(lne.A, mode="directed",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<1]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>1]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.75) # this line controls how big the arrow is
mtext(text = "Odds Ratio",side = 3,line = 2, cex=1.5)
dev.off()

#### ... ... with environ correction ####

# use mixed effects model - can only add random effects
# but this works here because we are only accounting
# for the effect of the environment, we don't necessarily
# want to know what that effect is

# this method allows only one random effect (factor)
# my idea: use cluster analysis to detect significant differences
# among environmental characteristics
install.packages(c('cluster','fpc','clustsig'))
library(clustsig)       # to run the simprof() clustering function
library(cluster)
library(fpc)

# run cluster analysis
alpha <- 0.05

ENV <- ENV[ , -1 ] # remove Sample (factor) column
View(ENV)
res <- simprof(data=ENV, method.distance="braycurtis", alpha=alpha)
# note: ^ this runs for a long time (~3.5 hours)
# res shows the samples and where they fall into the various clusters

# quickly plot
pl.color <- simprof.plot(res, plot=T)
# 'dendrogram' with 2 branches and 584 members total, at height 20.90772 
plot (pl.color, ylab="Bray-Curtis dissimilarity (%)")
# different colors are significant (at alpha 0.05) differences in env conditions

# how many clusters of environmental variables detected?
res$numgroups
#[1] 227 
# so there are 227 clusters

# convert to a factor list
sigclus<-ldply(res$significantclusters, data.frame)
head(sigclus)
clusleng<-ldply(lapply(res$significantclusters, length), data.frame) 
View(clusleng)
# 77 of the 229 clusters only have 1 sample (observation), which won't work 
# at the level of a random effect, so they have to be combined. 

require(dendextend)
# calculate lowest value of h in cut that produces groupings of a 
# given minimum size
dend<-as.dendrogram(res$hclust)
heights_per_k.dendrogram(dend)
# we know that 277 clusters results in many clusters with singletons. The height 
# at k=277 clusters is 4.980546e-02

res_cut<-cutree(res$hclust, h=4.443017e-01) # cut tree to give 100 clusters 
res_cut # gives cluster assignment for each sample
res_cut<-as.factor(res_cut) # convert cluster list to a factor
cut.table = table(res_cut) # convert to a table, 
                            # which will give frequencies for each level
cut.table
cut.table<-as.data.frame(cut.table) # convert to data_frame
names(cut.table)[1] = 'cluster' # 1st column renamed cluster
View(cut.table)
# still have 20 clusters with singletons

# repeated the above code and incrementally reduced the number of clusters 
# down to 18 before there were no more singletons
res_cut<-dendextend::cutree(res$hclust, k=18) # h=2.516257
# res_cut shows the cluster that each sample assigned to (from 1 to 18)

# view this clustering on the dendrogram
res$hclust <- color_branches(res$hclust, k = 18) 
res$hclust <- color_labels(res$hclust, k = 18)
plot(res$hclust)

# convert to a factor list
sigclus<-ldply(res_cut, data.frame)
View(sigclus)
names(sigclus)[2] = 'cluster' # 1st column is the cluster


# calculate odds ratio from logistic regression
lnee.pr<-spaa(X.clean, method="or.glmer", random.effect=sigclus$cluster)
# no warnings

lnee.pr
    sp1  sp2     value
1  AMGR AMMA 0.9342042
2  AMGR PSRE 1.0658023
3  AMGR RACA 0.8280683
4  AMGR TAGR 1.3554452
5  AMMA PSRE 1.0808899
6  AMMA RACA 0.9808454
7  AMMA TAGR 0.8452400
8  PSRE RACA 0.4897520
9  PSRE TAGR 3.7037421
10 RACA TAGR 0.7894247

# how many significant pairs?
(sigpair.lne<-nrow(lnee.pr))
[1] 10 #  all are sig
# which pairs are positive associations?
which(lne.pr$value > 1)
[1] 2 4 5 9
# what proportion are positive? 
length(which(lne.pr$value > 1))/sigpair.lne
[1] 0.4
# which pairs are negative associations?
which(lne.pr$value < 1)
[1]  1  3  6  7  8 10
# what proportion are negative? 
length(which(lne.pr$value < 1))/sigpair.lne
[1] 0.6


# make a full association ("A") matrix with significant & nonsig assoc
ncol(X) 
# how many unique species are listed in the pairwise list?
lnestk<-data.frame()
lnestk[1:nrow(lnee.pr),1]<-lnee.pr$sp1
lnestk[(nrow(lnee.pr)+1):((nrow(lnee.pr)+1)+(nrow(lnee.pr))-1),1]<-lnee.pr$sp2
length(unique(lnestk$V1))
[1] 5
# need to add on extra species that weren't significant
length(which(colnames(X) %in% unique(lnestk$V1)==FALSE))
colnames(X)[which(colnames(X) %in% unique(lnestk$V1)==FALSE)]
lnee.pr.df<-data.frame(lnee.pr$sp1,lnee.pr$sp2, lnee.pr$value, stringsAsFactors = FALSE)
#  add on NA values
lnee.pr.df[(nrow(lnee.pr.df)+1):
             (nrow(lnee.pr.df)+length(which(colnames(X) %in% unique(lnestk$V1)==FALSE))),
           1:2]<- cbind(colnames(X)[which(colnames(X) %in% unique(lnestk$V1)==FALSE)],
                        rev(colnames(X)[which(colnames(X) %in% unique(lnestk$V1)==FALSE)]))

lne.B<-as.matrix(list2mat_dir(lnee.pr.df))
dim(lne.B)
# replace NAs with 0s
lne.B[is.na(lne.B)]<-0 

# check: is the A matrix symmetric? (it shouldn't be)
isSymmetric(lne.B) # no

lne.B

plot.new()
pdf(file="oddsratio_method.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot from method "B"
g<-graph.adjacency(lne.B, mode="directed",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<1]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>1]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.75) # this line controls how big the arrow is
mtext(text = "Odds Ratio (environmental correction)",side = 3,line = 2, cex=1.5)
dev.off()




##### ... Partial correlation (Harris 2016) method #####

install.packages('progress','corpcor')
library(progress)           # required to load rosalia package
library(corpcor)            # for regularized partial covariance

devtools::install_github("davharris/rosalia")
library(rosalia)            # harris 2015 method

# function rosalia() developed in harris 2015 (bioRxiv) is the
# best-performing method to evaluate "true" direct interactions
# (rather than estimating net effects), with partial covariance
# as the second best-performing method for - for speciose (S > 20)
# communities of intermediate sample size (N = ~100), which
# best matches the form of our community data

# community matrix
X <- MORA.Len.matrix[ , -1 ] # remove "Sample" column

# however, rosalia() does not work well for communities with > 20 nodes
# because there are 2^S terms for S nodes
2^ncol(X)
# 128

# run rosalia function (with no prior)
hros <- rosalia(X)
# ^ runs very quickly

# returns 'interaction strengths'
hros.A<-hros$beta

colnames(hros.A)<-colnames(X)
rownames(hros.A)<-colnames(X)

View(hros.A)

plot.new()
pdf(file="harris_method.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot from method "B"
g<-graph.adjacency(hros.A, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "Markov Network",side = 3,line = 2, cex=1.5)
dev.off()



#### ... Inverse covariance ####

# from harris (2015 preprint): 
# "For cases where fitting a Markov network is computationally infeasible,
# these results also indicate that partial covariances...can often provide
# a surprisingly useful approximation"
# - to increase their reliability, harris (2015) suggests adding a 
# guild structure as a prior distribution on the parameters

# two ways to calculate partial correlation
pcva <- pcor.shrink(X) # also computes correlation shrinkage intensity 
# Estimating optimal shrinkage intensity lambda (correlation matrix): 0.1801
pcvb <- cor2pcor(cor(X)) # slower than pcor.shrink

# note that the blonder & morueta-holme package 'netassoc' above
# calculates partial correlation to estimate associations, using 
# corpcor::invcov.shrink() (see ?partial_correlation) to calculate the
# inverse covariance matrix, which is then used to estimate partial
# correlations, whereas corpcor::cor2pcor() uses the pseudoinverse()
# function to invert the covariance matrix

colnames(pcvb)<-colnames(X)
rownames(pcvb)<-colnames(X)

View(pcvb)

plot.new()
pdf(file="inversecov_method.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot from method "B"
g<-graph.adjacency(pcvb, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "Inverse Covariance",side = 3,line = 2, cex=1.5)
dev.off()



#### ... JSDM ('bayescomm') ####

# there are a lot of other methods coming online
# hui, for example, has a bayesian r package - explained here: http://doi.wiley.com/10.1111/2041-210X.12514
install.packages('boral')
library(boral) # maybe for another time though...

# bayescomm - from golding & harris
install.packages('BayesComm')
library(BayesComm)

# notes about running the BC() function :
  # you cannot have any NA rows or columns
  which(is.na(X))
  # you cannot have any empty rows or columns (no empty samples or species)
  which(apply(X = X, MARGIN = 2, FUN = sum)==0)
  which(apply(X = X, MARGIN = 1, FUN = sum)==0)
  # you cannot have columns of all 1s
  which(apply(X = X, MARGIN = 2, FUN = sum)==nrow(X))
  # must have number of species < number of samples
  ncol(X)<nrow(X)
  # all objects must be a matrix (not data frame)
  Xmat<-as.matrix(X)
  Xmat<-matrix(Xmat, ncol=ncol(Xmat), dimnames=NULL) # remove names

# note: i didn't add any priors but you can!

#### ... ... without environ correction ####

# can easily run BC with and without accounting for the environment,
# by only using a different specification of the "model" argument
bcc <- BC(Xmat, model="community", its=1000)
# in the absence of any other specificiations, it uses these default settings
# (from the BCfit function): an inverse WIshart prior, a form of "uniformative"
# priors (Pollock et al. 2014 talk about it a little)

# create a dataframe with just the posterior correlation matrix ('interactions')
bcc_cor<-bcc$trace["R"]$R
# so what are these? you can get a good idea of what this is by looking at...
head(bcc_cor)
str(bcc_cor)
# so for each species pair, these are the posterior correlations from each iteration
# of the model (its=1000, as we set above), without any thinning
# recall that in bayesian modeling, estimates from the model are *distributions*
# so for each species pairs, there's an estimated ('posterior') distribution
# of its interaction strength. the way to estimate what that interaction is would
# be to look at the credible intervals (most people use 90%) to see if they
# cross 0, which would indicate that the interaction strenghts are no different
# from 0

# what are the means? create a vector of mean correlations
bcc_cor_mean<-data.frame(apply(X = bcc_cor, MARGIN = 2, FUN = mean))
colnames(bcc_cor_mean)<-"mean_correlations"
# what are the standard deviations?
bcc_cor_sd<-data.frame(apply(X = bcc_cor, MARGIN = 2, FUN = sd))
colnames(bcc_cor_sd)<-"sd_correlations"
# get 90 % credible intervals (lower and upper limit for each pair)
bcc_cor_p<-apply(bcc_cor, 2, FUN = function(x) {quantile(x, c(0.05, 0.95))})
# which don't cross 0?
bcc_sub<-which(bcc_cor_p[1,]<0&bcc_cor_p[2,]<0|bcc_cor_p[1,]>0&bcc_cor_p[2,]>0)
bcc_len<-1:nrow(bcc_cor_mean)
# which species are the ones that have 90% CIs that don't cross 0?
bcc_sub_df<-as.data.frame(bcc_sub)
colnames(bcc_sub_df)<-"index"
bcc_sub_df2<-data.frame(bcc_sub_df,
                        t(as.data.frame(strsplit(rownames(bcc_sub_df), "_"))))
colnames(bcc_sub_df2)[2:3]<-c("sp1","sp2")
bcc_sub_df2$sp1<-substring(bcc_sub_df2$sp1,3) # takes number from 3rd position
bcc_sub_df2$sp2<-substring(bcc_sub_df2$sp2,3)
bcc_sub_df3<-transform(bcc_sub_df2, sp1=as.numeric(sp1), sp2=as.numeric(sp2))
# get species numbers
bcc_sub_df3$sp1_name<-NA
bcc_sub_df3$sp2_name<-NA
for (i in 1:nrow(bcc_sub_df3)) {
  sp_mat_tmp<-colnames(X)
  bcc_sub_df3$sp1_name[i]<-sp_mat_tmp[bcc_sub_df3$sp1[i]]
  bcc_sub_df3$sp2_name[i]<-sp_mat_tmp[bcc_sub_df3$sp2[i]]
}
# add in the intervals & mean
(bcc_sig<-data.frame(bcc_sub_df3, t(bcc_cor_p[,bcc_sub]), bcc_cor_mean[bcc_sub,]))
colnames(bcc_sig)[ncol(bcc_sig)]<-"mean"
# make a full association ("A") matrix with significant & nonsig assoc
# how many unique species are listed in the pairwise list?
stk<-data.frame() # create a df with a column that stacks sp1_name and sp2_name
stk[1:nrow(bcc_sig),1]<-bcc_sig$sp1_name
stk[(nrow(bcc_sig)+1):((nrow(bcc_sig)+1)+(nrow(bcc_sig))-1),1]<-bcc_sig$sp2_name
# need to add on extra species that weren't significant
bcc_sig.df<-data.frame(bcc_sig$sp1_name,bcc_sig$sp2_name,bcc_sig$mean, stringsAsFactors = FALSE)
#  add on NA values
bcc_sig.df[(nrow(bcc_sig.df)+1):
             (nrow(bcc_sig.df)+length(which(colnames(X) %in% unique(stk$V1)==FALSE))),
           1:2]<- cbind(colnames(X)[which(colnames(X) %in% unique(stk$V1)==FALSE)],
                        rev(colnames(X)[which(colnames(X) %in% unique(stk$V1)==FALSE)]))

list2matrix<-function (dat){ 
  dat.name1 <- as.character(dat[, 1])
  dat.name2 <- as.character(dat[, 2])
  dat.value <- dat[, 3]
  names1 <- sort(unique(as.character(dat[, 1])))
  names2 <- sort(unique(as.character(dat[, 2])))
  total.names <- unique(c(names1, names2))
  elements <- rep(NA, length(total.names)^2)
  dim(elements) <- c(length(total.names), length(total.names))
  rownames(elements) <- total.names
  colnames(elements) <- total.names
  for (k in 1:length(dat.name1)) {
    elements[dat.name1[k], dat.name2[k]] <- dat.value[k]}
  # make symmetric
  elements2<-t(elements)
  elements[which(is.na(elements) & upper.tri(elements))] <- elements2[which(is.na(elements) & upper.tri(elements))]
  elements[lower.tri(elements)]<- NA
  elements3<-Matrix::forceSymmetric(elements, "U")
  return(elements3)
}

bcc.A<-as.matrix(list2matrix(bcc_sig.df))
dim(bcc.A)
# replace NAs with 0s
bcc.A[is.na(bcc.A)]<-0 
# is the A matrix symmetric?
isSymmetric(bcc.A) # yes

bcc.A <- bcc.A[order(rownames(bcc.A)),] # order the rows alphabetically
bcc.A <- bcc.A[,order(colnames(bcc.A))] # order the columns alphabetically


plot.new()
pdf(file="jsdm_method.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot from method "B"
g<-graph.adjacency(bcc.A, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "JSDM Residuals",side = 3,line = 2, cex=1.5)
dev.off()


#### ... ... with environ correction ####

# notes about running the BC() function :
  # you cannot have any NA rows or columns
  which(is.na(X))
  # you cannot have any empty rows or columns (no empty samples or species)
  which(apply(X = X, MARGIN = 2, FUN = sum)==0)
  which(apply(X = X, MARGIN = 1, FUN = sum)==0)
  # you cannot have columns of all 1s
  which(apply(X = X, MARGIN = 2, FUN = sum)==nrow(X))
  # must have number of species < number of samples
  ncol(X)<nrow(X)
  # all objects must be a matrix (not data frame)
  Xmat<-as.matrix(X)
  #Xmat<-matrix(Xmat, ncol=ncol(Xmat), dimnames=NULL) # remove names

## environmental matrix  
ENV <- ENV[ , -1 ] # remove Sample (factor) column
View(ENV)  
ENVmat <- as.matrix(ENV)  
str(ENVmat)  
ENVmat <- scale(ENVmat) # scale environmental predictors
View(ENVmat)

## covariates list
# create a new dataframe with covariates
# which, in this case, is the same thing as the df for ENV, but scaled 
env.df <- data.frame(ENVmat)
str(env.df)

# run forward stepwise selection across all covariates for each species
# select the model that gives the lowest AIC and tabulate which covariates
# were selected
# create matrix of results (whether covariate was selected)
res2 <- matrix(0, ncol(Xmat), ncol(ENVmat))
colnames(res2) <- colnames(ENVmat)
rownames(res2) <- colnames(Xmat)
# rows are species, columns are predictors
View(res2)

# an empty list for the selected models
mods <- list()

# run the selection and record results for each species
for (i in 1:ncol(Xmat)) {
  # starting model (intercept only)
  mlow <- glm(Xmat[, i] ~ 1,
              data = env.df, family = binomial(link = "probit"))
  # potential final model (includes all covariates)
  mup <- glm(Xmat[, i] ~.,
             data = env.df, family = binomial(link = "probit"))
  # run model selection
  s <- step(mlow, formula(mup), direction = "forward")
  # add selected model to the list
  mods <- c(mods, list(s))
  # get the names of the selected coefficients and update the results matrix
  sel <- names(s$coefficients)
  ind <- (1:length(sel))[-grep('\\(', sel)]
  #  ind <- which(substring(sel, 1, 4) == "Comp")
  #   nos <- as.numeric(substring(sel[ind], 6, 7))
  res2[i, sel[ind]] <- 1
  #   res2[i, nos] <- 1
}

# matrix indicating whether a variable was selected
res2
     PRISM_ppt PRISM_tmax PRISM_tmean PRISM_tmin
AMGR         1          0           1          1
AMMA         0          0           1          1
ANBO         1          0           0          0
PSRE         0          1           1          1
RAAU         0          0           0          1
RACA         0          1           0          1
TAGR         0          1           0          0

# number of times each component was selected for a species:
# sort(colSums(res2), decreasing = TRUE)

# create a list of the components to use for each species in the modelling
covlist <- apply(res2, 1, function(x) which(as.logical(x)))
covlist

  
## set the number of iterations
iterations <- 1000

## now run the models!
# (warning: the following take several minutes to run
# and can be quite memory-intensive, hence the thinning)

# null model (ignores X; intercept only)

	bcc.null <- BC(Y = Xmat, X = ENVmat, model = "null",
		covlist = covlist,
    #condition = conditioning,
		its = iterations, 
		#burn = burnin, thin = thin)
     )
# check convergence
plot(bcc.null, 'B$AMGR')

# community model
bcc.comm <- BC(Y = Xmat, X = ENVmat, model = "community",
		covlist = covlist,
    #condition = conditioning,
		its = iterations, 
		#burn = burnin, thin = thin)
     )
# check convergence
plot(bcc.comm, 'B$AMGR')
plot(bcc.comm, 'R')

# environment model
bcc.env <- BC(Y = Xmat, X = ENVmat, model = "environment",
		covlist = covlist,
    #condition = conditioning,
		its = iterations, 
		#burn = burnin, thin = thin)
     )
plot(bcc.env, 'B$AMGR')


# full model
bcc.full <- BC(Y = Xmat, X = ENVmat, model = "full",
		covlist = covlist,
    #condition = conditioning,
		its = iterations, 
		#burn = burnin, thin = thin)
     )
# check convergence
par(mar = c(4, 3, 3, 1) + 0.1)
plot(bcc.full, 'B$AMGR')
plot(bcc.full, 'R')
  
# create a dataframe with just the posterior correlation matrix ('interactions')
bcc_cor2<-bcc.full$trace["R"]$R
# so what are these? you can get a good idea of what this is by looking at...
head(bcc_cor2)
str(bcc_cor2)
# so for each species pair, these are the posterior correlations from each iteration
# of the model (its=1000, as we set above), without any thinning
# recall that in bayesian modeling, estimates from the model are *distributions*
# so for each species pairs, there's an estimated ('posterior') distribution
# of its interaction strength. the way to estimate what that interaction is would
# be to look at the credible intervals (most people use 90%) to see if they
# cross 0, which would indicate that the interaction strenghts are no different
# from 0

# what are the means? create a vector of mean correlations
bcc_cor_mean<-data.frame(apply(X = bcc_cor2, MARGIN = 2, FUN = mean))
colnames(bcc_cor_mean)<-"mean_correlations"
# what are the standard deviations?
bcc_cor_sd<-data.frame(apply(X = bcc_cor2, MARGIN = 2, FUN = sd))
colnames(bcc_cor_sd)<-"sd_correlations"
# get 90 % credible intervals (lower and upper limit for each pair)
bcc_cor_p<-apply(bcc_cor2, 2, FUN = function(x) {quantile(x, c(0.05, 0.95))})
# which don't cross 0?
bcc_sub<-which(bcc_cor_p[1,]<0&bcc_cor_p[2,]<0|bcc_cor_p[1,]>0&bcc_cor_p[2,]>0)
bcc_len<-1:nrow(bcc_cor_mean)
# which species are the ones that have 90% CIs that don't cross 0?
bcc_sub_df<-as.data.frame(bcc_sub, stringsAsFactors=FALSE)
colnames(bcc_sub_df)<-"index"
bcc_sub_df2<-data.frame(bcc_sub_df,
                        t(as.data.frame(strsplit(rownames(bcc_sub_df), "_"))))
colnames(bcc_sub_df2)[2:3]<-c("sp1","sp2") 

## somehow the species names were retained, so we don't have to create the columns
## "sp1_name" or "sp2_name". The code hereafter is modified to accomodate this.


# add in the intervals & mean
(bcc_sig<-data.frame(bcc_sub_df2, t(bcc_cor_p[,bcc_sub]), bcc_cor_mean[bcc_sub,]))
colnames(bcc_sig)[ncol(bcc_sig)]<-"mean"
# make a full association ("A") matrix with significant & nonsig assoc
# how many unique species are listed in the pairwise list?
stk<-data.frame() # create a df with a column that stacks sp1 (name) and sp2 (name)
bcc_sig$sp1<-as.character(bcc_sig$sp1) # force species names to be characters
bcc_sig$sp2<-as.character(bcc_sig$sp2)
stk[1:nrow(bcc_sig),1]<-bcc_sig$sp1
stk[(nrow(bcc_sig)+1):((nrow(bcc_sig)+1)+(nrow(bcc_sig))-1),1]<-bcc_sig$sp2
# need to add on extra species that weren't significant
bcc_sig.df<-data.frame(bcc_sig$sp1,bcc_sig$sp2,bcc_sig$mean, stringsAsFactors = FALSE)
#  add on NA values
bcc_sig.df[(nrow(bcc_sig.df)+1):
             (nrow(bcc_sig.df)+length(which(colnames(X) %in% unique(stk$V1)==FALSE))),
           1:2]<- cbind(colnames(X)[which(colnames(X) %in% unique(stk$V1)==FALSE)],
                        rev(colnames(X)[which(colnames(X) %in% unique(stk$V1)==FALSE)]))

 

list2matrix<-function (dat){ 
  dat.name1 <- as.character(dat[, 1])
  dat.name2 <- as.character(dat[, 2])
  dat.value <- dat[, 3]
  names1 <- sort(unique(as.character(dat[, 1])))
  names2 <- sort(unique(as.character(dat[, 2])))
  total.names <- unique(c(names1, names2))
  elements <- rep(NA, length(total.names)^2)
  dim(elements) <- c(length(total.names), length(total.names))
  rownames(elements) <- total.names
  colnames(elements) <- total.names
  for (k in 1:length(dat.name1)) {
    elements[dat.name1[k], dat.name2[k]] <- dat.value[k]}
  # make symmetric
  elements2<-t(elements)
  elements[which(is.na(elements) & upper.tri(elements))] <- elements2[which(is.na(elements) & upper.tri(elements))]
  elements[lower.tri(elements)]<- NA
  elements3<-Matrix::forceSymmetric(elements, "U")
  return(elements3)
}

bcc.B<-as.matrix(list2matrix(bcc_sig.df))
dim(bcc.B)
# replace NAs with 0s
bcc.B[is.na(bcc.B)]<-0 
# is the A matrix symmetric?
isSymmetric(bcc.B) # yes

bcc.B <- bcc.B[order(rownames(bcc.B)),] # order the rows alphabetically
bcc.B <- bcc.B[,order(colnames(bcc.B))] # order the columns alphabetically

View(bcc.B)  
  

plot.new()
pdf(file="jsdm_method_environ.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot from method "B"
g<-graph.adjacency(bcc.B, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "JSDM Residuals (environmental correction)",side = 3,line = 2, cex=1.5)
dev.off()


#### Panel plot of all methods ####

# models used:
lne.A # odds ratio
lne.B # odds ratio with environmental conditioning
hros.A # markov partial correlations
bcc.A # JSDM residuals
bcc.B # JSDM residuals with environmental conditioning
pcvb # inverse covariance

library(igraph)

# first, make sure that all of your matrices have species in the same order

# plot 
plot.new()
pdf(file="multipanelnetwork_horiz.pdf",height=5.7,width=11.4,pointsize=6)
par(mfrow=c(2,3))
par(mar=c(3,3,3,3), oma=c(1,0,1,0))

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

# plot lne.A
g<-graph.adjacency(lne.A, mode="directed",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<1]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>1]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.25) # this line controls how big the arrow is
mtext(text = "Odds Ratio",side = 3,line = 2, cex=1.5)

# plot lne.B
g<-graph.adjacency(lne.B, mode="directed",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<1]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>1]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.25) # this line controls how big the arrow is
mtext(text = "Odds Ratio (environment)",side = 3,line = 2, cex=1.5)

# plot hros.A
g<-graph.adjacency(hros.A, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "Markov Network",side = 3,line = 2, cex=1.5)

# plot bcc.A
g<-graph.adjacency(bcc.A, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "JSDM Residuals",side = 3,line = 2, cex=1.5)

# plot bcc.B
g<-graph.adjacency(bcc.B, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "JSDM Residuals (environment)",side = 3,line = 2, cex=1.5)

# plot pcvb
g<-graph.adjacency(pcvb, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "Inverse Covariance",side = 3,line = 2, cex=1.5)

dev.off()

#### ____________________________________________________________________####

#### Spatial scale-dependencies (using MORA data) ####

# Use MORA Lentic amphibian co-occurrence data
str(MORA.Lentic)
names(MORA.Lentic)
[1] "ObjectID"        "RecordID"        "Year"            "SpeciesCod"     
 [5] "Genus"           "Species"         "Unique_Sit"      "Centroid_X_UTME"
 [9] "Centroid_Y_UTMN" "Centroid_Lat"    "Centroid_Long"   "PRISM_ppt"      
[13] "PRISM_tmax"      "PRISM_tmean"     "PRISM_tmin"      "Site.Num"       
[17] "Habitat"         "Sample"          "Buffer_125m"     "Buffer_250m"    
[21] "Buffer_500m"     "Buffer_1km" 

## Need to create new "samples" (observations by site and year) since sites are
## being aggregated across four distinct spatial scales. 

# Since each of the Buffer variables contain integers to indicate the new
# aggregated sites, have to distinguish the integers in each column. Because a 1
# in one column does not indicate the same group of sites as a 1 in another column.
MORA.Lentic$Buffer_125m<-paste("B125m", MORA.Lentic$Buffer_125m, sep=".")
MORA.Lentic$Buffer_250m<-paste("B250m", MORA.Lentic$Buffer_250m, sep=".")
MORA.Lentic$Buffer_500m<-paste("B500m", MORA.Lentic$Buffer_500m, sep=".")
MORA.Lentic$Buffer_1km<-paste("B1km", MORA.Lentic$Buffer_1km, sep=".")

head(MORA.Lentic)

# Create a new column "Sample_[X]Buff" for each observation that merges 
# the site (now indicated by "Buffer_[X]") and Year
# Sites linked by a buffer distance of 125 m (i.e. nearest neighbor within 250 m)
MORA.Lentic$Sample_125m = paste(MORA.Lentic$Buffer_125m, MORA.Lentic$Year, sep="_")

# Sites linked by a buffer distance of 250 m
MORA.Lentic$Sample_250m = paste(MORA.Lentic$Buffer_250m, MORA.Lentic$Year, sep="_")

# Sites linked by a buffer distance of 500 m
MORA.Lentic$Sample_500m = paste(MORA.Lentic$Buffer_500m, MORA.Lentic$Year, sep="_")

# Sites linked by a buffer distance of 1 km
MORA.Lentic$Sample_1km = paste(MORA.Lentic$Buffer_1km, MORA.Lentic$Year, sep="_")


## Have to create new community matrices for each of these new sample columns
## 125m buffer: columns 4 and 23 of 26
# Step 1: reduce the dataframe to just columns of Sample and SpeciesCod
MORA.Lentic2 <- MORA.Lentic[ , -c(1:3,5:22,24:26) ] # 4738 observations (all samples)
View(MORA.Lentic2)
colnames(MORA.Lentic2)
[1] "SpeciesCod" "Sample_125m" 

# Step 2: make the boolean matrix   
library(reshape2)

w <- melt(MORA.Lentic2) # 4738 observations (all samples), doesn't really change anything
w <- dcast(w, Sample_125m~SpeciesCod) # 477 samples (aggregated)
w[is.na(w)] <- 0        # assigns all NA values a 0
w2 <- cbind(apply(w[,2:8], 2, function(w) as.numeric(w > 0)), w[1])  
                                            # 7 species columns
                                            # recode as 0/1
                                            # applies function only to numeric columns
                                            # binds first column (Sample) 
MORA.matrix.125m <- w2[,c(8, 1:7)]  # rearrange so that site column is first
                                 # rename 
head(MORA.matrix.125m)

## 250m buffer: columns 4 and 24 of 26
MORA.Lentic3 <- MORA.Lentic[ , -c(1:3,5:23,25:26) ] # 4738 observations (all samples)
colnames(MORA.Lentic3)
[1] "SpeciesCod"  "Sample_250m"

w <- melt(MORA.Lentic3) # 4738 observations (all samples), doesn't really change anything
w <- dcast(w, Sample_250m~SpeciesCod) # 394 samples (aggregated)
w[is.na(w)] <- 0        # assigns all NA values a 0
w2 <- cbind(apply(w[,2:8], 2, function(w) as.numeric(w > 0)), w[1])  
                                            # 7 species columns
                                            # recode as 0/1
                                            # applies function only to numeric columns
                                            # binds first column (Sample) 
MORA.matrix.250m <- w2[,c(8, 1:7)]  # rearrange so that site column is first
                                 # rename 
head(MORA.matrix.250m)

## 500m buffer: columns 4 and 25 of 26
MORA.Lentic4 <- MORA.Lentic[ , -c(1:3,5:24,26) ] # 4738 observations (all samples)
colnames(MORA.Lentic4)
[1] "SpeciesCod"  "Sample_500m"

w <- melt(MORA.Lentic4) # 4738 observations (all samples), doesn't really change anything
w <- dcast(w, Sample_500m~SpeciesCod) # 300 samples (aggregated)
w[is.na(w)] <- 0        # assigns all NA values a 0
w2 <- cbind(apply(w[,2:8], 2, function(w) as.numeric(w > 0)), w[1])  
                                            # 7 species columns
                                            # recode as 0/1
                                            # applies function only to numeric columns
                                            # binds first column (Sample) 
MORA.matrix.500m <- w2[,c(8, 1:7)]  # rearrange so that site column is first
                                 # rename 
head(MORA.matrix.500m)

## 1km buffer: columns 4 and 26 of 26
MORA.Lentic5 <- MORA.Lentic[ , -c(1:3,5:25) ] # 4738 observations (all samples)
colnames(MORA.Lentic5)
[1] "SpeciesCod"  "Sample_1km"

w <- melt(MORA.Lentic5) # 4738 observations (all samples), doesn't really change anything
w <- dcast(w, Sample_1km~SpeciesCod) # 209 samples (aggregated)
w[is.na(w)] <- 0        # assigns all NA values a 0
w2 <- cbind(apply(w[,2:8], 2, function(w) as.numeric(w > 0)), w[1])  
                                            # 7 species columns
                                            # recode as 0/1
                                            # applies function only to numeric columns
                                            # binds first column (Sample) 
MORA.matrix.1km <- w2[,c(8, 1:7)]  # rearrange so that site column is first
                                 # rename 
head(MORA.matrix.1km)

### Lentic community matrices at each of the spatial scales:
MORA.matrix.125m
MORA.matrix.250m
MORA.matrix.500m
MORA.matrix.1km
# MORA.Len.matrix is the main matrix without any changes to spatial scales



## Compare Markov networks across these spatial scales 
library(rosalia)            # harris 2015 method

# community matrix
X.125m <- MORA.matrix.125m[ , -1 ] # remove "Sample" column
X.250m <- MORA.matrix.250m[ , -1 ] # remove "Sample" column
X.500m <- MORA.matrix.500m[ , -1 ] # remove "Sample" column
X.1km <- MORA.matrix.1km[ , -1 ] # remove "Sample" column

# run rosalia function (with no prior)
hros.125m <- rosalia(X.125m)
hros.250m <- rosalia(X.250m)
hros.500m <- rosalia(X.500m)
hros.1km <- rosalia(X.1km)
# ^ runs very quickly

# returns 'interaction strengths'
hros.125m.beta<-hros.125m$beta
hros.250m.beta<-hros.250m$beta
hros.500m.beta<-hros.500m$beta
hros.1km.beta<-hros.1km$beta

colnames(hros.125m.beta)<-colnames(X.125m) 
rownames(hros.125m.beta)<-colnames(X.125m)
colnames(hros.250m.beta)<-colnames(X.250m) 
rownames(hros.250m.beta)<-colnames(X.250m)
colnames(hros.500m.beta)<-colnames(X.500m) 
rownames(hros.500m.beta)<-colnames(X.500m)
colnames(hros.1km.beta)<-colnames(X.1km) 
rownames(hros.1km.beta)<-colnames(X.1km)

View(hros.125m.beta); View(hros.250m.beta); View(hros.500m.beta); View(hros.1km.beta)

# Panel plot of all five Markov networks (including original at finest scale)
# models used:
hros.A # Original
hros.125m.beta # 125m buffer (i.e. 250m nearest-neighbor distance)
hros.250m.beta # 250m buffer (i.e. 500m nearest-neighbor distance)
hros.500m.beta # 500m buffer (i.e. 1km nearest-neighbor distance)
hros.1km.beta # 1km buffer (i.e. 2km nearest-neighbor distance)

library(igraph)
#par(mfrow=c(2,3))
#par(mar=c(3,3,3,3), oma=c(1,0,1,0))
layout(matrix(c(0,0,0,1,1,1,1,1,0,0,0, 
              2,2,2,2,2,0,3,3,3,3,3,
              4,4,4,4,4,0,5,5,5,5,5), 3, 11, byrow = TRUE))

layout(matrix(c(0,1,0,
                2,0,3,
                4,0,5), 3, 3, byrow = TRUE))

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

# plot hros.A
g<-graph.adjacency(hros.A, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*5
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "Markov Network",side = 3,line = 2, cex=1.5)

# plot hros.125m.beta
g<-graph.adjacency(hros.125m.beta, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*5
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "250m",side = 3,line = 2, cex=1.5)

# plot hros.250m.beta
g<-graph.adjacency(hros.250m.beta, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*5
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "500m",side = 3,line = 2, cex=1.5)

# plot hros.500m.beta
g<-graph.adjacency(hros.500m.beta, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*5
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "1km",side = 3,line = 2, cex=1.5)

# plot hros.1km.beta
g<-graph.adjacency(hros.1km.beta, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*5
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "2km",side = 3,line = 2, cex=1.5)

#### ____________________________________________________________________####

#### Mt. Hood (HOOD) ####

#### Load original HOOD amphibian co-occurrence data ####
## Contains observations surrounding Mt. Hood, as well as miscellaneous locations
## throughout Oregon
HOOD <- read.csv("HOOD_Amphibians_CSV_102516.csv", stringsAsFactors=FALSE, 
                 fileEncoding="latin1", header=T)
View(HOOD)
str(HOOD)
'data.frame':	2686 obs. of  30 variables:
 $ VisitNum    : chr  "96-20" "96-20" "07-12" "07-12" ...
 $ Obs         : int  1854 1853 2126 2125 1767 1768 1770 1769 1272 1271 ...
 $ Date        : chr  "6/3/1996" "6/3/1996" "4/27/2007" "4/27/2007" ...
 $ Year        : int  1996 1996 2007 2007 1997 1997 1997 1997 1992 1992 ...
 $ SiteCode    : chr  "3MILE" "3MILE" "ABBYC" "ABBYC" ...
 $ SiteName    : chr  "Threemile Cr. nr. Stockton Quarry" "Threemile Cr. nr. Stockton Quarry" "Abby Cr. ponds" "Abby Cr. ponds" ...
 $ State       : chr  "OR" "OR" "OR" "OR" ...
 $ County      : chr  "Wasco" "Wasco" "Washington" "Washington" ...
 $ UTM_Zone    : int  10 10 NA NA 10 10 10 10 10 10 ...
 $ UTM_E       : int  623217 623217 NA NA 573945 573945 573945 573945 574256 574256 ...
 $ UTM_N       : int  5012326 5012326 NA NA 4992358 4992358 4992358 4992358 4992056 4992056 ...
 $ GPS_original: chr  "45*15.22'/121*25.78'" "45*15.22'/121*25.78'" "45*34.70'/122*48.84'" "45*34.70'/122*48.84'" ...
 $ Lat         : chr  "45°15.22'" "45°15.22'" "45°34.70'" "45°34.70'" ...
 $ Lon         : chr  "-121°25.78'" "-121°25.78'" "-122°48.84'" "-122°48.84'" ...
 $ Elev        : chr  "2800F" "2800F" "400F" "400F" ...
 $ Ecosys      : chr  "" "" "" "" ...
 $ Descr       : chr  "" "" "" "" ...
 $ Drain       : chr  "" "" "" "" ...
 $ Substr      : chr  "" "" "" "" ...
 $ FISH        : chr  "" "" "" "" ...
 $ CAPT        : chr  "Net" "Net" "Vis,net" "Vis,net" ...
 $ AIR         : chr  "65-75F" "65-75F" "58F" "58F" ...
 $ WATER       : chr  "48F" "48F" "" "" ...
 $ SpeciesCode : chr  "ASTR" "DICO" "RAAU" "TAGR" ...
 $ CommonName  : chr  "Coastal Tailed Frog" "Cope's Giant Salamander" "Northern red-legged Frog" "Roughskin Newt" ...
 $ ScientName  : chr  "Ascaphus truei" "Dicamptodon copei" "Rana aurora" "Taricha granulosa" ...
 $ Adults      : int  2 NA NA 25 0 10 1 6 NA NA ...
 $ Juvs        : int  NA NA 2 NA NA NA NA NA NA 20 ...
 $ Larvae      : int  NA 2 NA 1 NA NA NA NA NA NA ...
 $ Eggs        : int  NA NA NA NA NA NA NA NA 1 20 ...

HOOD$Adults <-as.numeric(HOOD$Adults)
HOOD$Juvs <-as.numeric(HOOD$Juvs)
HOOD$Larvae <-as.numeric(HOOD$Larvae)
HOOD$Eggs <-as.numeric(HOOD$Eggs)
HOOD$SpeciesCode <-as.factor(HOOD$SpeciesCode)

# Convert "Date" from character to actual Date
HOOD$Date <- as.Date(HOOD$Date, format = "%m/%d/%Y", origin="9/17/1985")

str(HOOD)

# Create a column for presence/absence
HOOD <- within(HOOD, {
  Abundance_matrix <- cbind(HOOD$Adults, HOOD$Juvs, HOOD$Larvae, HOOD$Eggs) # create a matrix of the columns to be summed
  Total_Abundance<-rowSums(Abundance_matrix, na.rm=TRUE) # sum across rows in the matrix
  Presence = ifelse(Total_Abundance >= 0, 1, 0) # new variable of 1's/0's for Presence/Absence
  })
View(HOOD)
HOOD$Abundance_matrix <- NULL # remove extra columns


#### Load clipped HOOD data ####
## Contains only those observations in the region surrounding Mt. Hood
HOODv2 <- read.csv("HOOD_Amphibians_CSV_031017_merged.csv", stringsAsFactors=FALSE, 
                   fileEncoding="latin1", header=T)
View(HOODv2)
str(HOODv2)

HOODv2$Adults <-as.numeric(HOODv2$Adults)
HOODv2$Juvs <-as.numeric(HOODv2$Juvs)
HOODv2$Larvae <-as.numeric(HOODv2$Larvae)
HOODv2$Eggs <-as.numeric(HOODv2$Eggs)
HOODv2$Year <-as.factor(HOODv2$Year)
levels(HOODv2$Year)
# Every year from 1985-2014

HOODv2$SpeciesCod<-as.factor(HOODv2$SpeciesCod)
levels(HOODv2$SpeciesCod)
# 29 species

HOODv2$SiteCode <- as.factor(HOODv2$SiteCode)
levels(HOODv2$SiteCode)
# 219 "sites"

# Create a column for presence/absence
HOODv2 <- within(HOODv2, {
  Abundance_matrix <- cbind(HOODv2$Adults, HOODv2$Juvs, HOODv2$Larvae, HOODv2$Eggs) # create a matrix of the columns to be summed
  Total_Abundance<-rowSums(Abundance_matrix, na.rm=TRUE) # sum across rows in the matrix
  Presence = ifelse(Total_Abundance >= 0, 1, 0) # new variable of 1's/0's for Presence/Absence
  })
HOODv2$Abundance_matrix <- NULL # remove extra columns
View(HOODv2)


#### Subsetting the data ####

## Lentic-breeding species 
HOOD.lentic<-Subset(HOODv2, SpeciesCod=="AMGR" | SpeciesCod=="AMMA"| SpeciesCod=="RAPR"| 
                      SpeciesCod=="ANBO" | SpeciesCod=="PSRE" | SpeciesCod=="RAAU"
                    | SpeciesCod=="RACA" | SpeciesCod=="TAGR"| SpeciesCod=="LICA"|
                    SpeciesCod=="RALU" | SpeciesCod=="SPIN")   
str(HOOD.lentic)
# 1611 observations (reduced from 2267 in the clipped file [HOODv2] and 2686 in 
# the original file [HOOD])
levels(HOOD.lentic$SiteCode)
# 117 sites

# Create a new column "Sample" for each observation that merges the Site and Year
HOOD.lentic$Sample = paste(HOOD.lentic$SiteCode, HOOD.lentic$Year, sep="_")

# Remove all of the unnecessary columns
names(HOOD.lentic)
HOOD.lentic <- HOOD.lentic[ , -c(3,6:8,12:23,25:26) ]
View(HOOD.lentic)
# Export HOOD.lentic
write.csv(HOOD.lentic, "HOOD_Amphibians_Lentic_CSV_031017.csv")

#### Load HOOD Lentic amphibian co-occurrence data *** ####
HOOD.Lentic <- read.csv("HOOD_Amphibians_Lentic_CSV_031017.csv", stringsAsFactors=FALSE, 
                   fileEncoding="latin1", header=T)
View(HOOD.Lentic)
str(HOOD.Lentic)

HOOD.Lentic$Year <-as.factor(HOOD.Lentic$Year)

HOOD.Lentic$SpeciesCod<-as.factor(HOOD.Lentic$SpeciesCod)
levels(HOOD.Lentic$SpeciesCod)
# 9 species

HOOD.Lentic$SiteCode <- as.factor(HOOD.Lentic$SiteCode)
HOOD.Lentic$Sample <- as.factor(HOOD.Lentic$Sample)


 
#### ... Lentic community matrix ***####
View(HOOD.Lentic) # see previous section on how this dataframe was subset & created
                  # This file can be loaded directly into R instead of going through
                  # those steps (see section: "Load Lentic HOOD amphibian co-occurrence data")

## Step 1: reduce the dataframe to just columns of Sample and SpeciesCode
## columns 5 and 9 of 15
HOOD.Lentic2 <- HOOD.Lentic[ , -c(1:4,6:8,10:15) ] # 1611 observations (all samples)
View(HOOD.Lentic2)
colnames(HOOD.Lentic2)
[1] "Sample"     "SpeciesCod"

## Step 2: make the boolean matrix   
library(reshape2)

w <- melt(HOOD.Lentic2) # 1611 observations (all samples), doesn't really change anything
w <- dcast(w, Sample~SpeciesCod) # 583 samples (aggregated)
w[is.na(w)] <- 0        # assigns all NA values a 0
w2 <- cbind(apply(w[,2:10], 2, function(w) as.numeric(w > 0)), w[1])  
                                            # 9 species columns
                                            # recode as 0/1
                                            # applies function only to numeric columns
                                            # binds first column (Sample) 
HOOD.Len.matrix <- w2[,c(10, 1:9)]  # rearrange so that site column is first
                                 # rename 
View(HOOD.Len.matrix)
head(HOOD.Len.matrix)
     Sample AMGR AMMA ANBO LICA PSRE RAAU RACA RAPR TAGR
1 ALDF_1997    0    0    0    0    1    0    0    0    1
2 ALDR_1992    0    0    0    0    1    1    0    0    0
3 ALDR_1993    1    0    0    0    1    1    0    0    1
4 ALDR_1994    1    0    0    0    1    1    0    0    1
5 ALDR_1995    0    0    0    0    1    1    0    0    1
6 ALDR_2010    1    0    0    0    1    1    0    0    0

#### Determining sampling frequency ####
## This section will determine how often species were sampled AND how 
## many sites were sampled each year AND which sites were repeatedly 
## sampled (and how often) over the course of the study

# Sum of # of sites where each species was observed (at least once)
HOOD.Lentic3 <- HOOD.Lentic[ , -c(1:3,5:8,10:15) ] # need just SiteCode and SpeciesCod
x <- melt(HOOD.Lentic3) # 1611 observations (all samples), doesn't really change anything
x <- dcast(x, SiteCode~SpeciesCod) # 117 sites
x[is.na(x)] <- 0        # assigns all NA values a 0
x2 <- cbind(apply(x[,2:10], 2, function(x) as.numeric(x > 0)), x[1])  
                                            # 9 species columns
                                            # recode as 0/1
                                            # applies function only to numeric columns
                                            # binds first column (Sample) 
colSums(x2[,c(1:9)]) # sum species presence/absence columns to get
                    # frequencies of observations per site (117 sites total)
AMGR AMMA ANBO LICA PSRE RAAU RACA RAPR TAGR 
  49   23   21    3   55   25   61    1   63 
 
  
library(plyr)
## How many observations per year?
ddply(HOOD.Lentic, .(Year), summarize, no_rows=length(Year), .drop=FALSE)
# apparently no lentic sites were surveyed in 1985 and even 1986 is iffy with only 2 sites

detach("package:FSA", unload=TRUE) 
detach("package:plyr", unload=TRUE) # packages conflict with parts of dplyr
library(dplyr)
byYearSite <- group_by(HOOD, SiteCode, Year )
sumYearSite <- summarise(byYearSite, count=n())
View(sumYearSite)
# 720 site x year combos

# find unique Site/Year combos - those will be the sites that are only sampled once
sumYearSite<-as.data.frame(sumYearSite)
single_obs<- unique(sumYearSite["SiteCode"])
View(single_obs)

repeated_visits <-  sumYearSite %>% 
                        group_by(SiteCode) %>% 
                            filter(n()>1)
View(repeated_visits)

single_visits <-  sumYearSite %>% 
                        group_by(SiteCode) %>% 
                            filter(n()<2)
View(single_visits) # list of sites visited only once (240)

unique_sites<- unique(repeated_visits["SiteCode"])
unique_sites
# 86 repeatedly sampled sites 
 
counts_bysite <-  repeated_visits %>% 
                              group_by(SiteCode) %>% 
                                    summarise(count=n())  
View(counts_bysite) # list of sites visited more than once
# Sites sampled relatively consistently over the 30 years of Char's surveys:
# BRUR 28 years
# CAMAS 18 years
# DRY 23 years
# FROG 23 years
# FRY 26 years
# JACK 24 years
# LCM 20 years
# WEENS 20 years


#### Vizualizing co-occurrence ####

X <- HOOD.Len.matrix[ , -1 ] 
total_occurrences <- colSums(X)
total_occurrences
AMGR AMMA ANBO LICA PSRE RAAU RACA RAPR TAGR 
 122   82  147    3  217   77  198   17  173 
 # total = 1036 occurrences
data_matrix <- as.matrix(X)
co_occurrence <- t(data_matrix) %*% data_matrix # transpose the matrix
co_occurrence
     AMGR AMMA ANBO LICA PSRE RAAU RACA RAPR TAGR
AMGR  122   37   52    0   76   25   70    3   79
AMMA   37   82   49    0   66    6   43    5   45
ANBO   52   49  147    0  106   20   63    0   71
LICA    0    0    0    3    0    2    0    0    0
PSRE   76   66  106    0  217   26  116   17  111
RAAU   25    6   20    2   26   77   14    0   32
RACA   70   43   63    0  116   14  198   10   89
RAPR    3    5    0    0   17    0   10   17   17
TAGR   79   45   71    0  111   32   89   17  173

# to convert this matrix to proportions (i.e. probabilities), have to calculate
# their joint proporitons of co-occurrence. 
# e.g. AMGR-AMMA 
# AMGR-only occurrences:
122-37 # total AMGR occurrences minus their shared co-occurrence 
# AMMA-only occurrences:
82-37 # total AMGR occurrences minus their shared co-occurrence 
# where AMGR and AMMA co-occur
37
# joint probability of co-occurrence
37/((122-37)+(82-37)+37)
# or...
37/(122+82-37) # 0.2215569

# now do this automatically and create a joint proportion of occurrence matrix
cooccurr_prop <-  matrix(, nrow = 9, ncol = 9) # create an empty 9x9 matrix

for (j in 1:9) { 
    for (i in 1:9) {
  cooccurr_prop[i,j] <- co_occurrence[i,j]/(co_occurrence[i,i] + 
                        co_occurrence[j,j] - co_occurrence[i,j]) 
}}
print(cooccurr_prop)
colnames(cooccurr_prop)<-colnames(co_occurrence)
rownames(cooccurr_prop)<-colnames(co_occurrence)


plot.new()
pdf(file="hood_cooccurrence.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot from method "A"
g<-graph.adjacency(cooccurr_prop, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*5
E(g)[E(g)$weight>0]$color<-"black" # all species co-occur at least once
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:9, direction=-1, start=0) # 
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
#mtext(text = "Raw co-occurrences",side = 3,line = 2, cex=1.5) # this line controls how big the arrow is
dev.off()





#### Visualizing abundance data ####

## Site = BRUR, Bull Run Upper Reservoir 
# BRUR is a permanent lake with fish (trout)

library(FSA)
HOOD.BRUR<-Subset(HOOD.Lentic, SiteCode=="BRUR") 
View(HOOD.BRUR)

HOOD.BRUR$Presence<-as.factor(HOOD.BRUR$Presence)

HOOD.BRUR$Date <- as.Date(HOOD.BRUR$Date, format = "%Y-%m-%d", origin="1987-05-20")

levels(HOOD.BRUR$SpeciesCode)
[1] "AMGR" "ANBO" "ASTR" "ELCO" "none" "PLDU" "PSRE" "RAAU" "TAGR" "THOR" "THSI"
# pond-breeding species of interest: AMGR, ANBO, PSRE, RAAU, TAGR, possibly
# ASTR

## Subset to just pond-breeding species
HOOD.BRUR<-Subset(HOOD.BRUR, SpeciesCode=="AMGR" | SpeciesCode=="ANBO" | 
                    SpeciesCode=="PSRE" | SpeciesCode=="RAAU" | SpeciesCode=="TAGR") 


# Look at trends
library(ggplot2)
ggplot(HOOD.BRUR, aes(x=Year, y=Eggs, colour=SpeciesCode)) +
  geom_point(aes(colour=SpeciesCode), size=2) +  
  geom_line(aes(colour=SpeciesCode), size=2)    
    #scale_y_continuous(limits=c(0, 1.2), breaks=seq(0,1.2, by=0.2) )

ggplot(HOOD.BRUR, aes(x=Year, y=Larvae, colour=SpeciesCode)) +
  geom_point(aes(colour=SpeciesCode), size=2) +  
  geom_line(aes(colour=SpeciesCode), size=2)    
    #scale_y_continuous(limits=c(0, 1.2), breaks=seq(0,1.2, by=0.2) )

ggplot(HOOD.BRUR, aes(x=Year, y=Total_Abundance, colour=SpeciesCode)) +
  geom_point(aes(colour=SpeciesCode), size=2)   +
  geom_smooth(aes(colour=SpeciesCode), size=2, se=FALSE)  +
  scale_y_continuous(limits=c(0, 500), breaks=seq(0,500, by=50) )

ggplot(HOOD.BRUR, aes(x=Year, y=SpeciesCode)) +
  geom_tile(aes(fill = Total_Abundance)) # or could do fill=Presence

ggplot(HOOD.BRUR, aes(x=Year, y=SpeciesCode)) +
  geom_point(aes(colour = SpeciesCode)) 

# Since toads can exist at extremely high densities (>10,000) and abundance estimates
# are not necessarily accurate at those extremes, 
# we can bin the total abundance estimates...

require(Hmisc)
HOOD.BRUR$Abundance_bin<-cut2(HOOD.BRUR$Total_Abundance, c(1,10,50,100,250,500,1000,Inf) )
View(HOOD.BRUR)

library(wesanderson)
wes_pal <- wes_palette("Zissou", 8, type = "continuous")

# Presence
ggplot(HOOD.BRUR, aes(x=Year, y=SpeciesCode)) +
  geom_point(aes(colour = Presence), size=6, pch=16) +
  theme_bw() +
  scale_color_manual(values=wes_palette("Zissou", 1)) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    ylab("Species") +
    xlab("Year") +
    scale_x_continuous(limits=c(1987, 2014), breaks=seq(1987, 2014, by=3))+
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.title.y=element_text(size=14, vjust=1)) +
    theme(axis.title.x=element_text(size=14, vjust=-0.25)) +
    theme(legend.title=element_text(size=12)) +
    theme(legend.text=element_text(size=12))

# Total abundance
ggplot(HOOD.BRUR, aes(x=Year, y=SpeciesCode)) +
  geom_point(aes(colour = Abundance_bin), size=6, pch=16) +
  scale_color_manual(values = wes_pal) +
  theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    ylab("Species") +
    xlab("Year") +
    scale_x_continuous(limits=c(1987, 2014), breaks=seq(1987, 2014, by=3))+
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.title.y=element_text(size=14, vjust=1)) +
    theme(axis.title.x=element_text(size=14, vjust=-0.25)) +
    theme(legend.title=element_text(size=12)) +
    theme(legend.text=element_text(size=12)) 

# Adult Abundance 
wes_pal <- wes_palette("Zissou", 5, type = "continuous")
ggplot(HOOD.BRUR, aes(x=Year, y=SpeciesCode)) +
  geom_point(aes(colour = Adults), size=6, pch=16) +
  scale_colour_gradientn(colours = wes_pal) +
  theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    ylab("Species") +
    xlab("Year") +
    scale_x_continuous(limits=c(1987, 2014), breaks=seq(1987, 2014, by=3))+
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.title.y=element_text(size=14, vjust=1)) +
    theme(axis.title.x=element_text(size=14, vjust=-0.25)) +
    theme(legend.title=element_text(size=12)) +
    theme(legend.text=element_text(size=12)) 
 
# Egg Abundance 
wes_pal <- wes_palette("Zissou", 6, type = "continuous")
ggplot(HOOD.BRUR, aes(x=Year, y=SpeciesCode)) +
  geom_point(aes(colour = Eggs), size=6, pch=16) +
  scale_colour_gradientn(colours = wes_pal) +
  theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    ylab("Species") +
    xlab("Year") +
    scale_x_continuous(limits=c(1987, 2014), breaks=seq(1987, 2014, by=3))+
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.title.y=element_text(size=14, vjust=1)) +
    theme(axis.title.x=element_text(size=14, vjust=-0.25)) +
    theme(legend.title=element_text(size=12)) +
    theme(legend.text=element_text(size=12)) 

# Larval Abundance 
wes_pal <- wes_palette("Zissou", 4, type = "continuous")
ggplot(HOOD.BRUR, aes(x=Year, y=SpeciesCode)) +
  geom_point(aes(colour = Larvae), size=6, pch=16) +
  scale_colour_gradientn(colours = wes_pal) +
  theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    ylab("Species") +
    xlab("Year") +
    scale_x_continuous(limits=c(1987, 2014), breaks=seq(1987, 2014, by=3))+
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.title.y=element_text(size=14, vjust=1)) +
    theme(axis.title.x=element_text(size=14, vjust=-0.25)) +
    theme(legend.title=element_text(size=12)) +
    theme(legend.text=element_text(size=12))

# Juvenile Abundance 
wes_pal <- wes_palette("Zissou", 5, type = "continuous")
ggplot(HOOD.BRUR, aes(x=Year, y=SpeciesCode)) +
  geom_point(aes(colour = Juvs), size=6, pch=16) +
  scale_colour_gradientn(colours = wes_pal) +
  theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    ylab("Species") +
    xlab("Year") +
    scale_x_continuous(limits=c(1987, 2014), breaks=seq(1987, 2014, by=3))+
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.title.y=element_text(size=14, vjust=1)) +
    theme(axis.title.x=element_text(size=14, vjust=-0.25)) +
    theme(legend.title=element_text(size=12)) +
    theme(legend.text=element_text(size=12))

## Since there are multiple observations per species within a year (e.g. first 
## visit observed breeding adults, second visit observed eggs), perhaps instead
## of aggregating by year, just do a time series by date.
wes_pal <- wes_palette("Zissou", 8, type = "continuous")
ggplot(HOOD.BRUR, aes(x=Date, y=SpeciesCode)) +
  geom_point(aes(colour = Abundance_bin), size=4, pch=16) +
  scale_color_manual(values = wes_pal) +
  theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank()) +
    ylab("Species") +
    xlab("Year") +
    #scale_x_continuous(limits=c(1987, 2014), breaks=seq(1987, 2014, by=3))+
    theme(axis.text.x=element_text(size=12)) +
    theme(axis.text.y=element_text(size=12)) +
    theme(axis.title.y=element_text(size=14, vjust=1)) +
    theme(axis.title.x=element_text(size=14, vjust=-0.25)) +
    theme(legend.title=element_text(size=12)) +
    theme(legend.text=element_text(size=12)) 


#### Co-occurrence Analyses ####

##### ... Partial correlation (Harris 2016) method #####

install.packages('progress','corpcor')
library(progress)           # required to load rosalia package
library(corpcor)            # for regularized partial covariance

devtools::install_github("davharris/rosalia")
library(rosalia)            # harris 2015 method

# function rosalia() developed in harris 2015 (bioRxiv) is the
# best-performing method to evaluate "true" direct interactions
# (rather than estimating net effects), with partial covariance
# as the second best-performing method for - for speciose (S > 20)
# communities of intermediate sample size (N = ~100), which
# best matches the form of our community data

# community matrix
X <- HOOD.Len.matrix[ , -1 ] # remove "Sample" column

# however, rosalia() does not work well for communities with > 20 nodes
# because there are 2^S terms for S nodes
2^ncol(X)
# 512

# run rosalia function (with no prior)
hros <- rosalia(X)
# ^ runs very quickly

# returns 'interaction strengths'
hros.A<-hros$beta

colnames(hros.A)<-colnames(X)
rownames(hros.A)<-colnames(X)


plot.new()
pdf(file="hood_harris_method.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot 
g<-graph.adjacency(hros.A, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:9, direction=-1, start=0) # 9 = number of nodes (species)
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "Markov Network",side = 3,line = 2, cex=1.5)
dev.off()

## LICA and RAPR only occur in 3 and 1 samples, respectively. If we remove them, the
## pool of species will be identical between both regions. 
View(X)
# remove columns 4 (LICA) and 8 (RAPR)
X.red <- X[-c(4,8) ] 
View(X.red)

# run rosalia function (with no prior)
hros.B <- rosalia(X.red)
# ^ runs very quickly

# returns 'interaction strengths'
hros.B<-hros.B$beta

colnames(hros.B)<-colnames(X.red)
rownames(hros.B)<-colnames(X.red)
hros.B

plot.new()
pdf(file="hood_harris_method.pdf",height=4,width=4)
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# plot 
g<-graph.adjacency(hros.B, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) # set the layout form of "g" (your graph), i chose circle
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) # allows plotting outside of the normal range
deg<-5 # set the size of your node (species). can set it to be dependent on another
# variable, like its "centrality" to the network (larger=more central). i didn't do this
V(g)$size<-deg # using format "V(g)$something <- somethingelse" is how you set the attributes
# of the vertices (the species nodes). V= vertices = species, E = edges= interactions
# V(g)$label<-NA # can add species names here
E(g)$width<-(abs(E(g)$weight)/max(abs(E(g)$weight)))*3
E(g)[E(g)$weight<0]$color<-"red"  # red for negative interactions
E(g)[E(g)$weight>0]$color<-"blue" # blue for positive interactions. note that if you use a weighted 
# index for interactions from co-occurrence, not always so simple as 
# which is > or < 0, because some are centered around 1
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:7, direction=-1, start=0) # 7 = number of nodes (species)
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans") 
mtext(text = "Markov Network",side = 3,line = 2, cex=1.5)
dev.off()

# panel plot comparing network with and without rare species
par(mfrow=c(1,2))
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
# re run plots from above (without titles)

#### ______________________________________________________________________ ####

#### Known interactions among species in the network ####

# Step 1: create empty matrix of all species (including 2 rarest species: LICA, RAPR)
# Step 2: for each study, fill in -1 for predation and 1 for competition and then color-code
# accordingly. For this, actual sign and weight are erroneous UNLESS interaction strength
# was quantified. We will also make graphs undirected.

X.empty <-  matrix(, nrow = 9, ncol = 9) # create an empty 7x7 matrix
colnames(X.empty)<-colnames(X) # can just use full HOOD data; X <- HOOD.Len.matrix[ , -1 ] 
rownames(X.empty)<-colnames(X)
X.empty[is.na(X.empty)] <- 0
X.empty

# Experiments on predation, and it's other forms (e.g. intraguild predation, cannibalism) 
X.pred<-X.empty
X.pred[9, c(3,5,7)] <- -1 # TAGR->ANBO, TAGR->PSRE, TAGR->RACA (Peterson & Blaustein 1991)
                          # TAGR-> RACA also in Romansic et al (2009)
X.pred[2, c(3,5,7)] <- -1 # AMMA->ANBO, AMMA->PSRE, AMMA->RACA (Thurman & Garcia in prep-b)
X.pred[3, c(3,5,7)] <- -1 # ANBO->PSRE, ANBO->ANBO (cannibalism), 
                                  # ANBO->RACA (Jordan et al. 2004)
X.pred[8, 3] <- -1 # RAPR->ANBO (Pearl & HAyes 2002)
X.pred[2, 2] <- -1 # AMMA cannibalism (Walls et al. 1993; Wildy et al 2001)
X.pred[1, 5] <- -1 # AMGR->PSRE (Puttlitz et al 1999)
X.pred[9, 1] <- -1 # TAGR->AMGR (MacCracken 2007)
X.pred[9, 6] <- -1 # TAGR->RAAU (Kiesecker et al 2003; Wilson & Lefcort 1993)
X.pred[9, 4] <- -1 # TAGR->LICA (Lefcort & Eiger 1993)
X.pred[9, 2] <- -1 # TAGR->AMMA (Garcia et al 2009) also, TAGR->PSRE, but that is already
                    # included above
X.pred[4, c(6,8)] <- -1 # LICA->RAAU, LICA->RAPR (Pearl et al 2004) also, LICA->RAAU
                        # in Kiesecker & Blaustein 1998 and Adams 2000
X.pred[4, 5] <- -1 # LICA->PSRE (Adams 2000) also, LICA->RAAU, but that is already included above
X.pred[4, c(1,4)] <- -1 # LICA cannibalism, LICA->AMGR (Rowe MS Thesis 2013)


# Experiments on interspecific competition
X.comp<-X.empty
X.comp[c(3,5,7), c(3,5,7)] <- 1 # ANBO<->PSRE<->RACA (Thurman & Garcia in prep-a)
  #X.comp[5,5] <- 0              # together with previous interaction set, predator-mediated 
  #X.comp[7,7] <- 0              # competition (Thurman & Garcia in prep-b)  
                                 # also, PSRE<->RACA (Kiesecker & Blaustein 1999)
                                 # also, 3-spp competition (Han et al. 2015)
X.comp[c(2,1), c(1,2)] <- 1 # AMMA<->AMGR Pearman (2002)
X.comp[c(9,1), c(1,9)] <- 1 # TAGR<->AMGR (Taylor 1984)
X.comp[c(6,8), c(8,6)] <- 1 # RAPR<->RAAU (Barnett & Richardson 2002)
X.comp[c(6,5), c(5,6)] <- 1 # PSRE<->RAAU (Hamilton & Richardson 2012)
X.comp[c(4,5), c(5,4)] <- 1 # PSRE<->LICA (Monello et al 2006)
X.comp[c(4,6), c(6,4)] <- 1 # RAAU<->LICA (Kiesecker et al 2001)
X.comp[c(4,8), c(8,4)] <- 1 # RAPR<->LICA (Pearl et al 2005); reproductive interference,
                            # also with RAAU


par(mfrow=c(1,2))
par(mar=c(3,3,3,3), oma=c(1,0,1,0))
        
# predation plot
g<-graph.adjacency(X.pred, mode="directed",weighted=TRUE,diag=TRUE)
l<-layout.circle(g) 
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) 
deg<-5
V(g)$size<-deg 
E(g)$width<-2
E(g)[E(g)$weight==-1]$color<-"darkred"  # red for predation
#E(g)[E(g)$weight==1]$color<-"red" # red for competitive interactions.
#g<-delete.vertices(g,which(degree(g)==0))
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:9, direction=-1, start=0) # 9 = number of nodes (species)
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans",
     edge.arrow.size=0.5) 
mtext(text = "Predation",side = 3,line = 2, cex=1.5)


# competition plot
g<-graph.adjacency(X.comp, mode="undirected",weighted=TRUE,diag=FALSE)
l<-layout.circle(g) 
l<- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) 
deg<-5
V(g)$size<-deg 
E(g)$width<-2
#E(g)[E(g)$weight==-1]$color<-"red"  # red for predation
E(g)[E(g)$weight==1]$color<-"red" # red for competitive interactions.
#g<-delete.vertices(g,which(degree(g)==0))
V(g)$color <- "gray80"
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=1:9, direction=-1, start=0) # 9 = number of nodes (species)
plot(g, rescale=FALSE, 
     layout=l*1.05,
     #vertex.size=2, 
     vertex.label.dist=1, 
     vertex.label.degree=lab.locs, 
     vertex.color="gray80",
     vertex.label.color="black",
     vertex.label.family="sans")      
mtext(text = "Competition",side = 3,line = 2, cex=1.5)