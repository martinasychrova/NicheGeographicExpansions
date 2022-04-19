################################################################################
####                      ENVIRONMENTAL FILTERING ----                                
################################################################################

# R function ENVfilter by Jan Divisek

# We plotted occurrence records into the multidimensional environmental space 
# defined by the standardised climatic variables and geographic coordinates 
# (in metres) for each species. We then divided this space into equal-sized bins
# and randomly selected one record from each bin. Records from native and 
# invaded ranges were filtered separately to ensure that occurrences in 
# overlapping climates were equally represented in both ranges. This procedure 
# resulted in a subset of filtered occurrences that more evenly covered species 
# environmental niches and all occupied areas. In total, we created six 
# alternative subsets of occurrence records that resulted from filtering with 
# 0.001, 0.005, 0.01, 0.05, 0.1, 0.5 and 1 SD wide bins.


# bioclimatic variables
bio.vars <- c("bio_1", "bio_2", "bio_3", "bio_4", "bio_5", "bio_6", "bio_7",
                  "bio_8", "bio_9","bio_10", "bio_11", "bio_12","bio_13", "bio_14", "bio_15", 
                  "bio_16","bio_17", "bio_18", "bio_19")

# create filtered dataset
natinv_filt <- list()

for(i in c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1))
{
  print(i)
  
  natinv_filt[[paste0("F", i)]] <- list()
  
  for(q in seq(1, length(nm)))
  {
    Am <- ENVfilter(Data = occ[[q]][occ[[q]]$Dataset == "GBIF_NAT"
                                    | occ[[q]]$Dataset == "Little",], 
                    Vars = c(bio.vars, "long", "lat"), 
                    Resolution = i, 
                    Seed= 123)
    Eu <- ENVfilter(Data = occ[[q]][occ[[q]]$Dataset != "GBIF_NAT"
                                    & natinv_occ[[q]]$Dataset != "Little"
                                    & natinv_occ[[q]]$Dataset == "GBIF_EU"
                                    | natinv_occ[[q]]$Dataset == "FNFI",], 
                    Vars = c(bio.vars, "long", "lat"), 
                    Resolution = i, 
                    Seed= 123)
    
    natinv_filt[[paste0("F", i)]][[q]] <- rbind(Am, Eu)
  }
}


