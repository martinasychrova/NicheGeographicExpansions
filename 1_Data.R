################################################################################
####                          DATA IMPORT ----                                
################################################################################

### 1. CLIMATIC DATA -----------------------------------------------------------

### (1a) NORTH AMERICA (NATIVE) ------------------------------------------------
### selected 12 variables used in species distribution models

path_natinv <- list.files(path="path_to_folder_with_rasters_Worldclim_selection",pattern='tif', full.names=TRUE)

bio_natinv <- stack(path_natinv)


### (1b) EUROPE (INVADED) -------------------------------------------------------
### selected 12 variables used in species distribution models

path_inv <- list.files(path="path_to_folder_with_rasters_Worldclim_selection_INV",pattern='tif', full.names=TRUE)

bio_inv <- stack(path_inv)


### 2. SPECIES DATA ------------------------------------------------------------
### columns and variables:
#       long, lat
#       occ: 
#           1 (occurrences and pseudo-occurrences)
#           0 - absences
#       Dataset:
#           GBIF_INV - occurrences from GBIF only for invaded area (Europe)
#           GBIF_NAT - occurrences from GBIF only for native area (North America)
#           FNFI - occurrences from French National Forest Inventory for invaded area (Europe)
#           Little - random points (pseudo-occurrences) generated inside native distribution range based
#                    on Little (1971) for each species
#           RP_INV - random points (absences) generated inside invaded area (Europe)
#           RP_NAT - random points (absences) generated inside native area (North America)
#       bio_1-19: bioclimatic variables based on Worldclim rasters
#       species: trees and tall shrubs from North America introduced in Europe



all_df <- read.delim("dataset_all.csv",
                     header = T, sep = ",")
all_df$X <- NULL #remove first column

all <- split(all_df, f = all_df$species) #create a list of dataframes for each species

nm <- names(all) # create object with 59 species

# select only occurrences
occ <- lapply(all, FUN=function(x){x[x$occ == 1, ]})

# select only absences
abs <- lapply(all, FUN=function(x){x[x$occ == 0, ]})

