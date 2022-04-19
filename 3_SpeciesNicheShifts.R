################################################################################
#                              SPECIES NICHE SHIFTS ----                                
################################################################################

library(ecospat)

###### 1. DATA -----------------------------------------------------------------

# add rp NAT
nat <- list()
for(u in seq(1, length(names(natinv_filt))))
{
  nat[[u]] <- natinv_filt[[u]]
  for(i in seq(1, length(nm)))
  {
    nat[[u]][[i]] <- rbind(natinv_filt[[u]][[i]]
                           [which(natinv_filt[[u]][[i]]$Dataset == "GBIF_NAT"
                                  | natinv_filt[[u]][[i]]$Dataset == "Little"),],
                           natinv_abs[[i]][which(natinv_abs[[i]]$Dataset == "RP_NAT"),])
  }
}

# add rp INV
inv <- list()
for(u in seq(1, length(names(natinv_filt))))
{
  inv[[u]] <- natinv_filt[[u]]
  for(i in seq(1, length(nm)))
  {
    inv[[u]][[i]] <- rbind(natinv_filt[[u]][[i]]
                           [which(natinv_filt[[u]][[i]]$Dataset == "GBIF_EU"
                                  | natinv_filt[[u]][[i]]$Dataset == "FNFI"),],
                           natinv_abs[[i]][which(natinv_abs[[i]]$Dataset == "RP_INV"),])
  }
}


###### 2. PCA ------------------------------------------------------------------

pca.env <- list()
for(u in seq(1, length(natinv_filt)))
{
  pca.env[[u]] <- natinv_filt[[u]]
  for(i in 1:length(nm))
  {
    print(u)
    pca.env[[u]][[i]] <- dudi.pca(rbind(nat[[u]][[i]],inv[[u]][[i]])
                                  [,c(5:23)],scannf=F,nf=2)
  }
}


### (2a) PCA scores for the whole study area 
scores.globclim <- list()
for(u in seq(1, length(natinv_filt)))
{
  scores.globclim[[u]] <- natinv_filt[[u]]
  for(i in 1:length(nm))
  {
    scores.globclim[[u]][[i]] <- pca.env[[u]][[i]]$li
  }
}


### (2b) PCA scores for the species native distribution
scores.sp.nat <- list()
for(u in seq(1, length(natinv_filt)))
{
  scores.sp.nat[[u]] <- natinv_filt[[u]]
  for(i in 1:length(nm))
  {
    scores.sp.nat[[u]][[i]] <- suprow(pca.env[[u]][[i]],
                                      nat[[u]][[i]][which(nat[[u]][[i]][,3]==1),
                                                    c(5:23)])$li
  }
}


### (2c) PCA scores for the species invasive distribution
scores.sp.inv <- list()
for(u in seq(1, length(natinv_filt)))
{
  scores.sp.inv[[u]] <- natinv_filt[[u]]
  for(i in 1:length(nm))
  {
    scores.sp.inv[[u]][[i]] <- suprow(pca.env[[u]][[i]],
                                      inv[[u]][[i]][which(inv[[u]][[i]][,3]==1),
                                                   c(5:23)])$li
  }
}

### (2d) PCA scores for the whole native study area
scores.clim.nat <- list()
for(u in seq(1, length(natinv_filt)))
{
  scores.clim.nat[[u]] <- natinv_filt[[u]]
  for(i in 1:length(nm))
  {
    scores.clim.nat[[u]][[i]] <- suprow(pca.env[[u]][[i]],
                                        nat[[u]][[i]][,c(5:23)])$li
  }
}



### (2e) PCA scores for the whole invaded study area
scores.clim.inv <- list()
for(u in seq(1, length(natinv_filt)))
{
  scores.clim.inv[[u]] <- natinv_filt[[u]]
  for(i in 1:length(nm))
  {
    scores.clim.inv[[u]][[i]] <- suprow(pca.env[[u]][[i]],
                                        inv[[u]][[i]][,c(5:23)])$li
  }
}




###### 3. GRIDDING -------------------------------------------------------------

### (3a) gridding the native niche
grid.clim.nat <- list()
for(u in 1:length(natinv_filt))
{
  print(u)
  grid.clim.nat[[u]] <- natinv_filt[[u]]
  for(i in 1:length(nm)){
    grid.clim.nat[[u]][[i]] <- ecospat.grid.clim.dyn(glob=scores.globclim[[u]][[i]],
                                                     glob1=scores.clim.nat[[u]][[i]],
                                                     sp=scores.sp.nat[[u]][[i]], R=100,
                                                     th.sp=0)
  }
}


### (3b) gridding the invasive niche
grid.clim.inv <- list()
for(u in 1:length(natinv_filt))
{
  print(u)
  grid.clim.inv[[u]] <- natinv_filt[[u]]
  for(i in 1:length(nm)){
    grid.clim.inv[[u]][[i]] <- ecospat.grid.clim.dyn(glob=scores.globclim[[u]][[i]],
                                                     glob1=scores.clim.inv[[u]][[i]],
                                                     sp=scores.sp.inv[[u]][[i]], R=100,
                                                     th.sp=0)
  }
}


###### 4. NICHE SHIFTS INDICES -------------------------------------------------
### (4a) Calculate Schoener's D (niche overlap index) 
D.overlap <- list()
for(u in 1:length(natinv_filt))
{
  print(u)
  D.overlap[[u]] <- natinv_filt[[u]]
  for(i in 1:length(nm)){
    D.overlap[[u]][[i]] <- ecospat.niche.overlap(grid.clim.nat[[u]][[i]],
                                                 grid.clim.inv[[u]][[i]], cor=T)$D 
    names(D.overlap[[u]]) <- nm
  }
}


#
D <- list()
for(u in seq(1, length(natinv_filt)))
{
  D[[u]] <- as.data.frame(do.call(rbind.data.frame, D.overlap[[u]]))
  colnames(D[[u]]) <- "D"
}


### (4b) Calculate niche expansion, niche unfilling and niche similarity
niche.dyn<- list()
for(u in 1:length(natinv_filt))
{
  print(u)
  niche.dyn[[u]] <- natinv_filt[[u]]
  for (i in 1:length(nm)){
    niche.dyn[[u]][[i]] <- ecospat.niche.dyn.index(grid.clim.nat[[u]][[i]],
                                                   grid.clim.inv[[u]][[i]],
                                                   intersection = 0.1)
  }
}


niche.dyn.indx <- list()
for(u in 1:length(natinv_filt))
{
  print(u)
  niche.dyn.indx[[u]] <- natinv_filt[[u]]
  for (i in 1:length(nm)){
    niche.dyn.indx[[u]][[i]] <- niche.dyn[[u]][[i]]$dynamic.index.w
    names(niche.dyn.indx[[u]]) <- nm
  }
}


indices <- list()
for(u in seq(1, length(natinv_filt)))
{
  indices[[u]] <- as.data.frame(do.call(rbind, niche.dyn.indx[[u]]))
}


###### 5. FINAL TABLE OF NICHE INDICES -----------------------------------------
# load table with information of the species
# columns and variables:
#       biom (Barbour and Billings, 1999):
#           BF - boreal forests
#           MF - mountain forests
#           DF - deciduous forests
#           CPF - coastal plain forests
#       area (native range size; Little, 1971)
#       mrt (minimum residence time; Krivanek, 2006 and other sources listed in Table S1.2)
#       id_biom:
#           BF - 1
#           MF - 2
#           DF - 3
#           CPF - 4

spe_info <- read.delim("spe_info.csv",
                       header = TRUE, sep = ",")
rownames(spe_info) <- spe_info$X
spe_info <- spe_info[,-c(1)]


# add niche indices
niche_index <- list()
for(u in seq(1, length(natinv_filt)))
{
  niche_index[[u]] <- cbind(D[[u]], round(indices[[u]],3))
  niche_index[[u]]$area <- spe_info$area
  niche_index[[u]]$biom <- spe_info$biom
  #niche_index[[u]]$absgeoexp <- area_reclass$geo_exp
  #niche_index[[u]]$relgeoexp <- area_reclass$geo_exp_rel
  niche_index[[u]]$mrt <- spe_info$mrt
}

# rename dataframes by filtering bins
nm_filt <- c("F0.001", "F0.005", "F0.01", "F0.05", "F0.1", "F0.5", "F1")
names(niche_index) <- nm_filt

