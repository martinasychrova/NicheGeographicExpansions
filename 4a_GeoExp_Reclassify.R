################################################################################
####                   QUANTIFICATION OF GEOGRAPHIC EXPANSION ----
####                            Reclassify models 
################################################################################



### 1. NAT+INV MODELS ---------------------------------------------------------
# export threshold for each species (balance threshold)
mod_natinv_thr <- list()
for (i in seq(1, length(nm)))
{
  mod_natinv_thr[[i]] <- sdm_natinv[[i]]@results["Balance.training.omission..predicted.area.and.threshold.value.Cloglog.threshold",]
}

# reclassify models
# cells with value greater than threshold --> 1
# cells with value less than threshold --> 0
sdm_natinv_reclass <- list()
for (i in seq(1, length(nm)))
{
  print(i)
  sdm_natinv_reclass[[i]] <- reclassify(mod_natinv[[i]], c(0,mod_natinv_thr[[i]],0 ,mod_natinv_thr[[i]],Inf,1))
}

# export
for(i in seq(1, length(nm)))
{
  writeRaster(sdm_natinv_reclass[[i]], filename = paste(getwd(), "/Projection_NATINV_reclass/", names = nm [i], ".tif", sep=""), format="GTiff")
}

modpaths_mod_natinv_reclass <- list.files(path= paste(getwd(), "/Projection_NATINV_reclass/", sep=""), pattern='tif', full.names=TRUE )
mod_natinv_reclass <- stack(modpaths_mod_natinv_reclass)

names(mod_natinv_reclass) <- nm

### 2. NAT MODELS ---------------------------------------------------------
# export threshold for each species (balance threshold)
mod_nat_thr <- list()
for (i in seq(1, length(nm)))
{
  mod_nat_thr[[i]] <- sdm_nat[[i]]@results["Balance.training.omission..predicted.area.and.threshold.value.Cloglog.threshold",]
}

# reclassify models
# cells with value greater than threshold --> 1
# cells with value less than threshold --> 0
sdm_nat_reclass <- list()
for (i in seq(1, length(nm)))
{
  print(i)
  sdm_nat_reclass[[i]] <- reclassify(mod_nat[[i]], c(0,mod_nat_thr[[i]],0 ,mod_nat_thr[[i]],Inf,1))
}

# export
for(i in seq(1, length(nm)))
{
  writeRaster(sdm_nat_reclass[[i]], filename = paste(getwd(), "/Projection_NAT_reclass/", names = nm [i], ".tif", sep=""), format="GTiff")
}

modpaths_mod_nat_reclass <- list.files(path= paste(getwd(), "/Projection_NAT_reclass/", sep=""), pattern='tif', full.names=TRUE )
mod_nat_reclass <- stack(modpaths_mod_nat_reclass)

names(mod_nat_reclass) <- nm