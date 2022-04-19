################################################################################
####                   QUANTIFICATION OF GEOGRAPHIC EXPANSION ----
####                    Species distribution modelling
################################################################################

library(dismo)
library(rJava)

jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

### 1. NAT+INV MODELS ----------------------------------------------------------
# (1a) maxent models
sdm_natinv <- list()
for (i in seq(1, length(nm)))
{
  print(i)
  sdm_natinv[[i]] <- dismo::maxent(bio_natinv, as.matrix(natinv_filt$F0.001[[i]][,1:2]))
}

names(sdm_natinv) <- names(nm)

# (1b) projection to Europe
for(i in seq(1, length(sdm_natinv)))
{ 
  print(i)
  mod_natinv <- dismo::predict(sdm_natinv[[i]], bio_inv)
  writeRaster(mod_natinv, filename = paste(getwd(), "/Projection_NATINV/", names = nm[i], ".tif", sep=""), format="GTiff")
}

modpaths_mod_natinv <- list.files(path= paste(getwd(), "/Projection_NATINV/", sep=""), pattern='tif', full.names=TRUE )
mod_natinv <- stack(modpaths_mod_natinv)

names(mod_natinv) <- nm


### 2. NAT MODELS --------------------------------------------------------------
# (2a) maxent models
sdm_nat <- list()
for (i in seq(1, length(nm)))
{
  print(i)
  sdm_nat[[i]] <- dismo::maxent(bio_natinv, as.matrix(natinv_filt$F0.001[[i]]
                                                      [natinv_filt$F0.001[[i]]$Dataset == "GBIF_NAT"
                                                      | natinv_filt$F0.001[[i]]$Dataset == "Little",][,1:2]))
}

names(sdm_nat) <- names(nm)

# (2b) projection to Europe
for(i in seq(1, length(sdm_nat)))
{ 
  print(i)
  mod_nat <- dismo::predict(sdm_nat[[i]], bio_inv)
  writeRaster(mod_nat, filename = paste(getwd(), "/Projection_NAT/", names = nm[i], ".tif", sep=""), format="GTiff")
}

modpaths_mod_nat <- list.files(path= paste(getwd(), "/Projection_NAT/", sep=""), pattern='tif', full.names=TRUE )
mod_nat <- stack(modpaths_mod_nat)

names(mod_nat) <- nm


### 3. EVALUATIONS -------------------------------------------------------------

mod_eval <- list()
for(i in 1:length(nm))
  {
    print(i)
    ev <- ENMeval::ENMevaluate(occ=natinv_filt[,c(1,2)], env=bio_natinv, RMvalues = 1, 
                               fc = c("LQHPT"), method = 'randomkfold', kfolds = 5, 
                               bin.output = TRUE, rasterPreds = FALSE)
    mod_eval[[i]] <- ev@results
}

names(mod_eval) <- nm

mod_eval_df <- do.call(rbind.data.frame, mod_eval)
mod_eval_df


write.csv(mod_eval_df, file = "mod_eval_df.csv")
