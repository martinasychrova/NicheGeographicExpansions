################################################################################
####                   QUANTIFICATION OF GEOGRAPHIC EXPANSION ----
####                               Calculations 
################################################################################

### 1. NAT+INV reclassified models ---------------------------------------------
# value 1 from reclassified models is replaced by 2
# to distinguish between NAT+INV reclass. models and NAT reclass. models
area_natinv <- list()
for (i in seq(1, length(nm)))
{
  print(i)
  area_natinv[[i]] <- mod_natinv_reclass[[i]]
  area_natinv[[i]][area_natinv[[i]] == 1] <- 2  # value 1 from reclassified models is replaced by 2
}

# calculate area of the potential distribution range (sum of cells with value 2)
area_natinv_df <- list()
for (i in seq(1, length(nm))){
  area_natinv_df[[i]] <- tapply(area(area_natinv[[i]]),
                             area_natinv[[i]][], sum)
  area_natinv_df[[i]] <- area_natinv_df[[i]] / 1000000
}

area_natinv_df <- do.call(rbind.data.frame, area_natinv_df)
rownames(area_natinv_df) <- nm
colnames(area_natinv_df) <- c("0", "2") #0 - absence, 2 - NAT+INV area of potential distribution range

### 2. NAT reclassified models ---------------------------------------------

# calculate area of the potential distribution range (sum of cells with value 1)
area_nat_df <- list()
for (i in seq(1, length(nm))){
  area_nat_df[[i]] <- tapply(area(mod_nat_reclass[[i]]),
                             mod_nat_reclass[[i]][], sum)
  area_nat_df[[i]] <- area_nat_df[[i]] / 1000000
}

area_nat_df <- do.call(rbind.data.frame, area_nat_df)
rownames(area_nat_df) <- nm
colnames(area_nat_df) <- c("0", "1") #0 = absence, 1 - NAT area of potential distribution range

### 3. QUANTIFICATION OF GEOGRAPHIC EXPANSION ----------------------------------
# overlay reclassified models over each other and sum the values of their cells
# identify areas of potential distribution range and quantify geographical expansion
area_overlay <- list()
for (i in seq(1, length(nm)))
{
  print(i)
  area_overlay[[i]] <- overlay(area_natinv[[i]],
                               mod_nat_reclass[[i]], fun = function(n)return(sum(n)))
  area_overlay[[i]] <- tapply(area(area_overlay[[i]]),
                              area_overlay[[i]][], sum)
  area_overlay[[i]] <- area_overlay[[i]] / 1000000
}

names(area_overlay) <- nm

# create dataframe with columns:
#     0 - area in which both models do not predict the distribution of species 
#     1 - area of the potential distribution range based on NAT models only
#     2 - area of the potential distribution range based on NAT+INV models (absolute geographic expansion) only
#     3 - area of the potential distribution range based on NAT and NAT+INV models
area_overlay_df <- plyr::rbind.fill(lapply(area_overlay,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
rownames(area_overlay_df) <- nm
area_overlay_df[is.na(area_overlay_df)] <- 0

# calculate relative geographical expansion
area_overlay_df$sum <- area_overlay_df$`2` + area_overlay_df$`3` + area_overlay_df$`1`
area_overlay_df$rel <- area_overlay_df$`2`/area_overlay_df$sum

# final table with potential distribution ranges
#     NAT - area of the potential distribution range based on NAT models + area calculated by both models
#     NATINV - area of the potential distribution range based on NAT+INV models + area calculated by both models
#     geo_exp - area of the potential distribution range based on NAT+INV models (absolute geographic expansion) only
area_reclass <- as.data.frame(area_overlay_df$`1` + area_overlay_df$`3`)
rownames(area_reclass) <- nm
area_reclass[,2] <- area_overlay_df$`2` + area_overlay_df$`3`
area_reclass[,3] <- area_overlay_df$`2`
colnames(area_reclass) <- c("NAT", "NATINV", "abs_geo_exp")

area_reclass$biom <- spe_info$biom #add info about biomes
area_reclass$species <- nm
area_reclass$geo_exp_rel <- area_geoexp_df$rel # add relative grographic expansion

# 4. FINAL TABLE WITH INDICES USED IN VISUALISATIONS ---------------------------
niche_index <- list()
for(u in seq(1, length(natinv_filt)))
{
  niche_index[[u]] <- cbind(D[[u]], round(indices[[u]],3))
  niche_index[[u]]$area <- spe_info$area
  niche_index[[u]]$biom <- spe_info$biom
  niche_index[[u]]$absgeoexp <- area_reclass$abs_geo_exp
  niche_index[[u]]$relgeoexp <- area_reclass$geo_exp_rel
  niche_index[[u]]$mrt <- spe_info$mrt
}

# rename dataframes by filtering bins
nm_filt <- c("F0.001", "F0.005", "F0.01", "F0.05", "F0.1", "F0.5", "F1")
names(niche_index) <- nm_filt

