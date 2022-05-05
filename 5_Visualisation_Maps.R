################################################################################
####                        VISUALISATION OF MAPS ----
################################################################################

### 1. OVERLAY OF MODELS FOR BIOMES ---------------------------------------------
### (1a) NATINV models --------------------------------------

mod_overlay_natinv <- overlay(mod_natinv, fun = function(n)return(mean(n)))
names(mod_overlay_natinv) <- "NATINV"

# BF
BF <- rownames(spe_info[spe_info$biom == "BF",])
mod_overlay_natinv_BF <- overlay(mod_natinv[[BF]], fun = function(n)return(mean(n)))
names(mod_overlay_natinv_BF) <- "NATINV_BF"

# MF
MF <- rownames(spe_info[spe_info$biom == "MF",])
mod_overlay_natinv_MF <- overlay(mod_natinv[[MF]], fun = function(n)return(mean(n)))
names(mod_overlay_natinv_BF) <- "NATINV_MF"

# DF
DF <- rownames(spe_info[spe_info$biom == "DF",])
mod_overlay_natinv_DF <- overlay(mod_natinv[[DF]], fun = function(n)return(mean(n)))
names(mod_overlay_natinv_DF) <- "NATINV_DF"

# CPF
CPF <- rownames(spe_info[spe_info$biom == "CPF",])
mod_overlay_natinv_CPF <- overlay(mod_natinv[[CPF]], fun = function(n)return(mean(n)))
names(mod_overlay_natinv_CPF) <- "NATINV_CPF"

#
final_NATINV_overlay <- stack(mod_overlay_natinv,
                              mod_overlay_natinv_BF,
                              mod_overlay_natinv_MF,
                              mod_overlay_natinv_CPF,
                              mod_overlay_natinv_DF)

### (1b) NAT models -----------------------------------------
mod_overlay_nat <- overlay(mod_nat, fun = function(n)return(mean(n)))
names(mod_overlay_nat) <- "NAT"

# BF
mod_overlay_nat_BF <- overlay(mod_nat[[BF]], fun = function(n)return(mean(n)))
names(mod_overlay_nat_BF) <- "NAT_BF"

# MF
mod_overlay_nat_MF <- overlay(mod_nat[[CMF]], fun = function(n)return(mean(n)))
names(mod_overlay_nat_MF) <- "NAT_MF"

# DF
mod_overlay_nat_DF <- overlay(mod_nat[[DF]], fun = function(n)return(mean(n)))
names(mod_overlay_nat_DF) <- "NAT_DF"

# CPF
mod_overlay_nat_CPF <- overlay(mod_nat[[CPF]], fun = function(n)return(mean(n)))
names(mod_overlay_nat_CPF) <- "NAT_CPF"

#
final_NAT_overlay <- stack(mod_overlay_nat,
                           mod_overlay_nat_BF,
                           mod_overlay_nat_MF,
                           mod_overlay_nat_CPF,
                           mod_overlay_nat_DF)


### 2. OVERLAY OF MAXIMUM OCCUPANCY MAPS (RECLASSIFIED MODELS) FOR BIOMES ------
### (2a) Reclassified NATINV models --------------------------------------

mod_overlay_natinv_reclass <- overlay(mod_natinv, fun = function(n)return(sum(n)))
names(mod_overlay_natinv_reclass) <- "NATINV"

# BF
mod_overlay_natinv_BF_reclass <- overlay(mod_natinv_reclass[[BF]], fun = function(n)return(sum(n)))
names(mod_overlay_natinv_BF_reclass) <- "NATINV_BF"

# MF
mod_overlay_natinv_MF_reclass <- overlay(mod_natinv_reclass[[MF]], fun = function(n)return(sum(n)))
names(mod_overlay_natinv_BF_reclass) <- "NATINV_MF"

# DF
mod_overlay_natinv_DF_reclass <- overlay(mod_natinv_reclass[[DF]], fun = function(n)return(sum(n)))
names(mod_overlay_natinv_DF_reclass) <- "NATINV_DF"

# CPF
mod_overlay_natinv_CPF_reclass <- overlay(mod_natinv_reclass[[CPF]], fun = function(n)return(sum(n)))
names(mod_overlay_natinv_CPF_reclass) <- "NATINV_CPF"

#
final_NATINV_overlay_reclass <- stack(mod_overlay_natinv_reclass,
                              mod_overlay_natinv_BF_reclass,
                              mod_overlay_natinv_MF_reclass,
                              mod_overlay_natinv_CPF_reclass,
                              mod_overlay_natinv_DF_reclass)

### (2b) Reclassified NAT models -----------------------------------------
mod_overlay_nat_reclass <- overlay(mod_nat_reclass, fun = function(n)return(sum(n)))
names(mod_overlay_nat_reclass) <- "NAT"

# BF
mod_overlay_nat_BF_reclass <- overlay(mod_nat_reclass[[BF]], fun = function(n)return(sum(n)))
names(mod_overlay_nat_BF_reclass) <- "NAT_BF"

# MF
mod_overlay_nat_MF_reclass <- overlay(mod_nat_reclass[[CMF]], fun = function(n)return(sum(n)))
names(mod_overlay_nat_MF_reclass) <- "NAT_MF"

# DF
mod_overlay_nat_DF_reclass <- overlay(mod_nat_reclass[[DF]], fun = function(n)return(sum(n)))
names(mod_overlay_nat_DF_reclass) <- "NAT_DF"

# CPF
mod_overlay_nat_CPF_reclass <- overlay(mod_nat_reclass[[CPF]], fun = function(n)return(sum(n)))
names(mod_overlay_nat_CPF_reclass) <- "NAT_CPF"

#
final_NAT_overlay_reclass <- stack(mod_overlay_nat_reclass,
                           mod_overlay_nat_BF_reclass,
                           mod_overlay_nat_MF_reclass,
                           mod_overlay_nat_CPF_reclass,
                           mod_overlay_nat_DF_reclass)

#
final_models <- stack(final_NATINV_overlay, final_NAT_overlay)
final_models_reclass <- stack(final_NATINV_overlay_reclass, final_NAT_overlay_reclass)


### 3. VISUALISATION OF MAPS --------------------------------------------------
### (3a) Visualisation of models -------------------------------
tiff("final_models.tiff", height = 26, width = 21, units = "cm", res = 400, compression = "lzw")
par(mar = c(0,0,0.5,0), mfcol = c(5,2))
tran <- list(6,1,7,2,8,3,9,4,10,5)

for (i in 1:10)
{
  par(mar = c(0,0,0,0))
  plot(final_models[[tran[i]]], legend = F, axes = F, col = colp, box = F,
       breaks=seq(min(minValue(final_models)),max(maxValue(final_models)),length.out=100))
}
dev.off()

### (3b) Visualisation of reclassified models ---------------------------
tiff("final_models_reclass.tiff",height = 26, width = 21, units = "cm", res = 400, compression = "lzw" )
par(mar = c(0,0,0.5,0), mfcol = c(5,1))
for(i in 1:5){
  par(mar = c(0,0,0,0))
  plot(final_NATINV_overlay_reclass[[i]] >= 1, col = c("lightblue", "darkred"), breaks = c(0,0.99,1), legend = F, axes = F, box = F)
  plot(final_NAT_overlay_reclass[[i]] >= 1, col = "burlywood1", breaks = c(0.99,1), legend = F, add = T, axes = F, box = F)
}

dev.off()
