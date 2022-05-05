################################################################################
####                        KRUSKAL-WALLIS TESTS ----
################################################################################

### FIGURE 1 -------------------------------------------------------------------
# niche overlap
kw_d <- list()
for(u in seq(1, length(natinv_filt)))
{
  kw_d[[u]] <- kruskal.test(niche_index[[u]]$D, niche_index[[u]]$biom)
}

names(kw_d) <- nm_filt

# niche expansion
kw_exp <- list()
for(u in seq(1, length(natinv_filt)))
{
  kw_exp[[u]] <- kruskal.test(niche_index[[u]]$expansion, niche_index[[u]]$biom)
}

names(kw_exp) <- nm_filt

# niche unfilling
kw_unf <- list()
for(u in seq(1, length(natinv_filt)))
{
  kw_unf[[u]] <- kruskal.test(niche_index[[u]]$unfilling, niche_index[[u]]$biom)
}

names(kw_unf) <- nm_filt

### FUGURE 2 -------------------------------------------------------------------
kw_NATINV <- kruskal.test(area_reclass$NATINV ~ area_reclass$biom)

kw_NAT <- kruskal.test(area_reclass$NAT ~ area_reclass$biom)

kw_geoexp_abs <- kruskal.test(area_reclass$abs_geo_exp ~ area_reclass$biom)

kw_geoexp_rel <- kruskal.test(area_reclass$rel_geo_exp ~ area_reclass$biom)
