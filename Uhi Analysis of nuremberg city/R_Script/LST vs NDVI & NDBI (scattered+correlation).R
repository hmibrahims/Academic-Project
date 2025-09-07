library(terra)

# --- PATHS ---
root <- "E:/Course&Class/SFRS"
aoi  <- vect(file.path(root, "n_shapefile", "AOI.shp"))

in_ndvi <- file.path(root, "_ndvi_clipped")
in_lst  <- file.path(root, "_lst_clipped")
in_ref  <- file.path(root, "_toa_reflectance_masked")  # for NDBI (B5,B6 TOA)

# --- SCENES (match by folder name, only those present in both NDVI & LST) ---
sc_ndvi <- list.dirs(in_ndvi, recursive=FALSE, full.names=FALSE)
sc_lst  <- list.dirs(in_lst,  recursive=FALSE, full.names=FALSE)
scenes  <- intersect(sc_ndvi, sc_lst)
scenes  <- scenes[grepl("^20", scenes)]
stopifnot(length(scenes) > 0)

# --- COLLECT SAMPLES ACROSS ALL YEARS ---
all_smp <- list()
per_scene_r <- data.frame(scene=character(), year=integer(),
                          r_LST_NDVI=numeric(), r_LST_NDBI=numeric(),
                          n=integer(), stringsAsFactors=FALSE)

for (scn in scenes) {
  # load NDVI & LST (clipped)
  ndvif <- list.files(file.path(in_ndvi, scn), pattern="NDVI.*_clipped\\.(tif|TIF)$",
                      full.names=TRUE, ignore.case=TRUE)
  lstf  <- list.files(file.path(in_lst,  scn), pattern="_LST_C_clipped\\.(tif|TIF)$",
                      full.names=TRUE, ignore.case=TRUE)
  if (!length(ndvif) || !length(lstf)) next
  ndvi <- rast(ndvif[1])
  lst  <- rast(lstf[1])
  
  # NDBI from TOA reflectance (B5=NIR, B6=SWIR1), clipped to AOI and aligned to LST
  refd <- file.path(in_ref, scn)
  f5 <- list.files(refd, pattern="_B5.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)
  f6 <- list.files(refd, pattern="_B6.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)
  if (!length(f5) || !length(f6)) next
  nir <- resample(mask(crop(rast(f5[1]), aoi), aoi), lst, method="bilinear")
  sw1 <- resample(mask(crop(rast(f6[1]), aoi), aoi), lst, method="bilinear")
  
  # align NDVI to LST grid
  if (!compareGeom(ndvi, lst, stopOnError=FALSE)) ndvi <- resample(ndvi, lst, method="bilinear")
  
  # indices
  ndbi <- clamp((sw1 - nir) / (sw1 + nir), -1, 1)
  
  # sample per scene (adjust size if needed)
  stk <- c(lst, ndvi, ndbi); names(stk) <- c("LST","NDVI","NDBI")
  smp <- spatSample(stk, size=4000, method="regular", na.rm=TRUE, as.data.frame=TRUE)
  if (nrow(smp) < 100) next
  
  smp$scene <- scn
  smp$year  <- as.integer(substr(scn,1,4))
  all_smp[[scn]] <- smp
  
  per_scene_r <- rbind(per_scene_r, data.frame(
    scene = scn,
    year  = smp$year[1],
    r_LST_NDVI = cor(smp$LST, smp$NDVI, use="complete.obs"),
    r_LST_NDBI = cor(smp$LST, smp$NDBI, use="complete.obs"),
    n = nrow(smp)
  ))
  cat(scn, "→ sampled", nrow(smp), "pixels.\n")
}

# pool all scenes
df <- do.call(rbind, all_smp)
stopifnot(nrow(df) > 0)

# overall correlations (12-year combined samples)
r_ndvi <- round(cor(df$LST, df$NDVI, use="complete.obs"), 3)
r_ndbi <- round(cor(df$LST, df$NDBI, use="complete.obs"), 3)
cat("\nOVERALL (all years): r(LST, NDVI) =", r_ndvi, " ; r(LST, NDBI) =", r_ndbi, "\n")
print(per_scene_r[order(per_scene_r$year),])

# --- PLOTS: combined 12-year scatters with r in title ---
op <- par(mfrow=c(1,2), mar=c(5,5,4,2))

# NDVI vs LST
plot(df$NDVI, df$LST, pch=16, cex=0.3, col=rgb(0,0.5,0,0.25),
     xlab="NDVI", ylab="LST (°C)",
     main=paste0("12 years: LST vs NDVI\nr = ", r_ndvi, "  (n = ", nrow(df), ")"))
abline(lm(LST ~ NDVI, data=df), col="red", lwd=2)

# NDBI vs LST
plot(df$NDBI, df$LST, pch=16, cex=0.3, col=rgb(0.6,0,0,0.25),
     xlab="NDBI", ylab="LST (°C)",
     main=paste0("12 years: LST vs NDBI\nr = ", r_ndbi, "  (n = ", nrow(df), ")"))
abline(lm(LST ~ NDBI, data=df), col="red", lwd=2)

par(op)

# OPTIONAL: save figure
# png(file.path(root, "_analysis", "LST_vs_NDVI_NDBI_all_years.png"), width=1400, height=600, res=130)
#   (repeat the two plot() calls above)
# dev.off()
