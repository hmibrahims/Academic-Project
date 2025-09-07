library(terra)
library(stats)

# ---- SETUP (adjust if not already in your session) ----
root   <- "E:/Course&Class/SFRS"
aoi    <- vect(file.path(root, "n_shapefile", "AOI.shp"))
in_ndvi <- file.path(root, "_ndvi_clipped")
in_lst  <- file.path(root, "_lst_clipped")
in_ref  <- file.path(root, "_toa_reflectance_masked")  # for NDBI/NDWI
scenes <- intersect(
  list.dirs(in_ndvi, recursive=FALSE, full.names=FALSE),
  list.dirs(in_lst,  recursive=FALSE, full.names=FALSE)
)
scenes <- scenes[grepl("^20", scenes)]
# -------------------------------------------------------

# compute NDBI/NDWI on the fly, then correlate with LST
out_idx <- file.path(root, "_indices_clipped"); dir.create(out_idx, FALSE, TRUE)
out_csv <- file.path(root, "_analysis", "corr_summary.csv"); dir.create(dirname(out_csv), FALSE, TRUE)

res <- data.frame(
  scene=character(), year=integer(),
  r_LST_NDVI=numeric(), r_LST_NDBI=numeric(), r_LST_NDWI=numeric(),
  n=integer(), stringsAsFactors=FALSE
)

for (scn in scenes) {
  # NDVI + LST (already clipped)
  ndvif <- list.files(file.path(in_ndvi, scn), pattern="NDVI.*_clipped\\.(tif|TIF)$",
                      full.names=TRUE, ignore.case=TRUE)
  lstf  <- list.files(file.path(in_lst,  scn), pattern="_LST_C_clipped\\.(tif|TIF)$",
                      full.names=TRUE, ignore.case=TRUE)
  if(!length(ndvif) || !length(lstf)) { cat("Skip (missing NDVI/LST):", scn, "\n"); next }
  ndvi <- rast(ndvif[1]); lst <- rast(lstf[1])
  
  # NDBI & NDWI from TOA reflectance (mask & clip to AOI)
  ref_dir <- file.path(in_ref, scn)
  f3 <- list.files(ref_dir, pattern="_B3.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)
  f5 <- list.files(ref_dir, pattern="_B5.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)
  f6 <- list.files(ref_dir, pattern="_B6.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)
  if(!length(f3) || !length(f5) || !length(f6)) { cat("Skip (missing B3/B5/B6):", scn, "\n"); next }
  
  g   <- mask(crop(rast(f3[1]), aoi), aoi)
  nir <- mask(crop(rast(f5[1]), aoi), aoi)
  sw1 <- mask(crop(rast(f6[1]), aoi), aoi)
  
  # align all to LST grid
  if(!compareGeom(ndvi, lst, stopOnError=FALSE)) ndvi <- resample(ndvi, lst, method="bilinear")
  if(!compareGeom(nir,  lst, stopOnError=FALSE)) nir  <- resample(nir,  lst, method="bilinear")
  if(!compareGeom(sw1,  lst, stopOnError=FALSE)) sw1  <- resample(sw1,  lst, method="bilinear")
  if(!compareGeom(g,    lst, stopOnError=FALSE)) g    <- resample(g,    lst, method="bilinear")
  
  # indices
  ndbi <- (sw1 - nir) / (sw1 + nir); ndbi <- clamp(ndbi, -1, 1)
  ndwi <- (g   - nir) / (g   + nir); ndwi <- clamp(ndwi, -1, 1)
  
  # sample pixels for correlation
  stk <- c(lst, ndvi, ndbi, ndwi); names(stk) <- c("LST","NDVI","NDBI","NDWI")
  smp <- spatSample(stk, size=10000, method="regular", na.rm=TRUE, as.data.frame=TRUE)
  if(nrow(smp) < 100) { cat("Too few samples:", scn, "\n"); next }
  
  res <- rbind(res, data.frame(
    scene = scn,
    year  = as.integer(substr(scn, 1, 4)),
    r_LST_NDVI = cor(smp$LST, smp$NDVI, use="complete.obs"),
    r_LST_NDBI = cor(smp$LST, smp$NDBI, use="complete.obs"),
    r_LST_NDWI = cor(smp$LST, smp$NDWI, use="complete.obs"),
    n = as.integer(nrow(smp))
  ))
  cat(scn, "→ correlations computed.\n")
}

write.csv(res, out_csv, row.names=FALSE)
cat("Saved:", out_csv, "\n")
