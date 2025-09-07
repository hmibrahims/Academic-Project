library(terra)

root   <- "E:/Course&Class/SFRS"
aoi    <- vect(file.path(root, "n_shapefile", "AOI.shp"))

# inputs you already created
in_ndvi <- file.path(root, "_ndvi_clipped")
in_lst  <- file.path(root, "_lst_clipped")

# we’ll derive NDBI/NDWI from TOA reflectance on-the-fly, then clip to AOI
in_ref  <- file.path(root, "_toa_reflectance_masked")

scenes <- intersect(
  list.dirs(in_ndvi, recursive=FALSE, full.names=FALSE),
  list.dirs(in_lst,  recursive=FALSE, full.names=FALSE)
)
scenes <- scenes[grepl("^20", scenes)]
#--------------------------------------------------------------



out_idx <- file.path(root, "_indices_clipped"); dir.create(out_idx, FALSE, TRUE)

for (scn in scenes) {
  ref_dir <- file.path(in_ref, scn)
  f3 <- list.files(ref_dir, pattern="_B3.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)
  f5 <- list.files(ref_dir, pattern="_B5.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)
  f6 <- list.files(ref_dir, pattern="_B6.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)
  if(!length(f3) || !length(f5) || !length(f6)){ cat("Skip (missing B3/B5/B6):", scn, "\n"); next }
  
  g <- rast(f3[1]); nir <- rast(f5[1]); sw1 <- rast(f6[1])
  # clip to AOI (same as your NDVI/LST)
  g <- mask(crop(g, aoi), aoi)
  nir <- mask(crop(nir, aoi), aoi)
  sw1 <- mask(crop(sw1, aoi), aoi)
  
  if(!compareGeom(nir, g, stopOnError=FALSE)) { nir <- resample(nir, g, method="bilinear") }
  if(!compareGeom(sw1, g, stopOnError=FALSE)) { sw1 <- resample(sw1, g, method="bilinear") }
  
  ndbi <- (sw1 - nir) / (sw1 + nir); ndbi <- clamp(ndbi, -1, 1)
  ndwi <- (g   - nir) / (g   + nir); ndwi <- clamp(ndwi, -1, 1)
  
  od <- file.path(out_idx, scn); dir.create(od, FALSE, TRUE)
  writeRaster(ndbi, file.path(od, paste0(scn, "_NDBI_clipped.tif")),
              overwrite=TRUE, wopt=list(datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES")))
  writeRaster(ndwi, file.path(od, paste0(scn, "_NDWI_clipped.tif")),
              overwrite=TRUE, wopt=list(datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES")))
  cat(scn, "→ NDBI & NDWI saved.\n")
}







