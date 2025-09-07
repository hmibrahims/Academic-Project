library(terra)

root     <- "E:/Course&Class/SFRS"
in_ref   <- file.path(root, "_toa_reflectance_masked")
out_ndvi <- file.path(root, "_ndvi_masked"); dir.create(out_ndvi, FALSE, TRUE)

# scene folders like "2013-07-16_LC08_194_026"
scenes <- list.dirs(in_ref, recursive = FALSE, full.names = FALSE)
scenes <- scenes[grepl("^20", scenes)]   # keep only real scene names

for (scn in scenes) {
  ref_dir <- file.path(in_ref, scn)
  od      <- file.path(out_ndvi, scn); dir.create(od, FALSE, TRUE)
  
  # Find B4_toa and B5_toa (case-insensitive). Works with names like "..._B4_toa.tif"
  f4 <- list.files(ref_dir, pattern = "_B4.*_toa\\.(tif|TIF)$",
                   full.names = TRUE, ignore.case = TRUE)
  f5 <- list.files(ref_dir, pattern = "_B5.*_toa\\.(tif|TIF)$",
                   full.names = TRUE, ignore.case = TRUE)
  
  if (!length(f4) || !length(f5)) {
    cat("Skip (missing B4/B5 TOA):", scn, "\n")
    next
  }
  
  r4 <- rast(f4[1])
  r5 <- rast(f5[1])
  
  # Ensure identical grid (should be already, but just in case)
  if (!compareGeom(r4, r5, stopOnError = FALSE)) {
    r5 <- resample(r5, r4, method = "bilinear")
  }
  
  # NDVI = (NIR - RED) / (NIR + RED)
  sum_ <- r5 + r4
  ndvi <- (r5 - r4) / sum_
  
  # Avoid division artifacts
  ndvi[sum_ == 0] <- NA
  ndvi <- clamp(ndvi, -1, 1)
  
  # Save
  out_name <- file.path(od, paste0(scn, "_NDVI_toa_masked.tif"))
  writeRaster(ndvi, out_name, overwrite = TRUE,
              wopt = list(datatype = "FLT4S",
                          gdal = c("COMPRESS=LZW","TILED=YES")))
  cat(scn, "→ NDVI saved.\n")
}
