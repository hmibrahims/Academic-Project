library(terra)

root    <- "E:/Course&Class/SFRS"
in_bt   <- file.path(root, "_toa_radiance_masked")   # contains <scene>/<...>_BT.tif
out_bt  <- file.path(root, "_bt_clipped"); dir.create(out_bt, FALSE, TRUE)

# AOI shapefile
aoi <- vect(file.path(root, "n_shapefile", "AOI.shp"))

# scene folders
scenes <- list.dirs(in_bt, recursive = FALSE, full.names = FALSE)
scenes <- scenes[grepl("^20", scenes)]  # keep only real scenes

for (scn in scenes) {
  btf <- list.files(file.path(in_bt, scn),
                    pattern = "_BT\\.(tif|TIF)$",
                    full.names = TRUE, ignore.case = TRUE)
  if (!length(btf)) { cat("Skip (no BT):", scn, "\n"); next }
  
  BT <- rast(btf[1])             # BT is in Kelvin from your earlier step
  BT_clip <- mask(crop(BT, aoi), aoi)
  
  od <- file.path(out_bt, scn); dir.create(od, FALSE, TRUE)
  out_file <- file.path(od, paste0(scn, "_BT_clipped.tif"))
  writeRaster(BT_clip, out_file, overwrite = TRUE,
              wopt = list(datatype = "FLT4S",
                          gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES")))
  cat(scn, "→ BT clipped saved.\n")
}
