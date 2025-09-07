library(terra)

root     <- "E:/Course&Class/SFRS"
in_ndvi  <- file.path(root, "_ndvi_masked")
out_ndvi <- file.path(root, "_ndvi_clipped"); dir.create(out_ndvi, FALSE, TRUE)

# load your AOI shapefile
aoi <- vect(file.path(root, "n_shapefile", "AOI.shp"))

# get scene folders
scenes <- list.dirs(in_ndvi, recursive = FALSE, full.names = FALSE)
scenes <- scenes[grepl("^20", scenes)]   # keep only real scenes

for (scn in scenes) {
  ndvif <- list.files(file.path(in_ndvi, scn), pattern="NDVI.*\\.(tif|TIF)$",
                      full.names=TRUE, ignore.case=TRUE)
  if (!length(ndvif)) { cat("Skip (no NDVI):", scn, "\n"); next }
  
  ndvi <- rast(ndvif[1])
  
  # clip + mask to AOI
  ndvi_clip <- mask(crop(ndvi, aoi), aoi)
  
  # save to new folder
  od <- file.path(out_ndvi, scn); dir.create(od, FALSE, TRUE)
  out_file <- file.path(od, paste0(scn, "_NDVI_clipped.tif"))
  writeRaster(ndvi_clip, out_file, overwrite=TRUE,
              wopt=list(datatype="FLT4S",
                        gdal=c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES")))
  
  cat(scn, "→ NDVI clipped saved.\n")
}
