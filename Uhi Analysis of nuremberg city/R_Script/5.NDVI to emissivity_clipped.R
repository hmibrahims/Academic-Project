library(terra)

root       <- "E:/Course&Class/SFRS"
in_ndvi    <- file.path(root, "_ndvi_clipped")                 # has <scene>/<scene>_NDVI_clipped.tif
out_emis   <- file.path(root, "_emissivity_clipped"); dir.create(out_emis, FALSE, TRUE)

# scene folders like "2013-07-16_LC08_194_026"
scenes <- list.dirs(in_ndvi, recursive = FALSE, full.names = FALSE)
scenes <- scenes[grepl("^20", scenes)]

for (scn in scenes) {
  ndvif <- list.files(file.path(in_ndvi, scn),
                      pattern = "NDVI.*_clipped\\.(tif|TIF)$",
                      full.names = TRUE, ignore.case = TRUE)
  if (!length(ndvif)) { cat("Skip (no NDVI clipped):", scn, "\n"); next }
  
  ndvi <- rast(ndvif[1])
  
  # Emissivity via NDVI-threshold method
  # NDVI < 0.2 -> 0.970 ; NDVI > 0.5 -> 0.990 ; else use Pv-based formula
  emis <- classify(ndvi, rcl = matrix(c(-Inf,0.2,0.970,
                                        0.5, Inf,0.990), ncol = 3, byrow = TRUE))
  mid  <- ndvi >= 0.2 & ndvi <= 0.5
  Pv   <- ((ndvi - 0.2) / 0.3)^2
  emis[mid] <- 0.004 * Pv[mid] + 0.986   # (+0.005 roughness term if desired)
  
  # Optional: water mask (NDVI < 0)
  emis[ndvi < 0] <- NA
  
  # Save emissivity (float32, compressed)
  od <- file.path(out_emis, scn); dir.create(od, FALSE, TRUE)
  out_file <- file.path(od, paste0(scn, "_emissivity_clipped.tif"))
  writeRaster(emis, out_file, overwrite = TRUE,
              wopt = list(datatype = "FLT4S",
                          gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES")))
  cat(scn, " → emissivity (clipped) saved.\n")
}
