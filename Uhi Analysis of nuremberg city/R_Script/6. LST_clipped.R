library(terra)

root        <- "E:/Course&Class/SFRS"
in_bt       <- file.path(root, "_bt_clipped")            # <scene>/<scene>_BT_clipped.tif (Kelvin)
in_emis     <- file.path(root, "_emissivity_clipped")    # <scene>/<scene>_emissivity_clipped.tif
out_lst     <- file.path(root, "_lst_clipped"); dir.create(out_lst, FALSE, TRUE)

# scenes present in both inputs
scenes <- intersect(
  list.dirs(in_bt,   recursive = FALSE, full.names = FALSE),
  list.dirs(in_emis, recursive = FALSE, full.names = FALSE)
)
scenes <- scenes[grepl("^20", scenes)]

# constants for emissivity correction
lambda <- 10.895e-6  # m (Landsat 8 B10 center)
c2     <- 1.4388e-2  # m*K

for (scn in scenes) {
  btf <- list.files(file.path(in_bt, scn),
                    pattern = "_BT_clipped\\.(tif|TIF)$",
                    full.names = TRUE, ignore.case = TRUE)
  ef  <- list.files(file.path(in_emis, scn),
                    pattern = "_emissivity_clipped\\.(tif|TIF)$",
                    full.names = TRUE, ignore.case = TRUE)
  if (!length(btf) || !length(ef)) { cat("Skip (missing BT or emissivity):", scn, "\n"); next }
  
  BT   <- rast(btf[1])     # Kelvin
  emis <- rast(ef[1])      # unitless (0-1)
  
  # align grids if needed
  if (!compareGeom(BT, emis, stopOnError = FALSE)) {
    emis <- resample(emis, BT, method = "bilinear")
  }
  
  # avoid log(<=0)
  emis[emis <= 0] <- NA
  
  # LST (Kelvin) with emissivity-only correction, then °C
  LSTk <- BT / (1 + (lambda * BT / c2) * log(emis))
  LSTc <- LSTk - 273.15
  
  od <- file.path(out_lst, scn); dir.create(od, FALSE, TRUE)
  out <- file.path(od, paste0(scn, "_LST_C_clipped.tif"))
  writeRaster(LSTc, out, overwrite = TRUE,
              wopt = list(datatype = "FLT4S",
                          gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES")))
  cat(scn, "→ LST (°C, clipped) saved.\n")
}
