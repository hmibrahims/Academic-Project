library(terra)

root      <- "E:/Course&Class/SFRS"
in_masked <- file.path(root, "_masked_full")
out_rad   <- file.path(root, "_toa_radiance_masked"); dir.create(out_rad, FALSE, TRUE)

scenes <- list.dirs(in_masked, recursive = FALSE, full.names = FALSE)
scenes <- scenes[grepl("^20", scenes)]

read_vals_therm <- function(mtl){
  x <- readLines(mtl, warn = FALSE)
  pick <- function(p) sub(".*=\\s*", "", x[grep(p, x)][1])
  num  <- function(s) as.numeric(gsub("[^0-9Ee+.-]", "", s))
  list(
    mult = num(pick("^\\s*RADIANCE_MULT_BAND_10")),
    add  = num(pick("^\\s*RADIANCE_ADD_BAND_10")),
    K1   = num(pick("^\\s*K1_CONSTANT_BAND_10")),
    K2   = num(pick("^\\s*K2_CONSTANT_BAND_10"))
  )
}

for (scn in scenes) {
  mtl <- list.files(file.path(root, scn), pattern = "_MTL\\.txt$", full.names = TRUE, ignore.case = TRUE)
  if (!length(mtl)) { cat("Skip (no MTL):", scn, "\n"); next }
  p  <- read_vals_therm(mtl[1])
  od <- file.path(out_rad, scn); dir.create(od, FALSE, TRUE)
  
  f10 <- list.files(file.path(in_masked, scn),
                    pattern = "_B10_masked\\.(tif|TIF)$",
                    full.names = TRUE, ignore.case = TRUE)
  if (!length(f10)) { cat("Skip (no B10 masked):", scn, "\n"); next }
  
  r  <- rast(f10[1])               # masked DN
  L  <- p$mult * r + p$add         # TOA radiance
  BT <- p$K2 / log((p$K1 / L) + 1) # Brightness Temperature (Kelvin)
  
  writeRaster(L,  file.path(od, sub("_masked\\.(tif|TIF)$", "_rad_toa.tif", basename(f10[1]), ignore.case=TRUE)),
              overwrite=TRUE, wopt=list(datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES")))
  writeRaster(BT, file.path(od, sub("_masked\\.(tif|TIF)$", "_BT.tif",       basename(f10[1]), ignore.case=TRUE)),
              overwrite=TRUE, wopt=list(datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES")))
  
  cat(scn, "→ TOA radiance & BT (B10) saved.\n")
}
