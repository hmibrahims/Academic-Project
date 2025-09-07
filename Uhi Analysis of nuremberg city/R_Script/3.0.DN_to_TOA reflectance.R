library(terra)

root      <- "E:/Course&Class/SFRS"
in_masked <- file.path(root, "_masked_full")
out_ref   <- file.path(root, "_toa_reflectance_masked"); dir.create(out_ref, FALSE, TRUE)

scenes <- list.dirs(in_masked, recursive = FALSE, full.names = FALSE)
scenes <- scenes[grepl("^20", scenes)]  # keep only real scene folders

read_vals_ref <- function(mtl){
  x <- readLines(mtl, warn = FALSE)
  pick <- function(p) sub(".*=\\s*", "", x[grep(p, x)][1])
  num  <- function(s) as.numeric(gsub("[^0-9Ee+.-]", "", s))
  getv <- function(tag, bs) setNames(sapply(bs, \(b)
                                            num(sub(".*=\\s*", "", x[grep(paste0("^\\s*", tag, b), x)][1])) ), paste0("B", bs))
  list(
    se    = num(pick("^\\s*SUN_ELEVATION")),
    multR = getv("REFLECTANCE_MULT_BAND_", 2:7),
    addR  = getv("REFLECTANCE_ADD_BAND_",  2:7)
  )
}

for (scn in scenes) {
  mtl <- list.files(file.path(root, scn), pattern = "_MTL\\.txt$", full.names = TRUE, ignore.case = TRUE)
  if (!length(mtl)) { cat("Skip (no MTL):", scn, "\n"); next }
  p  <- read_vals_ref(mtl[1]); se <- p$se * pi/180
  od <- file.path(out_ref, scn); dir.create(od, FALSE, TRUE)
  
  for (b in 2:7) {
    f <- list.files(file.path(in_masked, scn),
                    pattern = paste0("_B", b, "_masked\\.(tif|TIF)$"),
                    full.names = TRUE, ignore.case = TRUE)
    if (!length(f)) next
    r <- rast(f[1])                                # masked DN
    rho <- (p$multR[paste0("B", b)] * r + p$addR[paste0("B", b)]) / sin(se)
    rho <- clamp(rho, 0, 1.1); rho[r == 0] <- NA   # keep fill as NA
    
    out_name <- sub("_masked\\.(tif|TIF)$", "_toa.tif", basename(f[1]), ignore.case = TRUE)
    writeRaster(rho, file.path(od, out_name), overwrite = TRUE,
                wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=LZW", "TILED=YES")))
  }
  cat(scn, "→ TOA reflectance (B2–B7) saved.\n")
}
