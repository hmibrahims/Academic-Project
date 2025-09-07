library(terra)

root <- "E:/Course&Class/SFRS"
out_root <- file.path(root, "_masked_full")
dir.create(out_root, showWarnings = FALSE)

# scene list = all folders you showed
scenes <- list.dirs(root, recursive = FALSE, full.names = TRUE)

# --- helper (QA_PIXEL only, since your data are Landsat 8 Collection 2) ---
isbit <- function(x, bit) bitwAnd(x, bitwShiftL(1L, bit)) != 0L
qa_mask_pixel <- function(qa){
  fill    <- app(qa, isbit, 0)
  dcloud  <- app(qa, isbit, 1)
  cirrus  <- app(qa, isbit, 2)
  cloud   <- app(qa, isbit, 3)
  shadow  <- app(qa, isbit, 4)
  snow    <- app(qa, isbit, 5)
  water   <- app(qa, isbit, 7)
  fill | dcloud | cirrus | cloud | shadow | snow | water
}

# --- loop all scenes ---
for (scn_dir in scenes) {
  scn <- basename(scn_dir)
  out_dir <- file.path(out_root, scn)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # find QA band
  qa_file <- list.files(scn_dir, pattern="QA_PIXEL\\.(tif|TIF)$", full.names=TRUE)
  if (length(qa_file)==0) {
    cat("No QA_PIXEL found:", scn, "\n")
    next
  }
  qa <- rast(qa_file)
  bad <- qa_mask_pixel(qa)
  
  # mask bands B2–B7 and B10
  bands <- list.files(scn_dir, pattern="_B(2|3|4|5|6|7|10)\\.(tif|TIF)$", full.names=TRUE)
  for (bf in bands) {
    r <- rast(bf)
    bad_r <- resample(bad, r, method="near")
    r_masked <- mask(r, bad_r, maskvalues=1, updatevalue=NA)
    out_name <- paste0(tools::file_path_sans_ext(basename(bf)), "_masked.tif")
    writeRaster(r_masked, file.path(out_dir, out_name), overwrite=TRUE)
  }
  cat(scn, "masked.\n")
}
