library(terra)

root <- "E:/Course&Class/SFRS"
aoi  <- vect(file.path(root, "n_shapefile", "AOI.shp"))

# find scene folders that exist for both NDVI (clipped) and LST (clipped)
sc_ndvi <- list.dirs(file.path(root, "_ndvi_clipped"), recursive=FALSE, full.names=FALSE)
sc_lst  <- list.dirs(file.path(root, "_lst_clipped"),  recursive=FALSE, full.names=FALSE)
scenes  <- intersect(sc_ndvi, sc_lst)
scenes  <- scenes[grepl("^20", scenes)]

# helper: pick scenes at 4-year intervals (e.g., 2013, 2017, 2021, 2025)
years   <- sort(unique(as.integer(substr(scenes, 1, 4))))
targets <- seq(min(years), max(years), by=4)
scenes4 <- unlist(lapply(targets, function(y) {
  s <- scenes[substr(scenes, 1, 4) == as.character(y)]
  if (length(s)) s[1] else NA
}))
scenes4 <- scenes4[!is.na(scenes4)]
if (length(scenes4) < 2) stop("Not enough scenes at 4-year spacing.")

# --- index loaders ---
get_ndvi <- function(scene) {
  lst <- rast(file.path(root, "_lst_clipped", scene, paste0(scene, "_LST_C_clipped.tif")))
  nd  <- rast(file.path(root, "_ndvi_clipped", scene, paste0(scene, "_NDVI_clipped.tif")))
  nd  <- resample(nd, lst, method="bilinear")
  list(idx=nd, lst=lst, name="NDVI")
}
get_ndbi <- function(scene) {
  lst <- rast(file.path(root, "_lst_clipped", scene, paste0(scene, "_LST_C_clipped.tif")))
  refd <- file.path(root, "_toa_reflectance_masked", scene)
  nir <- rast(list.files(refd, pattern="_B5.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)[1])
  sw1 <- rast(list.files(refd, pattern="_B6.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)[1])
  nir <- resample(mask(crop(nir, aoi), aoi), lst, method="bilinear")
  sw1 <- resample(mask(crop(sw1, aoi), aoi), lst, method="bilinear")
  idx <- clamp((sw1 - nir)/(sw1 + nir), -1, 1)
  list(idx=idx, lst=lst, name="NDBI")
}

plot_4yr_index_vs_lst <- function(scenes4, index=c("NDVI","NDBI"), png_path=NULL, sample_n=4000) {
  index <- match.arg(index)
  fetch <- if (index=="NDVI") get_ndvi else get_ndbi
  
  if (!is.null(png_path)) png(png_path, width=1200, height=900, res=130)
  op <- par(mfrow=c(2,2), mar=c(4,4,3,1))
  
  for (scn in scenes4) {
    dat <- fetch(scn)
    stk <- c(dat$lst, dat$idx); names(stk) <- c("LST","IDX")
    smp <- spatSample(stk, size=sample_n, method="regular", na.rm=TRUE, as.data.frame=TRUE)
    r   <- round(cor(smp$LST, smp$IDX, use="complete.obs"), 2)
    
    colp <- if (index=="NDVI") rgb(0,0.5,0,0.35) else rgb(0.6,0,0,0.35)
    plot(smp$IDX, smp$LST, pch=16, cex=0.35, col=colp,
         xlab=index, ylab="LST (°C)",
         main=paste0(scn, "\n r = ", ifelse(r>=0, "+",""), r))
    abline(lm(LST ~ IDX, data=smp), col="red", lwd=2)
  }
  
  par(op)
  if (!is.null(png_path)) dev.off()
}

# ---------- RUN IT ----------
# NDVI vs LST for 4-year interval scenes
plot_4yr_index_vs_lst(scenes4, index="NDVI",
                      png_path=file.path(root, "_analysis", "NDVI_vs_LST_4yr.png"))

# NDBI vs LST for the same 4-year interval scenes
plot_4yr_index_vs_lst(scenes4, index="NDBI",
                      png_path=file.path(root, "_analysis", "NDBI_vs_LST_4yr.png"))

