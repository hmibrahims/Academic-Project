library(terra)

root <- "E:/Course&Class/SFRS"
aoi  <- vect(file.path(root, "n_shapefile", "AOI.shp"))

# scenes that exist in BOTH NDVI_clipped and LST_clipped
sc_ndvi <- list.dirs(file.path(root, "_ndvi_clipped"), recursive=FALSE, full.names=FALSE)
sc_lst  <- list.dirs(file.path(root, "_lst_clipped"),  recursive=FALSE, full.names=FALSE)
scenes  <- intersect(sc_ndvi, sc_lst)
scenes  <- scenes[grepl("^20", scenes)]
scenes  <- scenes[order(scenes)]   # nice ordering

# ---- index loaders ----
get_ndvi <- function(scene) {
  lst <- rast(file.path(root, "_lst_clipped",  scene, paste0(scene, "_LST_C_clipped.tif")))
  nd  <- rast(file.path(root, "_ndvi_clipped", scene, paste0(scene, "_NDVI_clipped.tif")))
  nd  <- resample(nd, lst, method="bilinear")
  list(idx=nd, lst=lst, name="NDVI")
}
get_ndbi <- function(scene) {
  lst  <- rast(file.path(root, "_lst_clipped", scene, paste0(scene, "_LST_C_clipped.tif")))
  refd <- file.path(root, "_toa_reflectance_masked", scene)
  nir  <- rast(list.files(refd, pattern="_B5.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)[1]) # NIR
  sw1  <- rast(list.files(refd, pattern="_B6.*_toa\\.(tif|TIF)$", full.names=TRUE, ignore.case=TRUE)[1]) # SWIR1
  nir  <- resample(mask(crop(nir,  aoi), aoi), lst, method="bilinear")
  sw1  <- resample(mask(crop(sw1,  aoi), aoi), lst, method="bilinear")
  idx  <- clamp((sw1 - nir)/(sw1 + nir), -1, 1)
  list(idx=idx, lst=lst, name="NDBI")
}

# ---- plot ALL scenes in one figure ----
plot_all_scenes_index_vs_lst <- function(scenes, index=c("NDVI","NDBI"),
                                         png_path=NULL, sample_n=4000,
                                         nrow=3, ncol=4) {
  index <- match.arg(index)
  fetch <- if (index=="NDVI") get_ndvi else get_ndbi
  if (!length(scenes)) stop("No scenes found.")
  
  if (!is.null(png_path)) png(png_path, width=1600, height=1200, res=130)
  op <- par(mfrow=c(nrow, ncol), mar=c(4,4,3,1))
  
  colp <- if (index=="NDVI") rgb(0,0.5,0,0.35) else rgb(0.6,0,0,0.35)
  
  for (scn in scenes) {
    dat <- fetch(scn)
    stk <- c(dat$lst, dat$idx); names(stk) <- c("LST","IDX")
    smp <- spatSample(stk, size=sample_n, method="regular", na.rm=TRUE, as.data.frame=TRUE)
    if (nrow(smp) < 50) { plot.new(); title(main=paste0(scn,"\n(no data)")); next }
    r   <- round(cor(smp$LST, smp$IDX, use="complete.obs"), 2)
    
    plot(smp$IDX, smp$LST, pch=16, cex=0.35, col=colp,
         xlab=index, ylab="LST (°C)",
         main=paste0(scn, "\n r = ", ifelse(r>=0, "+",""), r))
    abline(lm(LST ~ IDX, data=smp), col="red", lwd=2)
  }
  
  par(op)
  if (!is.null(png_path)) dev.off()
}

# ------- RUN (all 12 scenes in one PNG each) -------
dir.create(file.path(root, "_analysis"), showWarnings=FALSE, recursive=TRUE)

plot_all_scenes_index_vs_lst(
  scenes, index="NDVI",
  png_path=file.path(root, "_analysis", "NDVI_vs_LST_all_scenes.png")
)

plot_all_scenes_index_vs_lst(
  scenes, index="NDBI",
  png_path=file.path(root, "_analysis", "NDBI_vs_LST_all_scenes.png")
)

