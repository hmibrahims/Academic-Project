# ================================================================
# Urban Heat Island (UHI) Analysis – robust version (matched, aligned, adaptive)
# Inputs: NDVI, NDBI, LST rasters (AOI-clipped) in scene subfolders
# Outputs: SUHI maps (.tif), summary CSV, plots, urban/rural masks
# ================================================================

library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------
# 0) (Optional) Backup old outputs so nothing gets overwritten
# ------------------------
# try(file.rename("E:/Course&Class/SFRS/_analysis", "E:/Course&Class/SFRS/_analysis_old"), silent = TRUE)
# dir.create("E:/Course&Class/SFRS/_analysis", showWarnings = FALSE, recursive = TRUE)
# try(file.rename("E:/Course&Class/SFRS/_maps_hotspots/masks", "E:/Course&Class/SFRS/_maps_hotspots/masks_old"), silent = TRUE)

# ------------------------
# 1) Paths
# ------------------------
root     <- "E:/Course&Class/SFRS"
ndvi_dir <- file.path(root, "_ndvi_clipped")
ndbi_dir <- file.path(root, "_indices_clipped")
lst_dir  <- file.path(root, "_lst_clipped")
out_dir  <- file.path(root, "_analysis")
mask_dir <- file.path(root, "_maps_hotspots", "masks")

aoi <- vect(file.path(root, "n_shapefile", "AOI.shp"))

dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(mask_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------
# 2) Gather files and match by scene ID (not by list order)
# ------------------------
ndvi_files <- list.files(ndvi_dir, pattern = "_NDVI_clipped\\.tif$", full.names = TRUE, recursive = TRUE)
ndbi_files <- list.files(ndbi_dir, pattern = "_NDBI_clipped\\.tif$", full.names = TRUE, recursive = TRUE)
lst_files  <- list.files(lst_dir,  pattern = "_LST_C_clipped\\.tif$",  full.names = TRUE, recursive = TRUE)

scene_id <- function(p){
  b <- basename(p)
  m <- regexpr("\\d{4}-\\d{2}-\\d{2}_LC0[89]_\\d{3}_\\d{3}", b)
  if (m[1] > 0) substr(b, m[1], m[1] + attr(m, "match.length") - 1) else gsub("(_NDVI|_NDBI|_LST).*", "", b)
}
idx_by_scene <- function(v) tapply(v, sapply(v, scene_id), function(x) x[1], simplify = TRUE)

NDVI <- idx_by_scene(ndvi_files)
NDBI <- idx_by_scene(ndbi_files)
LST  <- idx_by_scene(lst_files)

scenes <- sort(Reduce(intersect, list(names(LST), names(NDVI), names(NDBI))))
if (length(scenes) == 0) stop("No matching scenes across NDVI/NDBI/LST.")

cat("Scenes detected (matched across folders):\n"); print(scenes)

# ------------------------
# 3) Helpers
# ------------------------
safe_mean <- function(r){
  v <- try(terra::global(r, "mean", na.rm = TRUE)[1,1], silent = TRUE)
  if (inherits(v, "try-error")) NA_real_ else v
}

# ------------------------
# 4) Loop through scenes
# ------------------------
summary_list <- list()

for (scene in scenes) {
  message("Processing: ", scene)
  
  ndvi <- rast(NDVI[[scene]])
  ndbi <- rast(NDBI[[scene]])
  lst  <- rast(LST[[scene]])
  
  # Align NDVI/NDBI to LST grid (prevents pixel shifts)
  ndvi <- terra::project(ndvi, lst, method = "bilinear")
  ndbi <- terra::project(ndbi, lst, method = "bilinear")
  
  # Crop/mask to AOI
  ndvi <- mask(crop(ndvi, aoi), aoi)
  ndbi <- mask(crop(ndbi, aoi), aoi)
  lst  <- mask(crop(lst,  aoi), aoi)
  
  # --- Adaptive thresholds (per scene quantiles) ---
  ndvi_vals <- values(ndvi, na.rm = TRUE)
  ndbi_vals <- values(ndbi, na.rm = TRUE)
  if (length(ndvi_vals) < 100 || length(ndbi_vals) < 100) {
    warning("Too few pixels after AOI mask: ", scene); next
  }
  
  q_low  <- 0.40; q_high <- 0.60
  ndvi_low  <- as.numeric(quantile(ndvi_vals, probs = q_low,  na.rm = TRUE))
  ndvi_high <- as.numeric(quantile(ndvi_vals, probs = q_high, na.rm = TRUE))
  ndbi_low  <- as.numeric(quantile(ndbi_vals, probs = q_low,  na.rm = TRUE))
  ndbi_high <- as.numeric(quantile(ndbi_vals, probs = q_high, na.rm = TRUE))
  
  # --- Urban/Rural masks (logical) ---
  urban_mask <- (ndvi <= ndvi_low)  & (ndbi >= ndbi_high)  # low NDVI & high NDBI
  rural_mask <- (ndvi >= ndvi_high) & (ndbi <= ndbi_low)   # high NDVI & low NDBI
  
  # Overlap guard (rare with 40/60, but safe): urban wins ties
  ov <- terra::global(urban_mask & rural_mask, "sum", na.rm = TRUE)[1,1]
  if (!is.na(ov) && ov > 0) rural_mask[urban_mask] <- FALSE
  
  # Handle empty masks by slight relaxation
  if (terra::global(urban_mask, "sum", na.rm = TRUE)[1,1] == 0) {
    urban_mask <- (ndvi <= quantile(ndvi_vals, 0.45, na.rm = TRUE)) &
      (ndbi >= quantile(ndbi_vals, 0.55, na.rm = TRUE))
  }
  if (terra::global(rural_mask, "sum", na.rm = TRUE)[1,1] == 0) {
    rural_mask <- (ndvi >= quantile(ndvi_vals, 0.55, na.rm = TRUE)) &
      (ndbi <= quantile(ndbi_vals, 0.45, na.rm = TRUE))
  }
  
  # Save masks (organized)
  writeRaster(urban_mask, file.path(mask_dir, paste0(scene, "_urban_mask.tif")), overwrite = TRUE)
  writeRaster(rural_mask, file.path(mask_dir, paste0(scene, "_rural_mask.tif")), overwrite = TRUE)
  
  # Apply masks to LST (keep TRUE; drop FALSE=0)
  urban_lst <- mask(lst, urban_mask, maskvalues = 0, updatevalue = NA)
  rural_lst <- mask(lst, rural_mask, maskvalues = 0, updatevalue = NA)
  
  # Means (robust)
  urban_mean <- safe_mean(urban_lst)
  rural_mean <- safe_mean(rural_lst)
  if (is.na(urban_mean) || is.na(rural_mean)) {
    warning("Urban/Rural mean NA for scene: ", scene); next
  }
  
  suhi_val <- urban_mean - rural_mean
  
  # SUHI map as anomaly vs rural mean (over whole AOI)
  suhi_map <- lst - rural_mean
  writeRaster(suhi_map, file.path(out_dir, paste0(scene, "_SUHI_map.tif")), overwrite = TRUE)
  
  # Histogram (sample to avoid huge memory)
  set.seed(1)
  urban_vals <- values(urban_lst, na.rm = TRUE)
  rural_vals <- values(rural_lst, na.rm = TRUE)
  if (length(urban_vals) > 200000) urban_vals <- sample(urban_vals, 200000)
  if (length(rural_vals) > 200000) rural_vals <- sample(rural_vals, 200000)
  
  df_hist <- data.frame(
    LST = c(urban_vals, rural_vals),
    Class = c(rep("Urban", length(urban_vals)), rep("Rural", length(rural_vals)))
  )
  
  p_hist <- ggplot(df_hist, aes(x = LST, fill = Class)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    labs(title = paste("Urban vs Rural LST -", scene), x = "LST (°C)", y = "Pixel count") +
    theme_minimal()
  
  ggsave(filename = file.path(out_dir, paste0(scene, "_LST_histogram.png")),
         plot = p_hist, width = 7, height = 5)
  
  # Save summary row
  summary_list[[length(summary_list) + 1]] <- data.frame(
    Scene = scene,
    Urban_LST = urban_mean,
    Rural_LST = rural_mean,
    SUHI = suhi_val,
    stringsAsFactors = FALSE
  )
}

# ------------------------
# 5) Combine summary and plots
# ------------------------
summary_df <- do.call(rbind, summary_list)
summary_df$Date <- as.Date(substr(summary_df$Scene, 1, 10))
write.csv(summary_df, file.path(out_dir, "SUHI_summary.csv"), row.names = FALSE)

# Time-series of SUHI
p_suhi <- ggplot(summary_df, aes(x = Date, y = SUHI)) +
  geom_line(color = "red", size = 1) +
  geom_point(size = 2, color = "darkred") +
  labs(title = "Time Series of SUHI Intensity", x = "Date", y = "SUHI (°C)") +
  theme_minimal()

ggsave(filename = file.path(out_dir, "SUHI_time_series.png"),
       plot = p_suhi, width = 7, height = 5)

# Urban vs Rural mean LST
df_long <- summary_df %>%
  pivot_longer(cols = c("Urban_LST", "Rural_LST"),
               names_to = "Class", values_to = "LST")

p_lsts <- ggplot(df_long, aes(x = Date, y = LST, color = Class, group = Class)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Urban vs Rural Mean LST", x = "Date", y = "Mean LST (°C)") +
  theme_minimal()

ggsave(filename = file.path(out_dir, "Urban_Rural_LST_plot.png"),
       plot = p_lsts, width = 7, height = 5)

message("✅ Full UHI analysis complete! Check outputs in: ", out_dir,
        " and masks in: ", mask_dir)
