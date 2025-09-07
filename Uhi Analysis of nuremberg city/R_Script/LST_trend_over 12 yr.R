# ---- LST TREND (12 scenes) ----
library(terra)

# 1) Folder with your scene subfolders
dir_lst <- "E:/Course&Class/SFRS/_lst_clipped"

# 2) Find the 12 LST rasters inside subfolders
lst_files <- list.files(
  dir_lst, pattern = "_LST_C_clipped\\.tif$", full.names = TRUE,
  recursive = TRUE, ignore.case = TRUE
)

stopifnot(length(lst_files) > 0)  # fail early if nothing found

# 3) Parse date from the start of each filename: YYYY-MM-DD_...
get_date <- function(x){
  as.Date(sub("_.*$", "", basename(x)))  # take part before first underscore
}
lst_dates <- get_date(lst_files)

# 4) Sort by date
o <- order(lst_dates)
lst_files <- lst_files[o]
lst_dates <- lst_dates[o]

# 5) Read & summarize
lst_stack <- rast(lst_files)
lst_mean  <- global(lst_stack, mean,   na.rm = TRUE)[,1]
lst_median<- global(lst_stack, median, na.rm = TRUE)[,1]

lst_stats <- data.frame(
  File   = basename(lst_files),
  Date   = lst_dates,
  Mean   = lst_mean,
  Median = lst_median
)
print(lst_stats)

# 6) Plot trend (mean + median)
plot(lst_stats$Date, lst_stats$Mean, type="b", pch=19,
     xlab="Year(Summer)", ylab="LST (°C)", main="LST Trend across 12 Years")
lines(lst_stats$Date, lst_stats$Median, type="b", pch=17)
legend("topleft", c("Mean LST","Median LST"), pch=c(19,17), lty=1, bty="n")
