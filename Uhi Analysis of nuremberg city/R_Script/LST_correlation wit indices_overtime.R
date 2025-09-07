# read the summary table
res <- read.csv("E:/Course&Class/SFRS/_analysis/corr_summary.csv")

# sort by year just in case
res <- res[order(res$year),]

# plot correlation trends (NDVI + NDBI only)
plot(res$year, res$r_LST_NDVI, type="b", pch=19, col="forestgreen",
     ylim=c(-1,1), xlab="Year", ylab="Correlation (r)",
     main="LST correlation with NDVI & NDBI over time")

lines(res$year, res$r_LST_NDBI, type="b", pch=17, col="brown")

legend("center", legend=c("NDVI","NDBI"),
       col=c("forestgreen","brown"),
       pch=c(19,17), lty=1)

