# read the summary table
res <- read.csv("E:/Course&Class/SFRS/_analysis/corr_summary.csv")

# sort by year just in case
res <- res[order(res$year),]

# plot correlation trends (NDVI + NDBI only)
plot(res$year, res$r_LST_NDVI, type="b", pch=19, col="forestgreen",
     ylim=c(-1,1), xlab="Year", ylab="Correlation (r)",
     main="LST correlation with NDVI & NDBI over time")
lines(res$year, res$r_LST_NDBI, type="b", pch=17, col="brown")

# add linear regression trendlines
abline(lm(r_LST_NDVI ~ year, data=res), col="darkgreen", lwd=2, lty=2)
abline(lm(r_LST_NDBI ~ year, data=res), col="darkred",   lwd=2, lty=2)

legend("center", legend=c("NDVI","NDBI","NDVI trend","NDBI trend"),
       col=c("forestgreen","brown","darkgreen","darkred"),
       pch=c(19,17,NA,NA), lty=c(1,1,2,2), lwd=c(1,1,2,2))

