rm(list = ls())

library(tidyverse)
library(lattice)
library(cptcity)
library(latticeExtra)
library(grid)
library(RColorBrewer)

load("data/rdata/anomalies_iv-and-sst.RData")

lagged.cor <- function(ndvi, sst, data) {
  for (k in 1:12) {
    anom.sst <-
      dplyr::select(data, date, sst) %>%
      dplyr::filter(date >= "2002-06-01" & date <= "2020-06-01") %>%
      dplyr::filter(str_sub(date, 6, 7) == sprintf("%02d", k))
    names(anom.sst)[2] <- "anom.sst"

    for (i in 0:11) {
      anom.ndvi <-
        dplyr::select(data, date, ndvi) %>%
        dplyr::filter(date >= "2002-06-01" & date <= "2020-06-01") %>%
        dplyr::filter(date %in% (anom.sst$date + months(i)))
      names(anom.ndvi)[2] <- "anom.ndvi"

      x <- anom.sst$anom.sst[1:nrow(anom.ndvi)]
      y <- anom.ndvi$anom.ndvi
      value <- cor(x, y, method = "pearson")

      if (i == 0) r <- value else r <- c(r, value)
    }

    if (k == 1) df.r <- matrix(r, nrow = 1) else df.r <- rbind(df.r, r)
  }
  return(raster(df.r))
  # as.tibble(df.r) %>%
  #   mutate(month = month.abb) %>%
  #   gather(key = "lag", value = "value", -month) %>%
  #   mutate(lag = as.numeric(str_sub(lag, 2, 3)) - 1) %>%
  #   return()
}

andes.region <- c("ts.nw", "ts.ne", "ts.cw", "ts.ce", "ts.sw", "ts.se")

list.cor <-
  lapply(andes.region, FUN = lagged.cor, sst = "anom.oni", data = df.anom)

# df <- lagged.cor(sst = "anom.icen", ndvi = "ts.nw", data = df.anom)
raster::stack(list.cor)
levelplot(list.cor[[1]])





df.plt <- mutate(df, month = rep(12:1, 12))

cb.palette <-
  c(
    "#00315f", "#1367a9", "#3594c0", "#cfe5ef",
    "#ffffff", "#ffffff",
    "#ffdbc9", "#de5f51", "#ba142e", "#6c001f"
  )

#' SAVE PLOT
png(
  "export/lag_cor.png",
  width = 9, height = 10, units = "cm", res = 1000
)

#' PLOT HEATMAP
levelplot(value ~ lag * month,
  data = df.plt,
  main =
    list(
      "NORTH-WESTERN ANDES",
      cex = .8, hjust = .4, vjust = 2.1,
      fontface = "plain", line = 0.5, side = "right"
    ),
  sub =
    list(
      "Correlation Coefficient",
      cex = .6, hjust = .35, vjust = .3,
      fontface = "plain", line = 0.5, side = "right"
    ),
  at = seq(-1, 1, .2),
  margin = F,
  pretty = T,
  col.regions = rev(cb.palette),
  colorkey = list(
    title = expression(m^3/m^3),
    at = seq(-1, 1, .2),
    space = "bottom", # location of legend
    labels = list(
      at = c(-1, -.6, -.2, 0, .2, .6, 1), cex = .8, col = "black",
      family = "Source Sans Pro"
    ),
    font = list(family = "Source Sans Pro"),
    axis.line = list(lwd = 1, col = "black"),
    height = .95, width = 1.2, tck = .5
  ),
  xlab = list("Monthy Lag", cex = .8, fontfamily = "Source Sans Pro", vjust = -.8),
  ylab = list("ONI", cex = .8, fontfamily = "Source Sans Pro", vjust = 2),
  scales = list(
    x = list(
      cex = .8, label = -1:11, tick.number = 12,
      fontfamily = "Source Sans Pro"
    ),
    y = list(
      cex = .8, label = c("x", rev(month.abb)), tick.number = 12,
      fontfamily = "Source Sans Pro"
    )
  ),
  label.style = "align",
  lwd = 1,
  region = T, # show value by pixel
  border = "black", # line color of gride
  border.lty = 1, # line type of gride
  border.lwd = 1.2, # line width of gride
  par.settings = list(
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1),
    axis.components = list(
      bottom = list(tck = 0, pad1 = .5),
      top = list(tck = 0, pad1 = 0),
      left = list(tck = 0, pad1 = .5),
      right = list(tck = 0, pad1 = 0)
    ),
    strip.background = list(col = "transparent"), # header fill for each map
    strip.border = list(col = "transparent", lwd = 1), # header line for each map
    panel.background = list(col = "gray"),
    axis.line = list(lwd = 1, col = "black")
  ),
  par.xlab.text = list(fontfamily = "Source Sans Pro"),
  par.ylab.text = list(fontfamily = "Source Sans Pro"),
  par.sub.text = list(fontfamily = "Source Sans Pro"),
  par.main.text =list(fontfamily = "Source Sans Pro")
)

#' CLOSE THE SAVED OF PLOT
dev.off()
