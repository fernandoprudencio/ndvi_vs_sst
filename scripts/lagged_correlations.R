rm(list = ls())

library(tidyverse)
library(lattice)
library(cptcity)
library(latticeExtra)
library(grid)
library(RColorBrewer)
library(rasterVis)
library(gridExtra)

load("data/rdata/anomalies_iv-and-sst.RData")

#' 1.1| BUILD FUNCTION TO CALCULATE LAGGED CORRELATION
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
  # raster(df.r,
  #   xmn = 0, xmx = 12,
  #   ymn = 1, ymx = 13
  # ) %>% return()
  as.tibble(df.r) %>%
    mutate(month = 12:1) %>%
    gather(key = "lag", value = "value", -month) %>%
    mutate(lag = as.numeric(str_sub(lag, 2, 3)) - 1) %>%
    return()
}

#' 1.2| BUILD FUNCTION TO CALCULATE LAGGED CORRELATION
andes.region <- c("ts.nw", "ts.cw", "ts.sw", "ts.ne", "ts.ce", "ts.se")
list.cor <-
  lapply(andes.region, FUN = lagged.cor, sst = "anom.oni", data = df.anom)

#' 2.1| BUILD FUNCTION TO PLOT LAGGED CORRELATION
plot.lc <- function(andes, ocean, pallete, data) {
  df <- lagged.cor(sst = ocean, ndvi = andes, data = df.anom)
  title <- c(
    ts.nw = "NORTH-WESTERN ANDES", ts.cw = "CENTRAL-WESTERN ANDES",
    ts.sw = "SOUTH-WESTERN ANDES", ts.ne = "NORTH-EASTERN ANDES",
    ts.ce = "CENTRAL-EASTERN ANDES", ts.se = "SOUTH-EASTERN ANDES"
  )

  if (andes == "ts.ce") {
    colkey <- list(
      at = seq(-1, 1, .2),
      space = "bottom", # location of legend
      labels = list(
        at = c(-1, -.6, -.2, 0, .2, .6, 1), cex = .8, col = "black",
        family = "Source Sans Pro"
      ),
      font = list(family = "Source Sans Pro"),
      axis.line = list(lwd = 1, col = "black"),
      height = .95, width = 1.2, tck = .5
    )
    subtitle <- list(
      "Correlation Coefficient",
      cex = .6, hjust = .35, vjust = .3,
      fontface = "plain", line = 0.5, side = "right"
    )
  } else {
    colkey <- NULL
    subtitle <- NULL
  }

  #' PLOT HEATMAP
  levelplot(value ~ lag * month,
    data = df,
    main =
      list(
        title[andes],
        cex = .8, hjust = .4, vjust = 2.1,
        fontface = "plain", line = 0.5, side = "right"
      ),
    sub = subtitle,
    at = seq(-1, 1, .2),
    margin = F,
    pretty = T,
    col.regions = rev(pallete),
    colorkey = colkey,
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
    par.main.text = list(fontfamily = "Source Sans Pro")
  ) %>%
    return()
}

#' 2.2| APPLY FUNCTION TO PLOT LAGGED CORRELATION
andes.region <- c("ts.nw", "ts.cw", "ts.sw", "ts.ne", "ts.ce", "ts.se")
cb.palette <-
  c(
    "#00315f", "#1367a9", "#3594c0", "#cfe5ef",
    "#ffffff", "#ffffff",
    "#ffdbc9", "#de5f51", "#ba142e", "#6c001f"
  )

plt <- lapply(
  andes.region,
  FUN = plot.lc, ocean = "anom.oni",
  pallete = cb.palette, data = df.anom
)

#' SAVE PLOT
png(
  "export/lag_cor.png",
  width = 9, height = 10, units = "cm", res = 1000
)



grid <- mutate(
  list.cor[[4]],
  value2 = list.cor[[5]]$value,
  value3 = list.cor[[6]]$value,
  value4 = list.cor[[1]]$value,
  value5 = list.cor[[2]]$value,
  value6 = list.cor[[3]]$value,
) %>%
  rename("value1" = "value") %>%
  gather(
    key = "region", value = "value", -month, -lag
  )# %>%
  # mutate(
  #   region = replace(region, region == "value1", "NORTH-EASTERN ANDES"),
  #   region = replace(region, region == "value2", "CENTRAL-EASTERN ANDES"),
  #   region = replace(region, region == "value3", "SOUTH-EASTERN ANDES"),
  #   region = replace(region, region == "value4", "NORTH-WESTERN ANDES"),
  #   region = replace(region, region == "value5", "CENTRAL-WESTERN ANDES"),
  #   region = replace(region, region == "value6", "SOUTH-WESTERN ANDES")
  # )

levelplot(value ~ lag * month | region,
  data = grid,
  layout = c(3, 2), # define number of row and colums
  names.attr = list(2001:2006),
  par.strip.text = list(2001:2006, cex = .8, lines = 1.5), # header size for each map
  sub = list(
    "Correlation Coefficient",
    cex = .6, hjust = .35, vjust = .3,
    fontface = "plain", line = 0.5, side = "right"
  ),
  at = seq(-1, 1, .2),
  margin = F,
  pretty = T,
  col.regions = rev(cb.palette),
  colorkey = list(
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
  par.main.text = list(fontfamily = "Source Sans Pro")
)

# names(p)
########################










marrangeGrob(plt)

library(ggpubr)
grid.draw(
  rbind(
    ggplotGrob(plt[[1]]), ggplotGrob(plt[[2]]), ggplotGrob(plt[[3]]),
    ggplotGrob(plt[[4]]), ggplotGrob(plt[[5]]), ggplotGrob(plt[[6]]),
    size = "first"
  )
)

plot_grid(
  plt[[1]], plt[[2]], plt[[3]], plt[[4]], plt[[5]], plt[[6]],
  ncol = 3, align = "hv"
)



ggarrange(
  plt[[1]], plt[[2]], plt[[3]], plt[[4]], plt[[5]], plt[[6]],
  align = "hv", ncol = 3, nrow = 2, common.legend = F
)

#' CLOSE THE SAVED OF PLOT
dev.off()
