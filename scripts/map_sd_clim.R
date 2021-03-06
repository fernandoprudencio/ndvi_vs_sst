#' @title
#' Plot the spatial distribution of GVMI an GVMI anomalies
#'
#' @description
#' this script plots, as monitoring, the spatial distribution of GMVI and GVMI
#'   anomalies for each month of the current year
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "ncdf4", "sf", "lattice", "extrafont",
  "cptcity", "latticeExtra", "rasterVis", "maptools", "grid"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

#' LOAD PACKAGE
library(tidyverse)
library(raster)
library(sf)
library(lattice)
library(extrafont)
library(cptcity)
library(latticeExtra)
library(rasterVis)
library(stringr)
library(magick)
library(gridExtra)
library(grid)
# library(maptools)

#' LOAD LIST OF RASTER
lst <- list.files(
  "data/raster/ndvi/climatology/sd",
  pattern = ".tif",
  full.names = T
)

#' LOAD VECTOR DATA TO PLOT WHIT RASTER OF TEMPERATURE
#'   load world countries limits
#'     load sf data
sf.world <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "world_countries", quiet = T, as_tibble = T
)
#'     load sp data
sp.world <- as(st_geometry(sf.world), Class = "Spatial")

#'   load climat regions
#'     load sf data
sf.region <- st_read(
  dsn = "data/vector/Andes_region.gpkg",
  layer = "Andes_region_clipped", quiet = T, as_tibble = T
)
#'     load sp data
sp.region <- as(st_geometry(sf.region), Class = "Spatial")

#' IF YOU WANT TO APPLAY MEAN FILTER
#'   run it
# index <- raster::focal(
#   index,
#   w = matrix(1, 3, 3),
#   fun = function(x) {
#     mean(x, na.rm = T)
#   },
#   pad = TRUE, na.rm = FALSE
# )

#' LOAD RASTER DATA
index <-
  aggregate(
    raster::stack(lst) / 10000,
    fact = 10, fun = mean
  ) %>%
  raster::crop(sf.world) %>%
  raster::mask(sf.world)

#' DEFINE INTERVAL OF VALUES
intrv <- c(seq(0, .02, .005), seq(.03, .1, .01), seq(.12, .24, .02))
intrv.lbl <- c(0, .02, .06, .1, .16, .24)

#' BUILD PLOT
#'   Define color palette
cb.palette <-
  c(
    "#3f0a13", "#770f29", "#a61b28", "#c75135", "#d68062",
    "#e6b3a1", "#f5e4df",
    "#122514", "#1b452d", "#176441", "#07874f", "#3aa955",
    "#7dc375", "#aedfa1", "#dcffd5",
    "#191e47", "#1e55c5", "#539ec5", "#c7d8de"
  )

#'   Define plot name
name <- "export/ndvi_sd_clim_v2.png"
#'   Save plot
png(name, width = 20, height = 10, units = "cm", res = 500)

levelplot(index,
  names.attr =
    c(
      "e) DJF Standard Deviation", "f) MAM Standard Deviation",
      "g) JJA Standard Deviation", "h) SON Standard Deviation"
    ),
  par.strip.text = list(cex = .8, lines = 1.5), # header size for each map
  # layout = c(4, 1), # define number of row and colums
  scales = list(
    x = list(
      limits = c(-81.8, -68.2), tick.number = 4 # , tck = .2
    ),
    y = list(
      at = c(-15, -10, -5, 0), limits = c(-18.5, .1), rot = 90,
      tick.number = 4
    ),
    draw = T
  ),
  col.regions = rev(cb.palette),
  margin = F,
  pretty = T,
  maxpixels = 15e6,
  at = intrv,
  colorkey = list(
    at = intrv, height = .8, width = 1.1,
    space = "top", tck = .3, # location of legend
    labels = list(at = intrv.lbl, cex = .7),
    font = list(family = "Source Sans Pro"),
    axis.line = list(lwd = 1, col = "black")
  ),
  xlab = NULL,
  ylab = NULL,
  par.settings = list(
    axis.text = list(fontfamily = "Source Sans Pro", cex = .6),
    axis.components = list(
      bottom = list(tck = .4, pad1 = .3),
      left = list(tck = .4, pad1 = .3)
    ),
    strip.background = list(col = "transparent"), # header fill for each map
    strip.border = list(col = "transparent", lwd = 1), # header line for each map
    panel.background = list(col = "gray"),
    axis.line = list(lwd = 1, col = "black")
  ),
  par.xlab.text = list(fontfamily = "Source Sans Pro"),
  par.ylab.text = list(fontfamily = "Source Sans Pro"),
  par.main.text = list(fontfamily = "Source Sans Pro"),
  par.sub.text = list(fontfamily = "Source Sans Pro")
) +
  latticeExtra::layer(
    sp.lines(sp.world, col = "black", lwd = .5),
    sp.lines(sp.region, col = "black", lwd = .7)
  )

#' CLOSE THE SAVED OF PLOT
dev.off()

#' TRIM FIGURE
img <- magick::image_read(name, strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#' SAVE FIGURE
image_write(img, path = name, format = "png", quality = 100)
