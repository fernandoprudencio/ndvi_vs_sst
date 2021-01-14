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
# library(maptools)
# library(grid)

#' LOAD LIST OF RASTER
lst <- list.files(
  "data/raster/ndvi/climatology/sd", pattern = ".tif",
  full.names = T
)

#' LOAD RASTER DATA
index <- raster(lst[1])/10000
# index[index > .3] <- NA
# raster::hist(index)

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
  dsn = "data/vector/climatic_regions.gpkg",
  layer = "bastian_regions", quiet = T, as_tibble = T
) %>%
  mutate(id = c(1, 4, 2, 3, 4)) %>%
  group_by(id) %>%
  summarise()
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

#' RESAMPLE RASTER
index4 <- aggregate(index, fact = 10, fun = mean) %>%
  raster::crop(sf.world) %>%
  raster::mask(sf.world)

#' DEFINE INTERVAL OF VALUES
# intrv <- c(seq(0, .02, .005), seq(.03, .06, .01))
# intrv.lbl <- c(0, .01, seq(.02, .06, .02))

# intrv <- c(seq(0, .04, .01), seq(.06, .24, .02)) * .5
# intrv.lbl <- c(0, .02, seq(.04, .24, .04)) * .5

intrv <- c(seq(0, .02, .005), seq(.03, .1, .01), seq(.12, .24, .02))
intrv.lbl <- c(0, .02, .06, .1, .16, .24)

#' BUILD PLOT
#' Define color palette
# cb.palette <-
#   c(
#     "#3f0a13", "#5b0f20", "#7f0f2a", "#9a132a", "#b22726",
#     "#c44a30", "#ce6747", "#d68062", "#dd9881", "#e7b7a7",
#     "#eed2c8", "#f9ebe8", "#4495c5", "#244ebd", "#293784",
#     "#191e47"
#   )

# cb.palette <-
#   c(
#     "#3f0a13", "#5b0f20", "#7f0f2a", "#9a132a", "#b22726",
#     "#c44a30", "#151d44", "#1c4058", "#1a5b69", "#117d79",
#     "#389a82", "#7eb491", "#4495c5", "#244ebd", "#293784",
#     "#191e47"
#   )

cb.palette <-
  c(
    "#3f0a13", "#770f29", "#a61b28", "#c75135", "#d68062",
    "#e6b3a1", "#f5e4df",
    "#122514", "#1b452d", "#176441", "#07874f", "#3aa955",
    "#7dc375", "#aedfa1", "#dcffd5",
    "#191e47", "#1e55c5", "#539ec5", "#c7d8de"
  )

# cb.palette <-
#   c(
#     "#3f0a13", "#620f22", "#860f2b", "#ac2127", "#c2442b",
#     "#ce6747", "#d9866b", "#e1a591", "#edcbc0", "#f9ebe8",
#     "#4495c5", "#244ebd", "#293784", "#191e47"
#   )

# cb.palette <-
#   c(
#     "#3f0a13", "#711027", "#9a132a", "#bd3b28", "#ce6747", "#dc937a", "#e9beb1",
#     "#e6e9eb", "#4495c5", "#244ebd", "#293784", "#191e47"
#   )

name <- "export/ndvi_dic-feb4.tif"
png(name, width = 20, height = 28, units = "cm", res = 500)

levelplot(index4,
  # main = list(
  #   title,
  #   cex = 2, side = 1, line = .5, fontfamily = "Source Sans Pro"
  # ),
  scales = list(
    x = list(limits = c(-81.8, -68.2)),
    y = list(limits = c(-18.4, .1))
  ),
  col.regions = rev(cb.palette), #cpt("cmocean_balance"),#rev(cb.palette),
  margin = F,
  pretty = T,
  maxpixels = 15e6,#15e6,
  at = intrv,
  colorkey = list(
    at = intrv,
    space = "right", # location of legend
    labels = list(at = intrv.lbl, cex = 1.1),
    font = list(family = "Source Sans Pro")
  ),
  xlab = NULL,
  ylab = NULL,
  aspect.fill = T,
  par.settings = list(
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    par.xlab.text = list(fontfamily = "Source Sans Pro"),
    par.ylab.text = list(fontfamily = "Source Sans Pro"),
    par.main.text = list(fontfamily = "Source Sans Pro"),
    par.sub.text = list(fontfamily = "Source Sans Pro")
  )
) +
  latticeExtra::layer(
    sp.lines(sp.world, col = "black", lwd = 1.5),
    sp.lines(sp.region, col = "black", lwd = 1.8)
  )

#' CLOSE THE SAVED OF PLOT
dev.off()

#' TRIM FIGURE
img <- magick::image_read(name, strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#' SAVE FIGURE
image_write(img, path = name, format = "png")
