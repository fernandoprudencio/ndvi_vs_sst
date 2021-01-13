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

k.dep <- c(
  "Piura", "Cajamarca", "La Libertad", "Ancash", "Loreto", "Huancavelica",
  "Amazonas", "Madre de Dios", "Cusco", "Apurimac", "Puno", "Huanuco", "Pasco",
  "Junin"
)

cb.palette <-
  c(
    "#0000E1", "#0055EB", "#00AAF5", "#00FFFF", "#1BA704", "#14BD03", "#0ED302",
    "#07E901", "#00FF00", "#40FF00", "#80FF00", "#BFFF00", "#FFFF00", "#F7D400",
    "#EFA900", "#E87F00", "#E05400", "#D82900", "#B92E00", "#993300", "#737373"
  )

cb.palette <-
  c(
    "#0055EB", "#00AAF5", "#00FFFF", "#1BA704", "#14BD03", "#0ED302",
    "#07E901", "#00FF00", "#40FF00", "#80FF00", "#BFFF00", "#FFFF00", "#F7D400",
    "#EFA900", "#E87F00", "#E05400", "#D82900", "#B92E00", "#993300", "#737373"
  )


# index <- raster("data/raster/ndvi/climatology/mean/quarterly/MOD09A1.006_NDVI_qua.mean.Sep-Nov.tif")/10000
index <- raster("data/raster/ndvi/climatology/sd/quarterly/MOD09A1.006_NDVI_qua.sd.Sep-Nov.tif")/10000
raster::hist(index)

sf.andes <- st_read(
  dsn = "data/vector/climatic_regions.gpkg",
  layer = "bastian_regions", quiet = T, as_tibble = T
) %>%
  filter(Region %in% c("Andes west slope", "Andes east slope"))

index <- raster::crop(index, sf.andes) %>% raster::mask(sf.andes)

intrv <- c(-.1, seq(0, 1, .05))
intrv.lbl <- c(-.1, seq(0, 1, .2))

intrv <- seq(0, .1, .01)
intrv.lbl <- seq(0, .1, .02)


name <- "export/ndvi_sep-nov2.tif"
png(name, width = 20, height = 28, units = "cm", res = 500)

levelplot(index,
  # main = list(
  #   title,
  #   cex = 2, side = 1, line = .5, fontfamily = "Source Sans Pro"
  # ),
  scales = list(
    x = list(limits = c(-81.8, -68.2)),
    y = list(limits = c(-18.7, .4))
  ),
  col.regions = rev(cb.palette),
  margin = F,
  pretty = T,
  maxpixels = 15e6,
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
) #+
#   latticeExtra::layer(
#     sp.lines(sp.world, col = "black", lwd = 2),
#     # sp.lines(sp.dep, col = "black", lwd = .8)
#     sp.points(sp.peru, pch = 20, cex = 1, col = "black"),
#     sp.text(
#       coordinates(sp.peru),
#       txt = sf.peru$Departamen, pos = 1, cex = 1.2,
#       fontfamily = "Source Sans Pro"
#     )
#   )

#' CLOSE THE SAVED OF PLOT
dev.off()

#' TRIM FIGURE
img <- magick::image_read(name, strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#' SAVE FIGURE
image_write(img, path = name, format = "png")
