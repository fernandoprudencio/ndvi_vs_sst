#' @title
#' calculate NDVI each three months
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

#' LOAD PACKAGES
library(tidyverse)
library(raster)

#' LOAD MODIS DATE
df <- tibble(
  date = seq(as.Date("2001-12-01"), as.Date("2020-11-01"), by = "1 month")
) %>%
  mutate(id = 1:n())

season <- rep(1:4, 19)
years <- rep(2002:2020, each = 4)

#' LOAD LIST OF RASTER
lst <- list.files("data/raster/ndvi/monthly", ".tif", full.names = T)

for (i in 1:76) {
  inicio <- seq(3, 228, 3)[i] - 2
  final <- seq(3, 228, 3)[i]
  img  <-
    raster::stack(lst[inicio:final]) %>%
    raster::calc(fun = function(x) mean(x, na.rm = T))

  writeRaster(
    img,
    sprintf(
      "%1$s/MOD09A1.006_NDVI_doy%2$s-0%3$s.tif",
      "data/raster/ndvi/three_month", years[i], season[i]
    )
  )
}
