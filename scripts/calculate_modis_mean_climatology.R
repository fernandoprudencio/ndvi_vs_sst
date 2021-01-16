#' @title
#' calculate NDVI climatology (average)
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

#' LOAD LIST OF RASTER
lst <- list.files("data/raster/ndvi/three_month", ".tif", full.names = T)

#' LOAD MODIS DATE
df <- tibble(date = str_sub(lst, -11, -5)) %>%
  mutate(id = 1:n())

#' BUILD A FUNCTION TO CALCULATE CLIMATOLOGY
fun.clim <- function(month, data) {
  grd.mt <- dplyr::filter(df, str_sub(date, 6, 7) == month)

  ndvi <- data[grd.mt$id] %>%
    raster::stack() %>%
    raster::calc(fun = function(x) mean(x, na.rm = T))

  writeRaster(
    ndvi,
    overwrite = T, datatype = "INT2S",
    filename =
      sprintf(
        "data/raster/ndvi/climatology/mean/MOD09A1.006_NDVI_mean.doy%1$s.tif",
        month
      )
  )
}

#' APPLY fun.clim() FUNCTION
grd.clim <-
  sapply(
    sprintf("%02d", 1:4),
    FUN = fun.clim,
    data = lst
  )
