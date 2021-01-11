#' @title
#' calculate NDVI climatology (standard deviation)
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
  date = seq(as.Date("2002-01-01"), as.Date("2018-12-01"), by = "1 month")
) %>%
  mutate(id = 1:n())

#' LOAD LIST OF RASTER
lst <- list.files("data/raster/ndvi/monthly", ".tif", full.names = T)

#' BUILD A FUNCTION TO CALCULATE CLIMATOLOGY
fun.clim <- function(month, data) {
  grd.mt <- filter(df, str_sub(date, 6, 7) == month)

  ndvi <- data[grd.mt$id] %>%
    stack() %>%
    raster::calc(fun = function(x) sd(x, na.rm = T))

  writeRaster(
    ndvi,
    overwrite = T, datatype = "INT2S",
    filename =
      sprintf(
        "data/raster/ndvi/climatology/sd/MOD09A1.006_NDVI_sd.doy%1s.tif",
        month
      )
  )
}

#' APPLY fun.clim() FUNCTION
grd.clim <-
  sapply(
    sprintf("%02d", 1:12),
    FUN = fun.clim,
    data = lst
  )

#' Calculate quarterly average
qua <- list(c(12, 1, 2), c(3:5), c(6:8), c(9:11))
avr.list <-
  list.files(
    "data/raster/ndvi/climatology/mean",
    pattern = ".tif", full.names = T
  )

for (i in 1:4) {
  avr.mean <-
    raster::stack(avr.list[qua[[i]]]) %>%
    raster::calc(fun = function(x) mean(x, na.rm = T))

  mth <- month.abb[qua[[i]]]

  writeRaster(
    avr.mean,
    filename =
      sprintf(
        "%1$s/mean/quarterly/MOD09A1.006_NDVI_qua.mean.%2$s-%3$s.tif",
        "data/raster/ndvi/climatology", mth[1], mth[3]
      )
  )
}

#' Calculate quarterly standard deviation
sd.list <-
  list.files(
    "data/raster/ndvi/climatology/sd",
    pattern = ".tif", full.names = T
  )

for (i in 1:4) {
  sd.mean <-
    raster::stack(sd.list[qua[[i]]]) %>%
    raster::calc(fun = function(x) mean(x, na.rm = T))

  mth <- month.abb[qua[[i]]]

  writeRaster(
    sd.mean,
    filename =
      sprintf(
        "%1$s/sd/quarterly/MOD09A1.006_NDVI_qua.sd.%2$s-%3$s.tif",
        "data/raster/ndvi/climatology", mth[1], mth[3]
      )
  )
}
