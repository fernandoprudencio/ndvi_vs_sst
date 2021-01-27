#' @title
#' extract time series (median value) by Andes region NDVI
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster", "sf")

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
library(sf)
library(tidyverse)
library(raster)

#' LOAD VECTOR DATA
#'   Andes region
sf.region <-
  st_read(
    dsn = "data/vector/climatic_regions.gpkg", layer = "bastian_regions"
  ) %>%
  dplyr::filter(Region %in% c("Andes west slope", "Andes east slope"))
#'   tile vector
sf.tiles <-
  st_read(dsn = "data/vector/Andes_region.gpkg", layer = "Andes_region")

st_crs(sf.tiles) <- st_crs(sf.region)
#'   intersect vector within Andes region
sf.andes <-
  st_intersection(sf.region, sf.tiles) %>%
  dplyr::mutate(
    Id = c(5, 6, 3, 4, 1, 2),
    Region =
      c(
        "Andes south-west", "Andes south-east",
        "Andes central-west", "Andes central-east",
        "Andes north-west", "Andes north-east"
      )
  ) %>%
  dplyr::rename("id" = "Id") %>%
  arrange(id)

#' LIST MONTHLY NDVI FILES
ls.ndvi <- list.files("data/raster/ndvi/monthly", ".tif", full.names = T)

#' BUILD FUNCTION TO EXTRACT NDVI VALUES (MEDIAN)
extract.ts <- function(data, vector) {
  img <- raster::crop(raster(data), vector) %>% raster::mask(vector)
  vls <- getValues(img)
  vls[vls < 2000] <- NA
  median(vls, na.rm = T) %>% return()
}

#'  APPLY FUNCTION TO EXTRACT NDVI VALUES (MEDIAN) BY ANDES REGION
ts.nw <- sapply(ls.ndvi, FUN = extract.ts, dplyr::filter(sf.andes, id == 1))
ts.ne <- sapply(ls.ndvi, FUN = extract.ts, dplyr::filter(sf.andes, id == 2))
ts.cw <- sapply(ls.ndvi, FUN = extract.ts, dplyr::filter(sf.andes, id == 3))
ts.ce <- sapply(ls.ndvi, FUN = extract.ts, dplyr::filter(sf.andes, id == 4))
ts.sw <- sapply(ls.ndvi, FUN = extract.ts, dplyr::filter(sf.andes, id == 5))
ts.se <- sapply(ls.ndvi, FUN = extract.ts, dplyr::filter(sf.andes, id == 6))

#' BUILD DATAFRAME OF NDVI VALUES
df.ts <-
  tibble(ts.nw, ts.ne, ts.cw, ts.ce, ts.sw, ts.se) %>%
  mutate(
    date = seq(as.Date("2001-12-01"), as.Date("2020-12-01"), by = "1 month")
  )

#' EXAMPLE TO PLOT TIME SERIES BY ANDES REGION
ggplot(df.ts) +
  geom_line(aes(date, ts.nw)) +
  geom_line(aes(date, ts.ne), col = "red") +
  geom_line(aes(date, ts.cw), col = "blue") +
  geom_line(aes(date, ts.ce), col = "green") +
  geom_line(aes(date, ts.sw), col = "yellow") +
  geom_line(aes(date, ts.se), col = "orange")
