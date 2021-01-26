rm(list = ls())

library(sf)
library(tidyverse)
library(raster)

sf.tiles <- st_read(dsn = "data/vector/Andes_region.gpkg", layer = "Andes_region")
sf.tiles <- st_read(dsn = "data/vector/Andes_region.gpkg", layer = "Andes_region")
