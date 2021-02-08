#' @title
#' Plot lag correlation between sst and ndvi anomalies
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "lattice", "cptcity", "latticeExtra", "grid",
  "RColorBrewer", "gridExtra"
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
library(lattice)
library(cptcity)
library(latticeExtra)
library(grid)
library(RColorBrewer)
library(gridExtra)

#' LOAD ANOMALY VALUES
load("data/rdata/anomalies_iv-and-sst.RData")
#' LOAD FUNCTIONS
source("scripts/functions.R")

#' DEFINE ANDES REGION
andes.region <- c("ts.nw", "ts.cw", "ts.sw", "ts.ne", "ts.ce", "ts.se")

#' DEFINE COLOR PALETTE
cb.palette <-
  c(
    "#00315f", "#1367a9", "#3594c0", "#cfe5ef",
    "#ffffff", "#ffffff",
    "#ffdbc9", "#de5f51", "#ba142e", "#6c001f"
  )

#' GENERATE HEATMAP
anom <- "icen"
plt <- lapply(
  andes.region,
  FUN = plot.lc, ocean = sprintf("anom.%1$s", anom),
  pallete = cb.palette, data = df.anom
)

#' ARRANGE HEATMAPS
plt.arrange <-
  grid_arrange_shared_legend(
    plt[[1]], plt[[2]], plt[[3]], plt[[4]], plt[[5]], plt[[6]],
    ncol = 3, nrow = 2
  )

#' SAVE PLOT
ggsave(
  plot = plt.arrange, sprintf("export/lag_corr_%1$s.png", anom),
  units = "cm", dpi = 500, height = 11.5, width = 15
)
