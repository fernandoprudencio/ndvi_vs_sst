#' @title
#' calculation of anomaly values from the decomposition of time series
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse")

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

#' LOAD RData FILE
load("data/rdata/Andesregion_timeseries.RData")

#' LOAD SST DATA OF PACIFIC AND ATLANTIC
#'   Oceanic Niño Index
oni <-
  read.table("data/tables/ONI/oni_new.txt", header = T) %>%
  as_tibble() %>%
  dplyr::select(ANOM) %>%
  rename("anom.oni" = "ANOM") %>%
  mutate(
    date = seq(as.Date("1950-01-01"), as.Date("2020-11-01"), by = "1 month")
  ) %>%
  dplyr::filter(date >= "2001-12-01")
#'   Índice Costero El Niño
icen <- read.table("data/tables/ICEN/icen.txt", header = T) %>%
  as_tibble() %>%
  dplyr::select(ICEN) %>%
  rename("anom.icen" = "ICEN") %>%
  mutate(
    date = seq(as.Date("1950-02-01"), as.Date("2020-11-01"), by = "1 month")
  ) %>%
  dplyr::filter(date >= "2001-12-01")
#'   Índice Costero El Niño
atlan <-
  read.table("data/tables/SST_ATL_anom/ATLA_ANOMALIES_mew.txt", header = T) %>%
  as_tibble() %>%
  dplyr::select(ANOM, ANOM.1) %>%
  rename("anom.natl" = "ANOM", "anom.satl" = "ANOM.1") %>%
  mutate(
    date = seq(as.Date("1982-01-01"), as.Date("2020-12-01"), by = "1 month")
  ) %>%
  dplyr::filter(date >= "2001-12-01")

#' BUILD FUNCTION TO CALCULATE ANOMALIES OF NDVI
calc.anom <- function(variable) {
  ts <-
    ts(
      dplyr::select(df.ts, variable)/10000,
       start = c(2001, 12), frequency = 12
    )

  descom <- decompose(ts, "additive")
  as.vector(descom$random) %>% return()
}

#' ANDES REGION
region <-
  c("ts.nw", "ts.ne", "ts.cw", "ts.ce", "ts.sw", "ts.se")
# sprintf("anom.ndvi.%1s", c("nw", "ne", "cw", "ce", "sw", "se"))

#' APPLY FUNCTION TO CALCULATE ANOMALIES
df.anom <-
  sapply(region, FUN = calc.anom) %>%
  as_tibble() %>%
  mutate(
    date = seq(as.Date("2001-12-01"), as.Date("2020-12-01"), by = "1 month"),
    anom.oni = c(oni$anom.oni, NA),
    anom.icen = c(icen$anom.icen, NA),
    anom.natl = atlan$anom.natl,
    anom.satl = atlan$anom.satl
  )

#' SAVE DATAFRAME AS .Rdata FILE
save(df.anom, file = "data/rdata/anomalies_iv-and-sst.RData")

#' EXAMPLE TO PLOT TIME SERIES BY ANDES REGION
ggplot(df.anom) +
  geom_line(aes(date, ts.nw)) +
  geom_line(aes(date, ts.ne), col = "red") +
  geom_line(aes(date, ts.cw), col = "blue") +
  geom_line(aes(date, ts.ce), col = "green") +
  geom_line(aes(date, ts.sw), col = "yellow") +
  geom_line(aes(date, ts.se), col = "orange")
