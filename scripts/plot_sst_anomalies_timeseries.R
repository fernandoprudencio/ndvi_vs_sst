#' @title
#' Plot SST anomaly time series by ocean
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "ggpubr")

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
library(ggpubr)

#' LOAD ANOMALIES TIME SERIES
load("data/rdata/anomalies_iv-and-sst.RData")

#' BUILD DATAFRAME OF SST ANOMALIES BY OCEAN
#'   sst anomalies time series of Pacific ocean
df.sst.pac <-
  dplyr::select(
    df.anom, c("date", "anom.oni", "anom.icen")
  ) %>%
  rename("01-anom.oni" = "anom.oni", "02-anom.icen" = "anom.icen") %>%
  gather(key = "region", value = "anom", -date)
#'   sst anomalies time series of Atlantic ocean
df.sst.atl <-
  dplyr::select(
    df.anom, c("date", "anom.natl", "anom.satl")
  ) %>%
  rename("01-anom.natl" = "anom.natl", "02-anom.satl" = "anom.satl") %>%
  gather(key = "region", value = "anom", -date)

#' PLOT TIME SERIES
#'   labels
lbls <- c("ONI", "ICEN", "NATL", "SATL")
#'   plot object
#'     Pacific ocean
plt.sst.pac <-
  ggplot(df.sst.pac, aes(x = date, y = anom, group = region)) +
  annotate(
    geom = "text", x = as.Date("2002-04-01"), y = 2.3,
    label = "a)", color ="black", family = "Source Sans Pro"
  ) +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "black", size = .3
  ) +
  geom_hline(
    yintercept = seq(-3, 3, 1),
    linetype = "dashed", color = "black", size = .01
  ) +
  geom_line(aes(linetype = region, color = region, size = region)) +
  # geom_point(aes(color = region), size = .01) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = lbls[1:2]
  ) +
  scale_color_manual(
    values = c("black", "gray"),
    labels = lbls[1:2]
  ) +
  scale_size_manual(values = rep(.4, 2), labels = lbls[1:2]) +
  scale_x_date(
    limits = c(as.Date("2002-01-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2002-01-01"), as.Date("2020-12-31"), by = "1 year"),
    date_labels = "%Y", expand = expansion(mult = c(.005, 0))
  ) +
  scale_y_continuous(
    breaks = seq(-3, 3, 1),
    labels = c("-3", "", "", "0", "", "", "3"),
    limits = c(-3, 3)
  ) +
  theme_bw() +
  theme(
    legend.background =
      element_rect(fill = "transparent", color = "transparent", size = .2),
    legend.key.size = unit(5, "cm"),
    legend.direction = "horizontal",
    legend.box.margin = margin(0, 0, 0, 0),
    legend.key.width = unit(.4, "cm"),
    legend.key.height = unit(.05, "cm"),
    legend.position = c(0.5, 0.88),
    legend.title = element_blank(),
    legend.text = element_text(size = 6, family = "Source Sans Pro"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      size = 7, family = "Source Sans Pro", color = "black",
      face = c("plain", "plain", "plain", "bold", "plain", "plain", "plain")
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "black", size = .3),
    axis.ticks.y = element_line(color = "black", size = .3),
    axis.ticks.length.x = unit(1.5, "pt"),
    axis.ticks.length.y = unit(1.5, "pt"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(.1, .1, .1, .1, "cm")
  )

# ggsave(
#   plot = plt.sst.pac, "export/anom_sst_pac.png",
#   width = 15, height = 2, units = "cm", dpi = 500
# )

#'     Atlantic ocean
plt.sst.atl <-
  ggplot(df.sst.atl, aes(x = date, y = anom, group = region)) +
  annotate(
    geom = "text", x = as.Date("2002-04-01"), y = 1.533333,
    label = "b)", color ="black", family = "Source Sans Pro"
  ) +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "black", size = .3
  ) +
  geom_hline(
    yintercept = seq(-2, 2, 1),
    linetype = "dashed", color = "black", size = .01
  ) +
  geom_line(aes(linetype = region, color = region, size = region)) +
  # geom_point(aes(color = region), size = .01) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = lbls[3:4]
  ) +
  scale_color_manual(
    values = c("black", "gray"),
    labels = lbls[3:4]
  ) +
  scale_size_manual(values = rep(.4, 2), labels = lbls[3:4]) +
  scale_x_date(
    limits = c(as.Date("2002-01-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2002-01-01"), as.Date("2020-12-31"), by = "1 year"),
    labels =
      c(
        "", "2003", "", "2005", "", "2007", "", "2009", "", "2011", "",
        "2013", "", "2015", "", "2017", "", "2019", ""
      ),
    expand = expansion(mult = c(.005, 0))
  ) +
  scale_y_continuous(
    breaks = seq(-2, 2, 1),
    labels = c("-2", "", "0", "", "2"),
    limits = c(-2, 2)
  ) +
  theme_bw() +
  theme(
    legend.background =
      element_rect(fill = "transparent", color = "transparent", size = .2),
    legend.key.size = unit(5, "cm"),
    legend.direction = "horizontal",
    legend.box.margin = margin(0, 0, 0, 0),
    legend.key.width = unit(.4, "cm"),
    legend.key.height = unit(.05, "cm"),
    legend.position = c(0.5, 0.88),
    legend.title = element_blank(),
    legend.text = element_text(size = 6, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 7, colour = "black", family = "Source Sans Pro",
      angle = 0, vjust = .6
    ),
    axis.text.y = element_text(
      size = 7, family = "Source Sans Pro", color = "black",
      face = c("plain", "plain", "bold", "plain", "plain")
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "black", size = .3),
    axis.ticks.y = element_line(color = "black", size = .3),
    axis.ticks.length.x = unit(1.5, "pt"),
    axis.ticks.length.y = unit(1.5, "pt"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(0, .1, .1, .1, "cm"),
  )

# ggsave(
#   plot = plt.sst.atl, "export/anom_sst_atl.png",
#   width = 15, height = 2, units = "cm", dpi = 500
# )

#'     arrange plots
plt <-
  ggarrange(
    plt.sst.pac, plt.sst.atl,# font.label = list(size = 10),
    #labels = c("a)", "b)"), hjust = -1.7, vjust = 1.8,
    ncol = 1, align = "v", widths = rep(15, 2), heights = c(2, 2.13)
  ) %>%
  annotate_figure(
    left = text_grob(
      "SST anomaly", size = 8, col = "black", rot = 90,
      family = "Source Sans Pro"
    )
  )

#'     save plot
ggsave(
  plot = plt, "export/anom_sst.png",
  units = "cm", dpi = 500, height = 4, width = 15
)
