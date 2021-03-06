#' @title
#' Plot NDVI anomaly time series by Andes region
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

#' BUILD DATAFRAME OF NDVI ANOMALIES BY ANDES REGION
#'   north Andes
df.iv.north <-
  dplyr::select(
    df.anom, c("date", "ts.nw", "ts.ne")
  ) %>%
  rename("01-ts.nw" = "ts.nw", "02-ts.ne" = "ts.ne") %>%
  gather(key = "region", value = "anom", -date)
#'   central Andes
df.iv.central <-
  dplyr::select(
    df.anom, c("date", "ts.cw", "ts.ce")
  ) %>%
  rename("01-ts.cw" = "ts.cw", "02-ts.ce" = "ts.ce") %>%
  gather(key = "region", value = "anom", -date)
#'   south Andes
df.iv.south <-
  dplyr::select(
    df.anom, c("date", "ts.sw", "ts.se")
  ) %>%
  rename("01-ts.sw" = "ts.sw", "02-ts.se" = "ts.se") %>%
  gather(key = "region", value = "anom", -date)

#' PLOT TIME SERIES
#'   labels
lbls <-
  c(
    "North-western Andes", "North-eastern Andes",
    "Central-western Andes", "Central-eastern Andes",
    "South-western Andes", "South-eastern Andes"
  )
#'   plot object
#'     north Andes
plt.iv.north <-
  ggplot(df.iv.north, aes(x = date, y = anom, group = region)) +
  annotate(
    geom = "text", x = as.Date("2002-04-01"), y = .1533333,
    label = "a)", color ="black", family = "Source Sans Pro"
  ) +
  labs(y = "NDVI anomaly") +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "black", size = .3
  ) +
  geom_hline(
    yintercept = c(-.2, -.1, .1, .2),
    linetype = "dashed", color = "black", size = .01
  ) +
  geom_line(aes(linetype = region, color = region, size = region)) +
  # geom_point(aes(color = region), size = .01) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = lbls[1:2]
  ) +
  scale_color_manual(
    values = c(
      "black", "gray"
      # rgb(105.9, 54.4, 0, maxColorValue = 255),
      # rgb(204.9, 166.8, 100.2, maxColorValue = 255)
    ),
    labels = lbls[1:2]
  ) +
  scale_size_manual(values = rep(.4, 2), labels = lbls[1:2]) +
  scale_x_date(
    limits = c(as.Date("2002-01-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2002-01-01"), as.Date("2020-12-31"), by = "1 year"),
    date_labels = "%Y", expand = expansion(mult = c(.005, 0))
  ) +
  scale_y_continuous(
    breaks = seq(-.2, .2, .1),
    labels = c("-.2", "", "0", "", ".2"),
    limits = c(-.2, .2)
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
    plot.margin = margin(.1, .1, .1, .1, "cm")
  )

# ggsave(
#   plot = plt.iv.north, "export/anom_north_ndvi.png",
#   width = 15, height = 2, units = "cm", dpi = 500
# )

#'     central Andes
plt.iv.central <-
  ggplot(df.iv.central, aes(x = date, y = anom, group = region)) +
  annotate(
    geom = "text", x = as.Date("2002-04-01"), y = .1533333/2,
    label = "b)", color ="black", family = "Source Sans Pro"
  ) +
  labs(y = "NDVI anomaly") +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "black", size = .3
  ) +
  geom_hline(
    yintercept = c(-.1, -.05, .05, .1),
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
    date_labels = "%Y", expand = expansion(mult = c(.005, 0))
  ) +
  scale_y_continuous(
    breaks = seq(-.1, .1, .05),
    labels = c("-.1", "", "0", "", ".1"),
    limits = c(-.1, .1)
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
      face = c("plain", "plain", "bold", "plain", "plain")
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      family = "Source Sans Pro", color = "black", size = 8 # , face = "bold"
    ),
    axis.ticks.x = element_line(color = "black", size = .3),
    axis.ticks.y = element_line(color = "black", size = .3),
    axis.ticks.length.x = unit(1.5, "pt"),
    axis.ticks.length.y = unit(1.5, "pt"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(0, .1, .1, .1, "cm"),
  )

# ggsave(
#   plot = plt.iv.central, "export/anom_central_ndvi.png",
#   width = 15, height = 2, units = "cm", dpi = 500
# )

#'     south Andes
plt.iv.south <-
  ggplot(df.iv.south, aes(x = date, y = anom, group = region)) +
  annotate(
    geom = "text", x = as.Date("2002-04-01"), y = .1533333/2,
    label = "c)", color ="black", family = "Source Sans Pro"
  ) +
  labs(y = "NDVI anomaly") +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "black", size = .3
  ) +
  geom_hline(
    yintercept = c(-.1, -.05, .05, .1),
    linetype = "dashed", color = "black", size = .01
  ) +
  geom_line(aes(linetype = region, color = region, size = region)) +
  # geom_point(aes(color = region), size = .01) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = lbls[5:6]
  ) +
  scale_color_manual(
    values = c("black", "gray"),
    labels = lbls[5:6]
  ) +
  scale_size_manual(values = rep(.4, 2), labels = lbls[5:6]) +
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
    breaks = seq(-.1, .1, .05),
    labels = c("-.1", "", "0", "", ".1"),
    limits = c(-.1, .1)
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
    plot.margin = margin(0, .1, .1, .1, "cm")
  )

# ggsave(
#   plot = plt.iv.south, "export/anom_south_ndvi.png",
#   width = 15, height = 2, units = "cm", dpi = 500
# )

#'     arrange plots
plt <-
  ggarrange(
    plt.iv.north, plt.iv.central, plt.iv.south,
    ncol = 1, align = "v", widths = rep(15, 3), heights = c(2, 1.91 , 2.13)
  )
#'     save plot
ggsave(
  plot = plt, "export/anom_ndvi.png",
  units = "cm", dpi = 500, height = 6, width = 15
)
