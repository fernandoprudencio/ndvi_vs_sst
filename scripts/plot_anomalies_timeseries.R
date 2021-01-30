rm(list = ls())

library(tidyverse)

load("data/rdata/anomalies_iv-and-sst.RData")

df.anom

df.iv.north <-
  dplyr::select(
    df.anom, c("date", "ts.nw", "ts.ne")
  ) %>%
  gather(key = "region", value = "anom", -date)

df.iv.central <-
  dplyr::select(
    df.anom, c("date", "ts.cw", "ts.ce")
  ) %>%
  gather(key = "region", value = "anom", -date)

df.iv.south <-
  dplyr::select(
    df.anom, c("date", "ts.sw", "ts.se")
  ) %>%
  gather(key = "region", value = "anom", -date)


lbls <-
  c(
    "North-western Andes", "North-eastern Andes",
    "Central-western Andes", "Central-eastern Andes",
    "South-western Andes", "South-eastern Andes"
  )

plt.iv.north <-
  ggplot(df.iv.north, aes(x = date, y = anom, group = region)) +
  labs(y = "NDVI anomaly") +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "black", size = .3
  ) +
  geom_line(aes(linetype = region, color = region, size = region)) +
  # geom_point(aes(color = region), size = .01) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = lbls[2:1]
  ) +
  scale_color_manual(
    values = c(
      # "black", "gray"
      rgb(105.9, 54.4, 0, maxColorValue = 255),
      rgb(204.9, 166.8, 100.2, maxColorValue = 255)
    ),
    labels = lbls[2:1]
  ) +
  scale_size_manual(values = rep(.4, 2), labels = lbls[2:1]) +
  scale_x_date(
    limits = c(as.Date("2002-01-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2003-01-01"), as.Date("2020-12-31"), by = "1 year"),
    date_labels = "%Y", expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = seq(-.2, .2, .1),
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
    legend.position = c(0.5, 0.85),
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
    axis.title.y = element_text(
      family = "Source Sans Pro", color = "black", size = 8 # , face = "bold"
    ),
    axis.ticks.x = element_line(color = "black", size = .3),
    axis.ticks.y = element_line(color = "black", size = .3),
    axis.ticks.length.x = unit(1.5, "pt"),
    axis.ticks.length.y = unit(1.5, "pt"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm") # ,
  )

ggsave(
  plot = plt.iv.north, "export/anom_north_ndvi.png",
  width = 15, height = 4.5, units = "cm", dpi = 500
)

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################

plt.iv.central <-
  ggplot(df.iv.central, aes(x = date, y = anom, group = region)) +
  labs(y = "NDVI anomaly") +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "black", size = .3
  ) +
  geom_line(aes(linetype = region, color = region, size = region)) +
  # geom_point(aes(color = region), size = .01) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = lbls[4:3]
  ) +
  scale_color_manual(
    values = c(
      # "black", "gray"
      rgb(105.9, 54.4, 0, maxColorValue = 255),
      rgb(204.9, 166.8, 100.2, maxColorValue = 255)
    ),
    labels = lbls[4:3]
  ) +
  scale_size_manual(values = rep(.4, 2), labels = lbls[4:3]) +
  scale_x_date(
    limits = c(as.Date("2002-01-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2003-01-01"), as.Date("2020-12-31"), by = "1 year"),
    date_labels = "%Y", expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = seq(-.2, .2, .1),
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
    legend.position = c(0.5, 0.85),
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
    axis.title.y = element_text(
      family = "Source Sans Pro", color = "black", size = 8 # , face = "bold"
    ),
    axis.ticks.x = element_line(color = "black", size = .3),
    axis.ticks.y = element_line(color = "black", size = .3),
    axis.ticks.length.x = unit(1.5, "pt"),
    axis.ticks.length.y = unit(1.5, "pt"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm") # ,
  )

ggsave(
  plot = plt.iv.central, "export/anom_central_ndvi.png",
  width = 15, height = 4.5, units = "cm", dpi = 500
)

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################

plt.iv.south <-
  ggplot(df.iv.central, aes(x = date, y = anom, group = region)) +
  labs(y = "NDVI anomaly") +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "black", size = .3
  ) +
  geom_line(aes(linetype = region, color = region, size = region)) +
  # geom_point(aes(color = region), size = .01) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = lbls[4:3]
  ) +
  scale_color_manual(
    values = c(
      # "black", "gray"
      rgb(105.9, 54.4, 0, maxColorValue = 255),
      rgb(204.9, 166.8, 100.2, maxColorValue = 255)
    ),
    labels = lbls[4:3]
  ) +
  scale_size_manual(values = rep(.4, 2), labels = lbls[4:3]) +
  scale_x_date(
    limits = c(as.Date("2002-01-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2003-01-01"), as.Date("2020-12-31"), by = "1 year"),
    date_labels = "%Y", expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = seq(-.2, .2, .1),
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
    legend.position = c(0.5, 0.85),
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
    axis.title.y = element_text(
      family = "Source Sans Pro", color = "black", size = 8 # , face = "bold"
    ),
    axis.ticks.x = element_line(color = "black", size = .3),
    axis.ticks.y = element_line(color = "black", size = .3),
    axis.ticks.length.x = unit(1.5, "pt"),
    axis.ticks.length.y = unit(1.5, "pt"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm") # ,
  )

ggsave(
  plot = plt.iv.south, "export/anom_central_ndvi.png",
  width = 15, height = 4.5, units = "cm", dpi = 500
)


