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
  labs(y = "GVMI") +
  geom_line(aes(linetype = region, color = region, size = region)) +
  #geom_point(aes(shape = type, color = type), size = 2) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = lbls[1:2]
  ) +
  scale_color_manual(
    values = c(
      rgb(105.9, 54.4, 0, maxColorValue = 255),
      rgb(241.9, 175.4, 105.2, maxColorValue = 255)
    ),
    labels = lbls[1:2]
  ) +
  scale_size_manual(values = rep(1, 2), labels = lbls[1:2]) +
  scale_x_date(
    limits = c(as.Date("2001-12-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2001-12-01"), as.Date("2020-12-31"), by = "1 year"),
    date_labels = "%y", expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = seq(-.2, .1, .05),
    limits = c(-.2, .1)
  ) +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin = margin(3, 7, 7, 7),
    # legend.key.size = unit(.8, "cm"),
    legend.key.width = unit(1.6, "cm"),
    legend.key.height = unit(1.1, "cm"),
    legend.position = c(0.77, 0.78),
    legend.title = element_blank(),
    legend.text = element_text(size = 15, family = "Source Sans Pro"),
    plot.title = element_text(size = 15, hjust = .5, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 12, colour = "black", family = "Source Sans Pro",
      face = "bold", angle = 0, vjust = .6
    ),
    axis.text.y = element_text(
      size = 13, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      face = "bold", family = "Source Sans Pro", color = "black", size = 20
    ),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm"),
    axis.line.y = element_line(
      size = .8, color = "black"
    )
  )

name <- sprintf("exports/%s_ssnl.png", k.index)

ggsave(
  plot = plt.iv, name,
  width = 20, height = 15, units = "cm", dpi = 500
)
