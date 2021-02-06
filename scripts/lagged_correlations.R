rm(list = ls())

library(tidyverse)
library(lattice)
library(cptcity)
library(latticeExtra)
library(grid)
library(RColorBrewer)
library(rasterVis)
library(gridExtra)

load("data/rdata/anomalies_iv-and-sst.RData")

#' 1.1| BUILD FUNCTION TO CALCULATE LAGGED CORRELATION
lagged.cor <- function(ndvi, sst, data) {
  for (k in 1:12) {
    anom.sst <-
      dplyr::select(data, date, sst) %>%
      dplyr::filter(date >= "2002-06-01" & date <= "2020-06-01") %>%
      dplyr::filter(str_sub(date, 6, 7) == sprintf("%02d", k))
    names(anom.sst)[2] <- "anom.sst"

    for (i in 0:11) {
      anom.ndvi <-
        dplyr::select(data, date, ndvi) %>%
        dplyr::filter(date >= "2002-06-01" & date <= "2020-06-01") %>%
        dplyr::filter(date %in% (anom.sst$date + months(i)))
      names(anom.ndvi)[2] <- "anom.ndvi"

      x <- anom.sst$anom.sst[1:nrow(anom.ndvi)]
      y <- anom.ndvi$anom.ndvi
      value <- cor(x, y, method = "pearson")

      if (i == 0) r <- value else r <- c(r, value)
    }

    if (k == 1) df.r <- matrix(r, nrow = 1) else df.r <- rbind(df.r, r)
  }
  # raster(df.r,
  #   xmn = 0, xmx = 12,
  #   ymn = 1, ymx = 13
  # ) %>% return()
  as.tibble(df.r) %>%
    mutate(month = 12:1) %>%
    gather(key = "lag", value = "value", -month) %>%
    mutate(lag = as.numeric(str_sub(lag, 2, 3)) - 1) %>%
    return()
}

#' 1.2| BUILD FUNCTION TO CALCULATE LAGGED CORRELATION
andes.region <- c("ts.nw", "ts.cw", "ts.sw", "ts.ne", "ts.ce", "ts.se")
list.cor <-
  lapply(andes.region, FUN = lagged.cor, sst = "anom.oni", data = df.anom)

#' 2.1| BUILD FUNCTION TO PLOT LAGGED CORRELATION
plot.lc <- function(andes, ocean, pallete, data) {
  df <- lagged.cor(sst = ocean, ndvi = andes, data = df.anom)
  title <- c(
    ts.nw = "NORTH-WESTERN ANDES", ts.cw = "CENTRAL-WESTERN ANDES",
    ts.sw = "SOUTH-WESTERN ANDES", ts.ne = "NORTH-EASTERN ANDES",
    ts.ce = "CENTRAL-EASTERN ANDES", ts.se = "SOUTH-EASTERN ANDES"
  )

  if (andes == "ts.ce") {
    colkey <- list(
      at = seq(-1, 1, .2),
      space = "bottom", # location of legend
      labels = list(
        at = c(-1, -.6, -.2, 0, .2, .6, 1), cex = .8, col = "black",
        family = "Source Sans Pro"
      ),
      font = list(family = "Source Sans Pro"),
      axis.line = list(lwd = 1, col = "black"),
      height = .95, width = 1.2, tck = .5
    )
    subtitle <- list(
      "Correlation Coefficient",
      cex = .6, hjust = .35, vjust = .3,
      fontface = "plain", line = 0.5, side = "right"
    )
  } else {
    colkey <- NULL
    subtitle <- NULL
  }

  #' PLOT HEATMAP
  levelplot(value ~ lag * month,
    data = df,
    main =
      list(
        title[andes],
        cex = .8, hjust = .4, vjust = 2.1,
        fontface = "plain", line = 0.5, side = "right"
      ),
    sub = subtitle,
    at = seq(-1, 1, .2),
    margin = T,
    scales.margin = list(x = NULL, y = NULL),
    pretty = T,
    col.regions = rev(pallete),
    colorkey = colkey,
    xlab = list("Monthy Lag", cex = .8, fontfamily = "Source Sans Pro", vjust = -.8),
    ylab = list("ONI", cex = .8, fontfamily = "Source Sans Pro", vjust = 2),
    scales = list(
      x = list(
        cex = .8, label = -1:11, tick.number = 12,
        fontfamily = "Source Sans Pro"
      ),
      y = list(
        cex = .8, label = c("x", rev(month.abb)), tick.number = 12,
        fontfamily = "Source Sans Pro"
      )
    ),
    label.style = "align",
    lwd = 1,
    region = T, # show value by pixel
    border = "black", # line color of gride
    border.lty = 1, # line type of gride
    border.lwd = 1.2, # line width of gride
    par.settings = list(
      axis.text = list(fontfamily = "Source Sans Pro", cex = 1),
      axis.components = list(
        bottom = list(tck = 0, pad1 = .5),
        top = list(tck = 0, pad1 = 0),
        left = list(tck = 0, pad1 = .5),
        right = list(tck = 0, pad1 = 0)
      ),
      strip.background = list(col = "transparent"), # header fill for each map
      strip.border = list(col = "transparent", lwd = 1), # header line for each map
      panel.background = list(col = "gray"),
      axis.line = list(lwd = 1, col = "black")
    ),
    par.xlab.text = list(fontfamily = "Source Sans Pro"),
    par.ylab.text = list(fontfamily = "Source Sans Pro"),
    par.sub.text = list(fontfamily = "Source Sans Pro"),
    par.main.text = list(fontfamily = "Source Sans Pro")
  ) %>%
    return()
}

#' 2.2| APPLY FUNCTION TO PLOT LAGGED CORRELATION
andes.region <- c("ts.nw", "ts.cw", "ts.sw", "ts.ne", "ts.ce", "ts.se")
cb.palette <-
  c(
    "#00315f", "#1367a9", "#3594c0", "#cfe5ef",
    "#ffffff", "#ffffff",
    "#ffdbc9", "#de5f51", "#ba142e", "#6c001f"
  )

plt <- lapply(
  andes.region,
  FUN = plot.lc, ocean = "anom.oni",
  pallete = cb.palette, data = df.anom
)

plt[[1]]


lbls <- c(
  "[-1,-0.8]", "<-0.8,0.6]", "<-0.6,-0.4]", "<-0.4,-0.2]", "<-0.2,0]",
  "<0,0.2]", "<0.2,0.4]", "<0.4,0.6]", "<0.6,0.8]", "<0.8,1]"
)

table <-
  list.cor[[1]] %>%
  mutate(
    countfactor = cut(
      value,
      breaks = seq(-1, 1, .2),
      labels = lbls
    ),
    lag = as.integer(lag),
    month = sprintf("%02d", month)
  )

ggplot(table, aes(x = lag, y = month, fill = countfactor)) +
  geom_tile(colour = "black", size = .2) +
  guides(
    fill = guide_legend(
      title = "Correlation Coefficient", nrow = 1, rot = 90,
      direction = "horizontal", title.position = "bottom",
      title.theme = element_text(
        size = 15, colour = "black", family = "Source Sans Pro", hjust = .5
      ),
      label.position = "bottom",
      label.theme = element_text(
        size = 12, colour = "black", family = "Source Sans Pro", angle = 0
      )
    )
  ) +
  labs(
    x = "Monthly Lag", y = "ONI", title = "NORTH-WESTERN ANDES"
  ) +
  scale_x_continuous(breaks = seq(0, 11, 1), expand = c(0, 0)) +
  scale_y_discrete(
    labels = rev(month.abb),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    limits = lbls, values = rev(cb.palette)
  ) +
  theme_bw() +
  theme(
    legend.background =
      element_rect(fill = "transparent", color = "transparent", size = .2),
    legend.position = "bottom",
    legend.spacing.x = unit(0, 'cm'),
    legend.title = element_text(
      hjust = .5, colour = "black", family = "Source Sans Pro"
    ),
    legend.margin = margin(grid::unit(0, "cm")),
    # legend.text =
    #   element_text(
    #     colour = "black", size = 3, face = "bold", family = "Source Sans Pro"
    #   ),
    legend.key.height = grid::unit(0.5, "cm"),
    legend.key.width = grid::unit(2, "cm"),
    legend.box.margin = margin(0, 0, 0, 0),

    axis.title = element_text(
      colour = "black", size = 15, face = "bold", family = "Source Sans Pro"
    ),
    axis.text.x =
      element_text(
        size = 10, colour = "black", family = "Source Sans Pro"
      ),
    axis.text.y =
      element_text(
        size = 10, colour = "black", family = "Source Sans Pro"
      ),
    axis.ticks = element_line(size = 0),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank(),
    plot.margin = margin(.1, .1, .1, .1, "cm"),
    plot.title = element_text(
      colour = "black", hjust = .5, size = 14, face = "bold",
      family = "Source Sans Pro"
    )
  )



 # read csv file
m <- read.csv("data/tables/measles_lev1.csv", header = T, stringsAsFactors = F, skip = 2)

# inspect data
head(m)
str(m)
table(m$YEAR)
table(m$WEEK)

m2 <- m %>%
  # convert data to long format
  gather(key = "state", value = "value", -YEAR, -WEEK) %>%
  # rename columns
  setNames(c("year", "week", "state", "value")) %>%
  # convert year to factor
  mutate(year = factor(year)) %>%
  # convert week to factor
  mutate(week = factor(week)) %>%
  # convert value to numeric (also converts '-' to NA, gives a warning)
  mutate(value = as.numeric(value))

fn_tc <- function(x) paste(str_to_title(unlist(strsplit(x, "[.]"))), collapse = " ")
m2$state <- sapply(m2$state, fn_tc)

na_sum <- function(x) {
  if (all(is.na(x))) val <- sum(x, na.rm = F)
  if (!all(is.na(x))) val <- sum(x, na.rm = T)
  return(val)
}

# sum incidences for all weeks into one year
m3 <- m2 %>%
  group_by(year, state) %>%
  summarise(count = na_sum(value)) %>%
  as.data.frame()

m4 <- m3 %>%
  # convert state to factor and reverse order of levels
  mutate(state = factor(state, levels = rev(sort(unique(state))))) %>%
  # create a new variable from count
  mutate(countfactor = cut(count,
    breaks = c(-1, 0, 1, 10, 100, 500, 1000, max(count, na.rm = T)),
    labels = c("0", "0-1", "1-10", "10-100", "100-500", "500-1000", ">1000")
  )) %>%
  # change level order
  mutate(countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor))))
textcol <- "grey40"
as_tibble(m4)
ggplot(m4, aes(x = year, y = state, fill = countfactor)) +
  geom_tile(colour = "white", size = 0.2) +
  guides(fill = guide_legend(title = "Cases per\n100,000 people")) +
  labs(x = "", y = "", title = "Incidence of Measles in the US") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0), breaks = c("1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000")) +
  scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#ddf1da"), na.value = "grey90") +
  # coord_fixed()+
  theme_grey(base_size = 10) +
  theme(
    legend.position = "right", legend.direction = "vertical",
    legend.title = element_text(colour = textcol),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(colour = textcol, size = 7, face = "bold"),
    legend.key.height = grid::unit(0.8, "cm"),
    legend.key.width = grid::unit(0.2, "cm"),
    axis.text.x = element_text(size = 10, colour = textcol),
    axis.text.y = element_text(vjust = 0.2, colour = textcol),
    axis.ticks = element_line(size = 0.4),
    plot.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(colour = textcol, hjust = 0, size = 14, face = "bold")
  )

# intervals <- seq(-1, 1, .2)
# binned <- cut(table$value, breaks = intervals)
# colfunc <- colorRampPalette(c("yellow", "black", "steelblue"))
# colgroups <- colfunc(length(levels(binned)))
# res <- colgroups[as.integer(binned)]
# res <- factor(res, levels = colgroups)
# build_labels <- function(breaks) {
#   labels <- gsub('\\([^,]*,(-?\\d+).*', '\\1', levels(binned))
#   c(head(labels, -1), '')
# }

# ggplot(table, aes(lag, month, fill = res)) +
#   geom_tile(color = "black") +
#   scale_fill_manual(values = levels(res), labels = build_labels) +
#   guides(fill = guide_legend(label.vjust = -0.25))
#
# scale_fill_manual(values = levels(res), labels = levels(binned))
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

#' SAVE PLOT
png(
  "export/lag_cor.png",
  width = 9, height = 10, units = "cm", res = 1000
)



grid <- mutate(
  list.cor[[4]],
  value2 = list.cor[[5]]$value,
  value3 = list.cor[[6]]$value,
  value4 = list.cor[[1]]$value,
  value5 = list.cor[[2]]$value,
  value6 = list.cor[[3]]$value,
) %>%
  rename("value1" = "value") %>%
  gather(
    key = "region", value = "value", -month, -lag
  ) # %>%
# mutate(
#   region = replace(region, region == "value1", "NORTH-EASTERN ANDES"),
#   region = replace(region, region == "value2", "CENTRAL-EASTERN ANDES"),
#   region = replace(region, region == "value3", "SOUTH-EASTERN ANDES"),
#   region = replace(region, region == "value4", "NORTH-WESTERN ANDES"),
#   region = replace(region, region == "value5", "CENTRAL-WESTERN ANDES"),
#   region = replace(region, region == "value6", "SOUTH-WESTERN ANDES")
# )

levelplot(value ~ lag * month | region,
  data = grid,
  layout = c(3, 2), # define number of row and colums
  names.attr = rep("fff", 6),
  par.strip.text = list(cex = 1, lines = 3), # header size for each map
  sub = list(
    "Correlation Coefficient",
    cex = .6, hjust = .35, vjust = .3,
    fontface = "plain", line = 0.5, side = "right"
  ),
  scales = list(
    x = list(
      cex = .8, label = -1:11, tick.number = 12,
      fontfamily = "Source Sans Pro"
    ),
    y = list(
      cex = .8, label = c("x", rev(month.abb)), tick.number = 12,
      fontfamily = "Source Sans Pro"
    )
  ),
  at = seq(-1, 1, .2),
  margin = T,
  pretty = T,
  col.regions = rev(cb.palette),
  colorkey = list(
    at = seq(-1, 1, .2),
    space = "bottom", # location of legend
    labels = list(
      at = c(-1, -.6, -.2, 0, .2, .6, 1), cex = .8, col = "black",
      family = "Source Sans Pro"
    ),
    font = list(family = "Source Sans Pro"),
    axis.line = list(lwd = 1, col = "black"),
    height = .95, width = 1.2, tck = .5
  ),
  xlab = list("Monthy Lag", cex = .8, fontfamily = "Source Sans Pro", vjust = -.8),
  ylab = list("ONI", cex = .8, fontfamily = "Source Sans Pro", vjust = 2),
  label.style = "align",
  lwd = 1,
  region = T, # show value by pixel
  border = "black", # line color of gride
  border.lty = 1, # line type of gride
  border.lwd = 1.2, # line width of gride
  par.settings = list(
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1),
    axis.components = list(
      bottom = list(tck = 0, pad1 = 1),
      top = list(tck = 0, pad1 = 1),
      left = list(tck = 0, pad1 = 1),
      right = list(tck = 0, pad1 = 1)
    ),
    strip.text = list(col = "blue"), # header fill for each map
    strip.background = list(col = "red"), # header fill for each map
    strip.border = list(col = "black", lwd = 3), # header line for each map
    panel.background = list(col = "gray"),
    axis.line = list(lwd = 1, col = "black")
  ),
  par.xlab.text = list(fontfamily = "Source Sans Pro"),
  par.ylab.text = list(fontfamily = "Source Sans Pro"),
  par.sub.text = list(fontfamily = "Source Sans Pro"),
  par.main.text = list(fontfamily = "Source Sans Pro")
)

# names(p)
########################

layers <- c(1:4)
s2 <- stack()

for (i in layers) {
  r <- raster(nrows = 100, ncols = 100)
  r[] <- sample(seq(from = 1, to = 6, by = 1), size = 10000, replace = TRUE)
  rasc <- ratify(r)
  rat <- levels(rasc)[[1]]
  rat$legend <- c("A", "B", "C", "D", "E", "F")
  levels(rasc) <- rat
  s2 <- stack(s2, rasc)
}

# using default names
levelplot(s2, names.attr = rep("fff", 4), col.regions = rev(terrain.colors(6)), main = "example")








marrangeGrob(plt)

library(ggpubr)
grid.draw(
  plt[[1]], plt[[2]], plt[[3]],
  plt[[4]], plt[[5]], plt[[6]]
)

plot_grid(
  plt[[1]], plt[[2]], plt[[3]], plt[[4]], plt[[5]], plt[[6]],
  ncol = 3, align = "hv"
)



ggarrange(
  plt[[1]], plt[[2]], plt[[3]], plt[[4]], plt[[5]], plt[[6]],
  align = "hv", ncol = 3, nrow = 2, common.legend = F
)

#' CLOSE THE SAVED OF PLOT
dev.off()
