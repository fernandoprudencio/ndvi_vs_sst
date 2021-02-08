#' 1| FUNCTION TO CALCULATE LAGGED CORRELATION
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

#' 2| FUNCTION TO PLOT LAGGED CORRELATION
plot.lc <- function(andes, ocean, pallete, data) {
  lbls <- c(
    "[-1,-0.8]", "<-0.8,0.6]", "<-0.6,-0.4]", "<-0.4,-0.2]", "<-0.2,0]",
    "<0,0.2]", "<0.2,0.4]", "<0.4,0.6]", "<0.6,0.8]", "<0.8,1]"
  )
  df <-
    lagged.cor(sst = ocean, ndvi = andes, data = data) %>%
    mutate(
      countfactor = cut(
        value,
        breaks = seq(-1, 1, .2),
        labels = lbls
      ),
      lag = as.integer(lag),
      month = sprintf("%02d", month)
    )

  title <- c(
    ts.nw = "NORTH-WESTERN ANDES", ts.cw = "CENTRAL-WESTERN ANDES",
    ts.sw = "SOUTH-WESTERN ANDES", ts.ne = "NORTH-EASTERN ANDES",
    ts.ce = "CENTRAL-EASTERN ANDES", ts.se = "SOUTH-EASTERN ANDES"
  )

  if (andes == "ts.nw" | andes == "ts.cw" | andes == "ts.sw") {
    space = .05
  } else {
    space = .3
  }

  #' PLOT HEATMAP
  ggplot(df, aes(x = lag, y = month, fill = countfactor)) +
    geom_tile(colour = "black", size = .2) +
    guides(
      fill = guide_legend(
        title = "Correlation Coefficient", nrow = 1, rot = 90,
        direction = "horizontal", title.position = "bottom",
        title.theme = element_text(
          size = 9, colour = "black", family = "Source Sans Pro", hjust = .5
        ),
        label.position = "bottom",
        label.theme = element_text(
          size = 8, colour = "black", family = "Source Sans Pro", angle = 0
        )
      )
    ) +
    labs(
      x = "Monthly Lag", title = title[andes],
      y = toupper(str_sub(ocean, 6, -1))
    ) +
    scale_x_continuous(breaks = seq(0, 11, 1), expand = c(0, 0)) +
    scale_y_discrete(
      labels = rev(month.abb),
      expand = c(0, 0)
    ) +
    scale_fill_manual(
      limits = lbls, values = rev(pallete)
    ) +
    theme_bw() +
    theme(
      legend.background =
        element_rect(fill = "transparent", color = "transparent", size = .2),
      legend.position = "bottom",
      legend.spacing.x = unit(0, "cm"),
      legend.margin = margin(grid::unit(0, "cm")),
      legend.key.height = grid::unit(.25, "cm"),
      legend.key.width = grid::unit(1.3, "cm"),
      legend.box.margin = margin(0, 0, 0, 0),

      axis.title = element_text(
        colour = "black", size = 8,
        family = "Source Sans Pro"
      ),
      axis.text.x =
        element_text(
          size = 8, colour = "black",
          family = "Source Sans Pro", vjust = 1
        ),
      axis.text.y =
        element_text(
          size = 8, colour = "black",
          hjust = 0
        ),
      axis.ticks = element_line(size = 0),
      axis.ticks.length.x = unit(0, "pt"),
      axis.ticks.length.y = unit(0, "pt"),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(.05, .05, space, .05, "cm"),
      plot.title = element_text(
        colour = "black", hjust = .5, vjust = -1, size = 8,
        face = "bold", family = "Source Sans Pro"
      )
    ) %>%
    return()
}

#' 3| FUNCTION TO SHARE ONE LEGEND WITH ANY GRAPHS
grid_arrange_shared_legend <-
  function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)

    combined <- switch(position,
      "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )

    grid.newpage()
    grid.draw(combined)

    # return gtable invisibly
    invisible(combined)
  }
