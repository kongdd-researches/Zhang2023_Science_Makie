library(Ipaper)
library(ggplot2)
library(gg.layers)
library(terra)
library(sf2)
library(tidyterra)
library(rcolors)
library(data.table)
library(dplyr)
library(gggrid)


df = read_xlsx("./data/Ensemble_anomaly.xlsx")[1:20, c(1, 5:7)] |> 
  melt(c("year"))

col_text <- function(head = "", x = "hello world", col = "black") {
  glue("<span style = 'color:{col};'>{head}{x}</span>")
}

make_title <- function(d) {
  c(g, n, s) %<-% sprintf("%6.2f", d$value)

  # <br>
  title_main <- glue("Water availability anomaly in {year}")
  title_sub <- glue('{col_text("Global: ", g)}, {col_text("NH: ", n, col="red")}, {col_text("SH: ", s, col="blue")}')

  title <- glue("<span style = 'font-size:16pt; font-weight:bold;'>{title_main}</span>: <span style = 'font-size:13pt; font-weight:normal;'>{title_sub} (mm year<sup> -1</sup>)</span>")
  title
}

plot_line <- function(year0) {
  d <- df[year == year0]
  title = make_title(d)

  lwd <- 1
  ylab <- expression(bold("WA anomaly")) # (mm " * year^-1 * "year )
  p <- ggplot(df, aes(year, value, color = variable, shape = variable)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.2, linetype = 2) + 
    geom_vline(linetype = 2, xintercept = year, linewidth = lwd, color = "red") +
    geom_line(linewidth = lwd) +
    geom_point(data = d, size = 5) +
    scale_color_manual(values = c("black", "red", "blue")) +
    # labs(title = glue("Water availability anomaly in {year} (mm/year)")) +
    guides(color = guide_legend(nrow=1), shape = guide_legend(nrow=1)) + 
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_markdown(margin = margin(t = -2, b = 2)),
      plot.margin = margin(b = 2, t = 10, l=0, r=3),
      panel.grid.major = element_line(linewidth = 0.3, linetype = 2),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      legend.key.width = unit(1, 'cm'), 
      legend.position = c(1, 1) - 0.005,
      legend.justification = c(1, 1)
    ) +
    coord_fixed(ratio = 1/56) +
    scale_x_continuous(breaks = seq(2002, 2020, 2), expand = c(0.02, 0)) +
    scale_y_continuous(breaks = seq(-80, 80, 40), limits = c(-80, 80)) +
    labs(y = NULL, color = NULL, shape = NULL, x = NULL, title = title)
  p
}

plot_tif <- function(r, year=2001, debug=TRUE, show=T) {
  d = as.data.table(r, xy = TRUE) |> set_names(c("x", "y", "z"))
  if (debug) {
    fout <- glue("Figures/wa_anom_{year}.pdf")
  } else {
    fout <- glue("Figures/wa_anom_{year}.png")
  }
  

  # year <- 2001
  p1 <- plot_line(year)
  p2 <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = shp, color = "black", linewidth = 0.2, fill = "transparent") +
    theme_grey(base_size = 14, base_family = "Times") +
    theme(
      panel.border = element_rect(color = "black", fill = "transparent"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      title = element_text(face = "bold", size = 16),
      legend.position = c(0, 0),
      legend.justification = c(0, 0),
      legend.title = element_blank(),
      legend.key.heigh = unit(0.8, "cm"),
      legend.background = element_rect(fill = "transparent"), 
      plot.margin = margin(t=0, b=0, l=0, r = 3),
    ) +
    geom_latFreq(
      data = d, aes2(y, z),
      options = list(is_spatial = T, zlim = c(-1, 1) * 100, ylabels = FALSE, col.regions = c("red", "blue")),
      bbox = c(183, 240, -60, 90)
    ) +
    # scale_x_continuous(limits = c(-180, 240), breaks = seq(-180, 180, 60))
    # annotate_richtext_npc(
    #   x = 0.99, y = 0.97, hjust = 1, vjust = 1,
    #   label = label, size = 5.5, fontface = "bold"
    # ) +
    labs(x = NULL, y = NULL) + 
    scale_fill_gradientn2(
      na.value = "transparent",
      colors = cols,
      limits = c(-1, 1) * 500,
      # oob = censor2,
      labels = scales::label_number(suffix = ""),
      guide = colourbar_triangle()
    ) +
    scale_y_continuous(limits = c(-60, 90), breaks = seq(-60, 90, 30)) + 
    coord_sf(xlim = c(-180, 240), ylim = c(-60, 90), expand = FALSE, clip="on")
  
  # p = p1 + p2 + plot_layout(heights = c(1.5, 3))
  p = plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1.7, 3))
  write_fig(p, fout, 11, 6, show=show)
}
