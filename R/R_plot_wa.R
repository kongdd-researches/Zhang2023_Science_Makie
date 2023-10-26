library(Ipaper)
library(ggplot2)
library(gg.layers)
library(terra)
library(sf2)
library(tidyterra)
library(rcolors)

fs <- dir2("./data/yearly_anom/")[-1]
r <- rast(fs[1])
shp <- shp_continent

nbrk <- 10
cols <- get_color(rcolors$amwg256, nbrk) %>% rev()
# cols[c(0, 1) + nbrk / 2] <- "grey90"
label <- "WA"

debug = T
debug = F
# Ipaper::set_font()

plot_tif <- function(r, year=2001) {
  d = as.data.table(r, xy = TRUE) |> set_names(c("x", "y", "z"))
  fout <- glue("Figures/wa_anom_{year}.pdf")

  # year <- 2001
  p <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = shp, color = "black", linewidth = 0.2, fill = "transparent") +
    labs(title = glue("Water availability anomaly in {year} (mm/year)")) +
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
      plot.margin = margin(t=0, b=-10, l=3, r = 15),
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
  write_fig(p, fout, 11.5, 5, show=T)
}


foreach(year = 2001:2020, f = fs, i = icount(1)) %do% {
  runningId(i)
  ra = rast(f)
  plot_tif(ra, year)
}
