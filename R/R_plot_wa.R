#! /usr/bin/Rscript --no-init-file
# Dongdong Kong ----------------------------------------------------------------
# Copyright (c) 2022 Dongdong Kong. All rights reserved.
source('R/main_pkgs.R')
library(patchwork)
library(cowplot)

# r <- rast(fs[1])
shp <- shp_continent
nbrk <- 10
cols <- get_color(rcolors$amwg256, nbrk) %>% rev()
# cols[c(0, 1) + nbrk / 2] <- "grey90"
# Ipaper::set_font()
fs <- dir2("./data/yearly_anom/")[-1]

foreach(year = 2001:2020, f = fs, i = icount()) %do% {
  runningId(i)
  ra = rast(f)
  plot_tif(ra, year, show=F, debug=F)
  # plot_tif(ra, year, show=T, debug=T)
}
