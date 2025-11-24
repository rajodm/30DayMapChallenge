# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(marmap)
library(metR)
library(rayshader)
library(systemfonts)


# Texts & Fonts ----------------------------------------------------------

register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

title <- glue::glue(
  "<span style='font-family: Cinzel; color: #1a3a5c; font-size:16pt;'>",
  "**The Great Barrier Reef**</span>"
)

subtitle <- glue::glue(
  "<span style='font-family: Cinzel; font-size:9pt; color: #4a4a4a;'>",
  "Northeastern Australia - UNESCO World Heritage Site</span>"
)

src <- glue::glue("Bathymetric data from NOAA - {{marmap}} R package")

chart <- "#30DayMapChallenge 2025 Day 20 & 24 - Water, Places and their names"

bsky <- glue::glue("<span style='font-family:fa7-brands;'>&#xe671;</span>")

author <- glue::glue("Visualization: {bsky} @rajodm")

caption_text <- glue::glue(
  "{title}<br>",
  "{subtitle}<br><span style ='font-family: Outfit; font-size:6pt; color: #4a4a4a'>",
  "{src}<br>{chart} | {author} | #rstats | #rayshader</span>"
)


# Data -------------------------------------------------------------------

# Get data from NOAA
gbr_large <- getNOAA.bathy(
  lon1 = 142,
  lon2 = 154,
  lat1 = -24.5,
  lat2 = -10,
  resolution = .5
)

gbr_df_large <- fortify.bathy(gbr_large)
# write_csv(gbr_df_large, "gbr_df_large.csv")

gbr_map <- gbr_df_large |>
  ggplot(aes(x = x, y = y, z = z)) +
  geom_contour_fill(bins = 20, show.legend = FALSE) +
  geom_contour_tanaka(
    bins = 20,
    sun.angle = 35,
    light = "#fefdfa",
    dark = "#171412",
    range = c(.01, .02),
    show.legend = FALSE,
    smooth = .5
  ) +
  cols4all::scale_fill_continuous_c4a_div(
    "met.hiroshige",
    mid = 0,
    reverse = TRUE,
    n = 12
  ) +
  annotate(
    "richtext",
    x = 148,
    y = -24,
    label = caption_text,
    color = NA,
    fill = NA,
    label.padding = unit(c(0, 0, 0, 0), "lines"),
    label.r = unit(0, "lines"),
    lineheight = .4,
    hjust = .5,
    vjust = 0
  ) +
  coord_fixed(expand = FALSE) +
  theme_void() +
  # Useless?
  theme_sub_plot(
    background = element_rect(fill = "#fcfaf8", color = NA)
  ) +
  theme_sub_panel(
    background = element_rect(fill = "#fcfaf8", color = NA)
  )

plot_gg(
  gbr_map,
  sunangle = 35,
  shadow = FALSE,
  phi = 90,
  theta = 0,
  zoom = .62,
  height = 7.5,
  width = 6.4,
  scale = 250,
  windowsize = c(2560, 3000),
  multicore = TRUE
)

render_snapshot(
  here::here("2025/maps", "day24_places-and-names_gbr.png"),
  clear = TRUE
)
