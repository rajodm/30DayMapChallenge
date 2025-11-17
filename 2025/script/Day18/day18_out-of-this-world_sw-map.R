# Packages -------------------------------------------------------

library(sf)
library(tidyverse)
library(ggshadow)
library(ggrepel)
library(ggtext)
library(systemfonts)

# Data -----------------------------------------------------------

source(here::here("2025/script/Day18", "planet_list.R"))

planets <- read_sf(here::here("2025/data/sw", "planets_new.gpkg")) |>
  mutate(
    zm = replace_na(zm, 4),
    zm = factor(zm)
  )

planets_hgh <- planets |>
  filter(planet %in% planets_highlights)

hs_lanes <- read_sf("2025/data/sw/hyperspace-lanes.gpkg")

grids <- read_sf("2025/data/sw/grid.gpkg")

labels <- grids |>
  filter(grid %in% c("O2", "V11", "P20", "G20", "B11", "I11")) |>
  mutate(
    label = case_when(
      grid == "O2" ~ "New Territories",
      grid == "V11" ~ "The Slice",
      grid == "P20" ~ "Trailing<br>Sectors",
      grid == "G20" ~ "Western<br>Reaches",
      grid == "B11" ~ "Unknown Regions",
      grid == "I11" ~ "The Interior"
    )
  ) |>
  st_centroid()

cregions <- read_sf("2025/data/sw/cregion.gpkg")


# Colors ---------------------------------------------------------

color_black <- "#15151c"
color_yellow <- "#cf9f52"
color_white <- "#f5f4ed"

# Texts ----------------------------------------------------------

register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

src <- glue::glue(
  "Data: www\\.swgalaxymap\\.com - ",
  "https:\\/\\/iskore\\.github\\.io\\/star-wars-galaxy"
)
notes <- "Note: planets labeled for clarity, not importance"
chart <- "#30DayMapChallenge 2025 Day 18 - Out of this world"
bsky <- glue::glue("<span style='font-family:fa7-brands;'>&#xe671;</span>")
author <- glue::glue("Visualization: {bsky} @rajodm")
caption_text <- glue::glue("{notes}<br>{src}<br>{chart} | {author} | #rstats")

title <- "Some planets in a galaxy far, far away..."

subtitle <- glue::glue(
  "<span style='color:{color_yellow}'>**Lines**</span> show <span style='color:#f7b732;'>**hyperlane routes**</span>",
  " and **quadrant regions**",
)


# Plot -----------------------------------------------------------

theme_set(theme_void())

proj <- "Proj4 +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext
+no_defs"

map <- ggplot() +
  geom_sf(
    data = cregions,
    aes(fill = cregion),
    linewidth = .03,
    color = color_white,
    alpha = .06,
    show.legend = FALSE
  ) +
  geom_sf(
    data = hs_lanes,
    linewidth = .2,
    alpha = .8,
    linetype = "1234",
    color = color_yellow,
  ) +
  geom_sf(
    data = planets,
    aes(size = zm, alpha = zm),
    color = color_white,
    shape = 16,
    show.legend = FALSE
  ) +
  geom_glowpoint(
    data = planets_hgh,
    aes(geometry = the_geom, size = zm, alpha = zm),
    color = color_white,
    shadowsize = .14,
    shadowcolor = color_white,
    shadowalpha = .08,
    stat = "sf_coordinates",
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = planets_hgh,
    aes(label = str_wrap(planet, 3), geometry = the_geom),
    size = 2.63,
    lineheight = .8,
    family = "Atkinson Hyperlegible Next",
    color = color_white,
    box.padding = .1,
    segment.color = color_white,
    stat = "sf_coordinates",
    seed = 123,
    show.legend = FALSE
  ) +
  geom_richtext(
    data = labels,
    aes(label = label, geometry = the_geom),
    size = 2.80,
    alpha = .8,
    label.padding = unit(rep(0, 4), "lines"),
    label.r = unit(0, "lines"),
    label.colour = NA,
    text.color = color_yellow,
    fill = NA,
    stat = "sf_coordinates",
    family = "Roboto",
    fontface = "bold"
  ) +
  coord_sf(
    crs = proj,
    expand = FALSE,
    clip = "off",
  ) +
  scale_size_manual(
    values = c(
      "0" = .25,
      "1" = .12,
      "2" = .12,
      "3" = .05,
      "4" = .05
    )
  ) +
  scale_alpha_manual(
    values = c(
      "0" = 1,
      "1" = 1,
      "2" = 1,
      "3" = .5,
      "4" = .2
    )
  ) +
  scale_fill_manual(
    values = c(
      "Unknown Regions" = "#E73F74",
      "New Territories" = "#F1CE63",
      "Western Reaches" = "#009988",
      "The Slice" = "#9467BD",
      "The Interior" = "#225555",
      "Trailing Sectors" = "#882255"
    )
  ) +
  labs(
    title = title,
    caption = caption_text,
    subtitle = subtitle
  ) +
  theme_sub_panel(
    background = element_rect(fill = color_black, color = NA),
  ) +
  theme_sub_plot(
    background = element_rect(fill = color_black, color = NA),
    margin = margin(t = 1, b = 1, r = -1, l = -1, unit = "lines"),
    title.position = "plot",
    title = element_textbox(
      fill = "#ffd700",
      family = "Roboto",
      size = 10,
      padding = margin(1.8, 1.8, 1.8, 1.8),
      color = color_black,
    ),
    subtitle = element_textbox_simple(
      family = "Roboto",
      size = 8,
      color = color_white,
      margin = margin(t = 6, b = 12)
    ),
    caption.position = "plot",
    caption = element_textbox_simple(
      family = "Roboto",
      size = 6,
      margin = margin(t = 12),
      color = color_white,
      halign = 1
    )
  )


ggh4x::save_plot(
  here::here("2025/maps/day18_out-of-this-world_sw-map.png"),
  plot = map,
  width = 7.5,
  height = 7.5,
  units = "in",
  dpi = 250
)
