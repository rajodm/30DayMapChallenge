# Libraries ------------------------------------------------------

library(osmdata)
library(sf)
library(dplyr)
library(stringr)
library(tmap)
library(ggplot2)
library(colorspace)
library(ggtext)
library(systemfonts)

# Data -----------------------------------------------------------

rovaniemi <- getbb("Rovaniemi, Finland")

all_streets <- opq(rovaniemi) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

railways <- opq(rovaniemi) |>
  add_osm_feature("railway") |>
  osmdata_sf()

leisures <- opq(rovaniemi) |>
  add_osm_feature("leisure") |>
  osmdata_sf()

landuse <- opq(rovaniemi) |>
  add_osm_feature("landuse") |>
  osmdata_sf()

natural <- opq(rovaniemi) |>
  add_osm_feature("natural") |>
  osmdata_sf()

water <- natural$osm_multipolygons |>
  filter(natural == "water")

buildings <- opq(rovaniemi) |>
  add_osm_feature("building") |>
  osmdata_sf()

primary_streets <- all_streets$osm_lines |>
  filter(
    highway %in%
      c(
        "motorway",
        "motorway_link",
        "primary",
        "primary_link",
        "secondary",
        "secondary_link",
        "tertiary",
        "tertiary_link",
        "residential",
        "pedestrian",
        "trunk",
        "trunk_link",
        "living_street"
      )
  )

rovaniemi_buildings <- buildings$osm_polygons

rovaniemi_leisures <- leisures$osm_polygons

green_places <- rovaniemi_leisures |>
  filter(
    leisure %in%
      c("park", "pitch", "nature reserve") |
      str_detect(leisure, "golf")
  )
meadow <- landuse$osm_polygons |>
  filter(landuse == "meadow")

forest <- landuse$osm_polygons |>
  filter(landuse == "forest")

wood <- natural$osm_polygons |>
  filter(natural == "wood")

rail <- railways$osm_lines |>
  filter(railway == "rail")

register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

chart <- glue::glue(
  "Data: OpenStreetMap<br>#30DayMapChallenge 2025 Day 14"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")
author <- glue::glue("Visualization: {bsky} @rajodm")
caption_text <- glue::glue(
  "{chart} | {author} | #rstats"
)

title <- glue::glue(
  "<span style='font-size: 16pt;'>**ROVANIEMI**</span><br>",
  "<span style='font-size: 10pt;'>66.503°N, 25.726°E</span><br>",
  "<span style='font-size: 10pt;'>Finland</span><br>",
  "<span style='font-size: 8pt;'>{caption_text}</span>"
)

color_dark <- "#15151c"
color_white <- "#f5f4ed"
color_blue <- "#c6d6d7"
color_green <- "#dae5d7"

xmin <- 25.62
xmax <- 25.85
ymin <- 66.47
ymax <- 66.545

# Plot -----------------------------------------------------------

theme_set(theme_void(paper = color_white, ink = color_dark))

map <- ggplot() +
  geom_sf(
    data = green_places,
    color = color_green,
    fill = color_green
  ) +
  geom_sf(
    data = water,
    fill = color_blue,
    col = color_blue,
  ) +
  geom_sf(
    data = meadow,
    fill = color_white,
    color = color_white
  ) +
  geom_sf(
    data = forest,
    color = color_green,
    fill = color_green
  ) +
  geom_sf(
    data = wood,
    color = color_green,
    fill = color_green
  ) +
  geom_sf(
    data = rovaniemi_buildings,
    color = lighten(color_dark, .5),
    fill = lighten(color_dark, .5)
  ) +
  geom_sf(
    data = rail,
    linetype = '1234',
    linewidth = .2,
    color = lighten(color_dark, .4),
    fill = lighten(color_dark, .4)
  ) +
  geom_sf(
    data = rovaniemi_leisures |>
      filter(!is.na(building)),
    color = lighten(color_dark, .5),
    fill = lighten(color_dark, .5)
  ) +
  geom_sf(
    data = primary_streets,
    aes(linewidth = highway, color = highway),
    show.legend = FALSE
  ) +
  scale_linewidth_manual(
    values = c(
      motorway = 1.6,
      motorway_link = 1.4,
      trunk = 1.4,
      trunk_link = 1.2,
      primary = 1,
      primary_link = .8,
      secondary = .6,
      secondary_link = .4,
      tertiary = .3,
      tertiary_link = .3,
      residential = .3,
      pedestrian = .3,
      living_street = .3
    )
  ) +
  scale_color_manual(
    values = c(
      c(
        motorway = color_dark,
        motorway_link = color_dark,
        trunk = color_dark,
        trunk_link = color_dark,
        primary = color_dark,
        primary_link = color_dark,
        secondary = lighten(color_dark, .2),
        secondary_link = lighten(color_dark, .2),
        tertiary = lighten(color_dark, .2),
        tertiary_link = lighten(color_dark, .2),
        residential = lighten(color_dark, .2),
        pedestrian = lighten(color_dark, .2),
        living_street = lighten(color_dark, .2)
      )
    )
  ) +
  annotate(
    "richtext",
    label = title,
    x = 25.848,
    y = 66.4705,
    fill = "transparent",
    color = "transparent",
    label.padding = unit(rep(0, 4), "lines"),
    label.r = unit(0, "lines"),
    family = "Atkinson Hyperlegible Next",
    lineheight = .04,
    text.color = color_dark,
    hjust = 1,
    vjust = 0
  ) +
  coord_sf(
    crs = 4326,
    expand = FALSE,
    clip = "off",
    xlim = c(xmin, xmax),
    ylim = c(ymax, ymin)
  )

ggh4x::save_plot(
  here::here("2025/maps/day14_rovaniemi_osm.png"),
  plot = map,
  width = 9.8,
  height = 8,
  units = "in",
  dpi = 420
)
