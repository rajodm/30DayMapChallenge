# Packages ---------------------------------------------------------------

library(dplyr)
library(osmdata)
library(sf)

# Data -------------------------------------------------------------------

# CRS & bounding box
crs <- "EPSG:26919"
bb <- c(xmin = -68.785, ymin = 44.793, xmax = -68.757, ymax = 44.808) |>
  st_bbox(crs = 4326)

# River
water <- opq(bb, timeout = 600) |>
  add_osm_feature("natural", "water") |>
  osmdata_sf()

river <- water$osm_multipolygons |>
  st_union() |>
  st_transform(crs = "EPSG:26919")

# Buildings
buildings <- opq(bb, timeout = 600) |>
  add_osm_feature("building") |>
  osmdata_sf()

building_p <-
  buildings$osm_polygons |>
  st_transform(crs = crs)

# Roads
minor_paths <- c("footway", "path", "steps", "track")

streets <- opq(bb, timeout = 600) |>
  add_osm_feature("highway") |>
  osmdata_sf()

p_streets <-
  streets$osm_lines |>
  st_transform(crs = "EPSG:26919")

# Small streets
p_streets_min <-
  p_streets |>
  filter(highway %in% minor_paths)

# Major streets
p_streets_main <-
  p_streets |>
  filter_out(highway %in% minor_paths)

bb_proj <-
  bb |>
  st_as_sfc() |>
  st_transform(crs = "EPSG:26919") |>
  st_bbox()

# Miscs ------------------------------------------------------------------

systemfonts::register_variant(
  "nw_title",
  "Newsreader 36pt",
  "semibold"
)

col_paper <- "#faf8f2"
col_ink <- "#2b2723"
col_blue <- "#9dbcd4"
col_blue_borders <- "#7fa3bf"
col_red <- "#b5382a"
col_gray <- "#bdb6a6"
col_pink <- "#d9c9b8"
col_pink_borders <- "#a8927e"

# lwd
highway_w <- c(
  "primary" = 1.6,
  "primary_link" = 1.6,
  "secondary" = 1.4,
  "tertiary" = 1.2,
  "tertiary_link" = 1.2,
  "residential" = 1,
  "unclassified" = 1,
  "service" = 0.6,
  "cycleway" = 0.5
)

# Map --------------------------------------------------------------------

map <-
  tmap::tm_shape(river, bbox = bb) +
  tmap::tm_polygons(fill = col_blue, col = col_blue_borders, lwd = 0.5) +
  tmap::tm_shape(p_streets_main, bbox = bb) +
  tmap::tm_lines(
    lwd = "highway",
    lwd.scale = tmap::tm_scale(values = highway_w * 1.62),
    col = "#2b2723",
    lwd.legend = tmap::tm_legend_hide()
  ) +
  tmap::tm_shape(p_streets_main, bbox = bb) +
  tmap::tm_lines(
    lwd = "highway",
    lwd.scale = tmap::tm_scale(values = highway_w),
    col = col_gray,
    lwd.legend = tmap::tm_legend_hide()
  ) +
  tmap::tm_shape(p_streets_min, bbox = bb) +
  tmap::tm_lines(
    lwd = 0.4,
    lty = "42",
    col = col_gray,
    lwd.legend = tmap::tm_legend_hide()
  ) +
  tmap::tm_shape(
    p_streets |>
      filter(highway == "primary"),
    bbox = bb
  ) +
  tmap::tm_lines(
    lwd = 1.6,
    col = col_red
  ) +
  tmap::tm_shape(building_p, bbox = bb) +
  tmap::tm_polygons(
    fill = col_pink,
    col = col_pink_borders,
    lwd = 0.35
  ) +
  tmap::tm_compass(
    type = "8star",
    text.size = 0.7,
    size = 1.6,
    color.dark = "#2b2723",
    color.light = "#faf8f2",
    text.color = "#2b2723",
    position = c("left", "top")
  ) +
  tmap::tm_scalebar(
    c(0, 100, 200),
    text.size = 0.22,
    color.dark = "#2b2723",
    color.light = "#faf8f2",
    text.color = "#2b2723",
    position = c("right", "bottom")
  ) +
  tmap::tm_title(
    "BANGOR",
    size = 1.2,
    just = 0.5,
    fontfamily = "nw_title",
    position = tmap::tm_pos_in("center", "bottom", "center"),
    color = col_ink,
    padding = c(0.4, 0, 0, 0)
  ) +
  tmap::tm_title(
    "MAINE",
    size = 0.6,
    fontfamily = "Newsreader 24pt",
    just = TRUE,
    position = tmap::tm_pos_in("center", "bottom", "center"),
    color = col_ink,
    padding = c(0.3, 0, 0, 0)
  ) +
  tmap::tm_credits(
    glue::glue(
      "Data: OpenStreetMap contributors, ODbL\n",
      "#30DayMapChallenge 2026: Day 6 - Vintage | Visualization: Andriambelo Rajo | #rstats"
    ),
    size = 0.3,
    color = col_ink,
    position = tmap::tm_pos_out("center", "bottom", "left", "top"),
    padding = c(0, 0, 0, 0)
  ) +
  tmap::tm_layout(
    text.fontfamily = "Newsreader 24pt",
    frame.lwd = 0.7,
    asp = 0,
    outer.margins = c(0, 0.02, 0.02, 0.02),
    inner.margins = rep(0.01, 4),
    frame.double_line = TRUE,
    frame.r = 0,
    bg.color = col_paper,
    outer.bg.color = col_paper,
    scale = 2.3
  )

asp <- tmaptools::get_asp_ratio(map)
w <- 25
h <- w / asp

tmap::tmap_save(
  map,
  here::here("2026/maps", "day06_vintage-bangor.png"),
  device = ragg::agg_png,
  w,
  h,
  "cm",
  600
)
