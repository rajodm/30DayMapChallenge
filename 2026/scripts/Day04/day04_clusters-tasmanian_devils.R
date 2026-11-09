# Packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(osmdata)

source("cap_func.R")

# Data -------------------------------------------------------------------

roadkill <- st_read(here::here(
  "2026/data/NVA_TASMANIAN_ROADKILL_STATEWIDE/NVA_Tasmanian_Roadkill_statewide.shp"
)) |>
  janitor::clean_names()

tdevil_roadkill <-
  roadkill |>
  mutate(species_clean = stringr::word(species_na, 1, 2)) |>
  filter(
    observat_1 == "2024",
    stringr::str_detect(species_clean, "Sarcophilus harrisii")
  )

td_dat <-
  tdevil_roadkill |>
  mutate(
    x = st_coordinates(geometry)[, "X"],
    y = st_coordinates(geometry)[, "Y"],
    clust = dbscan::dbscan(data.frame(x, y), eps = 3000, minPts = 4)[[
      "cluster"
    ]],
    clust_bin = if_else(clust == 0, "noise", "not_noise"),
    clust_ord = case_when(
      clust %in% c(1, 2) ~ "Major",
      clust != 0 ~ "Minor",
      TRUE ~ "Other"
    )
  )

bb <- st_bbox(tdevil_roadkill)
buff <- 1e4
bb_buff <- bb + c(-buff, -buff, buff, buff)
bb_osm <- bb_buff |>
  st_as_sfc() |>
  st_transform(4326) |>
  st_bbox()

tsa_coastlines <- rnaturalearth::ne_countries("large", country = "Australia") |>
  st_crop(bb_osm) |>
  st_transform(st_crs(tdevil_roadkill))

coast <- st_crop(tsa_coastlines, bb_buff)

roads_rw <-
  opq(bb_osm, timeout = 300) |>
  add_osm_feature(
    key = "highway",
    value = c("motorway", "trunk", "primary", "secondary", "tertiary")
  ) |>
  osmdata_sf()

roads <-
  roads_rw$osm_lines |>
  select(name, highway) |>
  st_transform(crs = st_crs(tdevil_roadkill))

major_clusters <-
  td_dat |>
  filter(clust %in% 1:2) |>
  st_drop_geometry() |>
  summarize(
    .by = clust,
    n = n()
  ) |>
  mutate(labs = glue::glue("{n} carcasses"))

td_dat_summary <- td_dat |>
  left_join(major_clusters) |>
  filter(clust_ord == "Major")

# Misc -------------------------------------------------------------------

# Colors
col_ink <- "#f8f4ed"
col_paper <- "#060609"
col_blue <- "#0d0e15"
col_lighter_blue <- "#3a3f52"
col_orange <- "#eb7b07"
col_white <- colorspace::adjust_transparency("#e0d6c8", 0.5)

# Texts
title_text <- "**Where endangered Tasmanian devils die on the road?**"
subtitle_text <- "Each `x` is one reported carcass in 2024. {.hg **Orange** marks **carcasses in a cluster**}, a group of at least 4 within 3 km, and **two stretches of road hold 63 of the 313**."
caption_text <-
  create_caption(
    "NVA Tasmanian Roadkill from www.theLIST.tas.gov.au ©State of Tasmania | Roads: © OpenStreetMap Collaborators (ODbL)",
    day = 4,
    "Clusters",
    "EPSG:28355",
    base_map = "Natural Earth via the {rnaturalearth} R package"
  ) |>
  stringr::str_replace_all("<br>", "\\\\\n")

# Marquee style
stl <-
  marquee::classic_style() |>
  marquee::modify_style(
    "strong",
    weight = "semibold"
  ) |>
  marquee::modify_style("hg", color = col_orange)

# Fonts
systemfonts::require_font("Newsreader 24pt")

# Map --------------------------------------------------------------------

map <-
  ggplot() +
  geom_sf(
    data = tsa_coastlines,
    color = col_lighter_blue,
    fill = col_blue,
    alpha = 0.6,
    linewidth = 0.15
  ) +
  geom_sf(
    data = roads,
    aes(linewidth = highway),
    color = col_lighter_blue,
    show.legend = FALSE
  ) +
  geom_sf(
    data = td_dat |> filter(clust_ord == "Other"),
    color = col_white,
    stroke = 0.8,
    size = 1.3,
    shape = 4
  ) +
  geom_sf(
    data = td_dat |> filter(clust_ord == "Major"),
    color = col_orange,
    stroke = 0.8,
    size = 2.2,
    shape = 4
  ) +
  geom_sf(
    data = td_dat |> filter(clust_ord == "Minor"),
    color = colorspace::adjust_transparency(col_orange, 0.6),
    stroke = 0.8,
    size = 1.5,
    shape = 4
  ) +
  ggforce::geom_mark_hull(
    data = td_dat_summary,
    aes(group = clust, label = labs, geometry = geometry),
    expand = unit(2.6, "mm"),
    alpha = 0.12,
    linewidth = 0.4,
    label.fill = col_ink,
    con.type = "straight",
    con.colour = col_ink,
    label.colour = col_paper,
    label.buffer = unit(20, "mm"),
    label.family = "Atkinson Hyperlegible Next",
    label.fontsize = 9,
    stat = "sf_coordinates",
    con.cap = 0
  ) +
  scale_linewidth_manual(
    values = c(
      motorway = 0.6,
      trunk = 0.6,
      primary = 0.45,
      secondary = 0.3,
      tertiary = 0.15
    ) *
      1.2
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_void(
    base_family = "Atkinson Hyperlegible Next",
    ink = col_ink,
    paper = col_paper
  ) +
  theme_sub_plot(
    title.position = "plot",
    title = marquee::element_marquee(
      "Newsreader 24pt",
      size = rel(2),
      width = 1,
      margin = margin(b = 2),
      style = stl
    ),
    subtitle = marquee::element_marquee(
      width = 0.96,
      size = rel(1.15),
      lineheight = 1.2,
      style = stl
    ),
    caption.position = "plot",
    caption = marquee::element_marquee(
      width = 0.95,
      hjust = 0,
      color = colorspace::lighten(col_blue, 0.6),
      size = rel(0.75)
    ),
    margin = margin(25, 25, 10, 25)
  )

ggh4x::save_plot(
  here::here("2026/maps", "day04_clusters-tasmanian_devils.png"),
  plot = map,
  width = 21,
  height = 25,
  units = "cm",
  dpi = 300
)
