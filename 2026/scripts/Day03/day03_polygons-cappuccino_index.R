# Packages ---------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)
library(ggrefine)
library(giscoR)

source("cap_func.R")

# Data -------------------------------------------------------------------

world <- giscoR::gisco_countries_2024

world_proj <- world |>
  sf::st_transform(crs = 3035)

tt <- tidytuesdayR::tt_load(2026, 36)
index <- tt$cappuccino_index

eu_index <-
  index |>
  dplyr::mutate(
    name_iso = countrycode::countryname(country, "iso3c"),
    continent = countrycode::countryname(country, "eu27", warn = FALSE)
  ) |>
  dplyr::filter_out(is.na(continent)) |>
  dplyr::mutate(
    name_eng = countrycode::countryname(country, "un.name.en")
  )

eu_index_sf <-
  eu_index |>
  dplyr::left_join(world, dplyr::join_by(name_iso == ISO3_CODE)) |>
  dplyr::select(name_eng, name_iso, index, geometry) |>
  dplyr::mutate(label = glue::glue("{name_iso} {round(index, 1)}")) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 3035)

bbox <- c(
  xmin = -10,
  xmax = 35,
  ymin = 34,
  ymax = 71
) |>
  sf::st_bbox(crs = 4326) |>
  sf::st_transform(crs = 3035)


# Miscs ------------------------------------------------------------------

# Colors
palettes <- list(
  paper = "#fafaf8",
  ink = "#1a1a1a",
  color_sub = "#4a4a4a",
  color_muted = "#8a8a8a",
  color_grid = "#e8e8e4",
  color_accent = "#e8f0f5"
)

# Font
systemfonts::require_font("Atkinson Hyperlegible Next")
systemfonts::require_font("Schibsted Grotesk")
systemfonts::register_variant(
  "ahn_thin",
  "Atkinson Hyperlegible Next",
  weight = "thin"
)

# Texts
title_text <- "**How long a barista would need to work to pay for a small cappuchino ?**"
subtitle_text <- "The **cappuccino index** indicates the **time in minutes a barista needs to work** to afford a small cappuccino in their country: **the darker the color, the longer the time**."
caption_text <- create_caption(
  source = "The 2026 Cappuccino Index, via **TidyTuesday** 2026 Week 36",
  base_map = "GISCO countries 2024, via {giscoR} R package",
  note = "Only member states of the European Union (as of February 2020), without special territories are included.",
  day = 3,
  prompt = "Polygons",
  proj = "EPSG:3035"
) |>
  stringr::str_replace_all("<br>", "\\\\\n")

# Map --------------------------------------------------------------------

map <-
  eu_index_sf |>
  ggrefine::ggrefine() +
  ggplot2::aes(fill = index, label = label) +
  ggplot2::geom_sf(
    data = world_proj,
    fill = palettes$color_muted,
    color = colorspace::adjust_transparency(palettes$color_muted, alpha = 0.4),
    alpha = 0.05,
    linewidth = 0.15,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_sf(
    data = eu_index_sf,
    color = colorspace::adjust_transparency(palettes$ink, alpha = 0.8),
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  ggplot2::geom_sf_label(
    ggrefine::aes_colour_contrast(
      dark = palettes$ink,
      light = palettes$paper
    ),
    family = "ahn_thin",
    size = 3,
    show.legend = FALSE
  ) +
  cols4all::scale_fill_continuous_c4a_seq("ocean.turbid") +
  ggplot2::coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  ggplot2::theme_void(
    base_family = "Atkinson Hyperlegible Next",
    ink = palettes$ink,
    paper = palettes$paper
  ) +
  ggplot2::theme_sub_plot(
    title.position = "plot",
    title = marquee::element_marquee(
      family = "Schibsted Grotesk",
      size = ggplot2::rel(1.8),
      width = 0.98,
      margin = ggplot2::margin(b = 4)
    ),
    subtitle = marquee::element_marquee(
      size = ggplot2::rel(1.15),
      width = 0.95,
      margin = ggplot2::margin(t = 0, b = 12)
    ),
    caption.position = "plot",
    caption = marquee::element_marquee(
      size = ggplot2::rel(0.75),
      width = 0.98,
      hjust = 0,
      margin = ggplot2::margin(t = 8)
    ),
    margin = ggplot2::margin(25, 25, 10, 25)
  )

ggh4x::save_plot(
  here::here("2026/maps", "day03_polygons-cappucino_index.png"),
  plot = map,
  width = 21,
  height = 25,
  units = "cm",
  dpi = 600
)
