# Packages ---------------------------------------------------------------

library(rvest)
library(tidyverse)
library(sf)
library(rnaturalearth)

# Data -------------------------------------------------------------------

world <- rnaturalearth::ne_countries(10)
latvia <- rnaturalearth::ne_countries(scale = 10, country = "Latvia")
buff <- 4e4

wd_proj <-
  world |>
  sf::st_transform(crs = 3035)

ltv_proj <-
  latvia |>
  sf::st_transform(crs = 3035)

bb <- sf::st_bbox(ltv_proj)

frame <-
  sf::st_bbox(
    c(
      xmin = bb[["xmin"]] - buff,
      xmax = bb[["xmax"]] + buff,
      ymin = bb[["ymin"]] - buff,
      ymax = bb[["ymax"]] + buff
    )
  )

url <- "https://en.wikipedia.org/wiki/List_of_reportedly_haunted_locations"

page_html <- rvest::read_html(url)

haunted_places <-
  page_html |>
  rvest::html_elements("section[aria-labelledby='Latvia']") |>
  rvest::html_elements("li") |>
  rvest::html_text2() |>
  tibble::tibble(i = _) |>
  tidyr::separate_wider_delim(
    dplyr::everything(),
    delim = ": ",
    names = c("place", "desc")
  )

haunted_places_geo <-
  haunted_places |>
  tidygeocoder::geocode(place, method = "osm")

p_dat <-
  haunted_places_geo |>
  tidyr::drop_na() |>
  dplyr::mutate(
    desc = stringr::str_remove_all(desc, "\\[.*\\]") |>
      stringr::str_remove_all(
        "A manor with|A castle (with )?|A neoclassical manor"
      ) |>
      stringr::str_trim() |>
      stringr::str_to_sentence() |>
      stringr::str_wrap(24),
    labs = glue::glue("{place}\n{desc}")
  ) |>
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  sf::st_transform(crs = 3035)

p_dat_grid <-
  p_dat |>
  dplyr::mutate(
    x = sf::st_coordinates(geometry)[, "X"],
    y = sf::st_coordinates(geometry)[, "Y"]
  ) |>
  sf::st_drop_geometry()

# Misc -------------------------------------------------------------------

# Colors
# Palette: Alex Cristache #MindfulPalette No. 20
col_ink <- "#e4e3dc"
col_paper <- "#1f282e"
col_gray <- "#414e58"
col_red <- "#b83312"
col_orange <- "#ff4e20"

# {marquee} style
stl <-
  marquee::classic_style() |>
  marquee::modify_style("strong", weight = "semibold") |>
  marquee::modify_style("hg", color = col_orange)

# Map --------------------------------------------------------------------

map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = wd_proj,
    fill = colorspace::lighten(col_gray, 0.31),
    color = col_ink
  ) +
  ggplot2::geom_sf(data = ltv_proj, fill = col_gray, color = col_ink) +
  ggshadow::geom_glowpoint(
    data = p_dat,
    size = 2,
    ggplot2::aes(geometry = geometry),
    stat = "sf_coordinates",
    color = col_orange,
    shadowsize = 0.35,
    shadowcolor = col_red,
    shadowalpha = 0.6
  ) +
  ggplot2::geom_sf_text(
    data = wd_proj,
    ggplot2::aes(label = name),
    size = 6.34,
    family = "Schibsted Grotesk",
    alpha = 0.6
  ) +
  ggforce::geom_mark_circle(
    data = p_dat_grid,
    ggplot2::aes(x, y, group = place, label = place, description = desc),
    position = ggplot2::position_nudge(y = 0),
    label.buffer = unit(18, "mm"),
    label.family = c("Schibsted Grotesk", "Atkinson Hyperlegible Next"),
    label.fontsize = 11,
    label.fill = col_ink,
    label.colour = c(col_red, col_paper),
    con.colour = col_ink,
    con.type = "straight",
    radius = 0,
    expand = 0,
    con.cap = unit(2, "mm"),
    alpha = 0.6
  ) +
  ggplot2::labs(
    title = "**Reportedly {.hg Haunted Locations} in Latvia**",
    caption = "**Source**: Wikipedia - List of reportedly haunted locations, Latvia\\
    **Base map**: Natural Hearth via {rnaturalhearth} R package | **Projection**: EPSG3035\\
    **#30DayMapChallenge 2026**: Day 1 - points | **Visualisation**: Andriambelo Rajo | #rstats"
  ) +
  ggplot2::coord_sf(
    xlim = c(frame$xmin, frame$xmax),
    ylim = c(frame$ymin, frame$ymax),
    expand = FALSE
  ) +
  ggplot2::theme_void(
    base_family = "Atkinson Hyperlegible Next",
    ink = col_ink,
    paper = col_paper
  ) +
  ggplot2::theme(geom = ggplot2::element_geom(linewidth = 0.65)) +
  ggplot2::theme_sub_plot(
    title.position = "plot",
    title = marquee::element_marquee(
      family = "Schibsted Grotesk",
      size = ggplot2::rel(1.8),
      style = stl,
      margin = ggplot2::margin(0, 0, 8, 0)
    ),
    caption.position = "plot",
    caption = marquee::element_marquee(
      hjust = 1,
      style = marquee::classic_style(align = "right"),
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(8, 0, 0, 0)
    ),
    margin = ggplot2::margin(15, 15, 5, 15)
  )

ggh4x::save_plot(
  "dat01_points_haunted_places.png",
  plot = map,
  width = 25,
  height = 21,
  units = "cm",
  dpi = 600
)
