# Packages ---------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)


# Projection -------------------------------------------------------------

proj <- "+proj=laea +lat_0=40 +lon_0=15"

# Data -------------------------------------------------------------------

world <-
  ne_coastline(10) |>
  st_transform(crs = proj)

# gpkg file available at: https://zenodo.org/records/17122148/files/itinere_roads.gpkg?download=1
roads <- st_read(here::here("2026/data/itiner-e/itinere_roads.gpkg"))
roads_proj <- roads |>
  mutate(
    Segment_s = forcats::fct_relevel(
      Segment_s,
      c("Certain", "Conjectured", "Hypothetical")
    )
  ) |>
  st_transform(crs = proj)

bb <- st_bbox(roads_proj)

# Misc -------------------------------------------------------------------

# Colors
col_paper <- "#0a0d0f"
col_gray <- "#0a2538"
col_red <- "#e84a2e"
col_ink <- "#f6f4f1"

# Fonts
systemfonts::require_font("Atkinson Hyperlegible Next")
systemfonts::require_font("Schibsted Grotesk")
systemfonts::register_variant(
  "SG semibold",
  "Schibsted Grotesk",
  weight = "semibold"
)

# {marquee} style
stl <-
  marquee::classic_style() |>
  marquee::modify_style(
    "strong",
    weight = "semibold"
  ) |>
  marquee::modify_style("hg", color = col_red)

credit <- "de Soto, P. et al. (2025) “A High-Resolution Dataset of Roads of the Roman Empire: Itiner-e static version 2024”. Zenodo. Available at: https://doi.org/10.5281/zenodo.17122148."

# Map --------------------------------------------------------------------

map <- ggplot() +
  geom_sf(
    data = world,
    color = col_gray,
    linewidth = 0.1
  ) +
  geom_sf(
    data = roads_proj,
    aes(linetype = Segment_s, alpha = Segment_s),
    linewidth = 0.3,
    color = col_red
  ) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_linetype_manual(
    values = c(
      "Certain" = "solid",
      "Conjectured" = "31",
      "Hypothetical" = "dotted"
    )
  ) +
  scale_alpha_manual(values = c(0.9, 0.6, 0.4)) +
  labs(
    title = "**{.hg Roads} of the Roman Empire**",
    caption = glue::glue(
      "**Source**: {credit}\\
      **Base map**: Natural Earth via {{rnaturalearth}} R package | **Projection**: Lambert azimuthal equal-area (40°N, 15°E)\\
      **30DayMapChallenge 2026**: Day 2 - Lines | **Visualization**: Andriambelo Rajo | #rstats",
      .trim = FALSE
    )
  ) +
  guides(
    linetype = guide_legend(
      title = "Segment certainty",
      override.aes = list(linewidth = 0.5)
    ),
    alpha = guide_legend(title = "Segment certainty")
  ) +
  theme_void(
    base_family = "Atkinson Hyperlegible Next",
    base_size = 12,
    paper = col_paper,
    ink = col_ink
  ) +
  theme_sub_plot(
    title.position = "plot",
    title = marquee::element_marquee(
      family = "SG semibold",
      size = rel(1.8),
      style = stl,
      margin = margin(0, 0, 16, 0)
    ),
    caption.position = "plot",
    caption = marquee::element_marquee(
      size = rel(0.7),
      width = 1,
      hjust = 0,
      style = stl,
      margin = margin(4, 0, 0, 0)
    ),
    margin = margin(25, 25, 10, 25)
  ) +
  theme_sub_legend(
    title = element_text("SG semibold", size = rel(1.05)),
    text = element_text(size = rel(1)),
    position = "inside",
    position.inside = c(0.9, 0.9)
  )

ggh4x::save_plot(
  here::here("2026/maps", "day02_lines-roman_empire_roads.png"),
  plot = map,
  width = 21,
  height = 21,
  units = "cm",
  dpi = 600
)
