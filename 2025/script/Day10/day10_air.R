# Libraries ------------------------------------------------------

# The data can be accessed using the {ecmwfr} package
# but I'v pre-downloaded it.

library(dplyr)
library(tidyr)
library(ggplot2)
library(metR)
library(terra)
library(sf)
library(rnaturalearth)
library(classInt)
library(ggtext)
library(systemfonts)

# Load Data ------------------------------------------------------

wind_data <- terra::rast(here::here(
  "tana",
  "data_stream-oper_stepType-instant.nc"
)) |>
  terra::project("EPSG:4326")


wind_data
world <- ne_countries(50) |>
  st_transform(crs = "EPSG:4326")

wind_data_table <- wind_data |>
  data.table::as.data.table(xy = TRUE, na.rm = TRUE) |>
  janitor::clean_names()


# Caption --------------------------------------------------------

register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

src <- glue::glue(
  "Data: Copernicus Climate Change Service, ECMWF - ERA5 hourly data on single levels from 1940 to present."
)

chart <- glue::glue(
  "#30DayMapChallenge 2025 Day 10 - Air"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{src}<br>{chart} | {author} | #rstats")

# Helper function -----------------------------------------------

summarize_wind_comp <- function(wind_direction = c("u", "v")) {
  wind_data_table |>
    select(lon = x, lat = y, starts_with(wind_direction)) |>
    pivot_longer(
      starts_with(wind_direction),
      names_to = "time",
      values_to = paste0(wind_direction, "10")
    ) |>
    summarize(
      .by = c(lon, lat),
      # Take the selected wind direction as variable name
      # & use the mean of the newly created variable (u10 or v10) as value
      "{wind_direction}" := mean(.data[[paste0(wind_direction, "10")]])
    )
}

u_comp <- summarize_wind_comp("u")
v_comp <- summarize_wind_comp("v")


wind_stream <-
  u_comp |>
  inner_join(
    v_comp,
    by = c("lon", "lat"),
    relationship = "many-to-many"
  )

# Calculate wind speed based on u and v

wind_speed <-
  wind_stream |>
  mutate(
    speed = sqrt(u**2 + v**2)
  )

wind_speed

breaks <- classInt::classIntervals(
  wind_speed$speed,
  n = 6,
  style = "equal"
)

# Plot -----------------------------------------------------------

map <- wind_speed |>
  ggplot() +
  geom_sf(
    data = world,
    fill = NA,
    color = "#d9d9d9",
    linewidth = .3,
  ) +
  geom_streamline(
    aes(
      x = lon,
      y = lat,
      dx = u,
      dy = v,
      color = after_stat(dx**2 + dy**2),
      alpha = after_stat(dx**2 + dy**2),
      linewidth = after_stat(dx**2 + dy**2)
    ),
    lineend = "round",
    L = 2,
    res = 14,
    n = 90,
    arrow = NULL,
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    colors = cols4all::c4a(
      "ocean.tempo",
      range = c(.1, .8),
      n = 12
    ),
    breaks = round(breaks$brks, digits = 0),
  ) +
  scale_alpha(
    range = c(.2, 1),
  ) +
  scale_linewidth(
    range = c(.2, .6),
  ) +
  coord_sf(
    crs = 4326,
    expand = FALSE,
    xlim = c(-180, 180),
    ylim = c(-90, 90),
    clip = "off"
  ) +
  labs(
    caption = caption_text
  ) +
  theme_void(base_family = "Outfit", ink = "#2d2d2d") +
  theme_sub_panel(background = element_rect(fill = "white", color = NA)) +
  theme_sub_plot(
    background = element_rect(fill = "white", color = NA),
    caption.position = "plot",
    caption = element_textbox_simple(
      halign = 1,
      hjust = 1,
      size = 7,
      family = "Outfit",
      margin = margin(t = 2, r = 5),
      lineheight = 1,
    )
  ) +
  guides(
    alpha = "none",
    linewidth = "none",
    color = guide_colourbar(
      title = "<span style='font-size: 10pt;'>Date: 2025-11-05</span><br>**Average Wind Speed (m/s)**",
      position = "inside",
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0,
      label.hjust = .5,
      theme = theme(
        legend.ticks = element_blank(),
        legend.title = element_textbox_simple(
          size = 11,
          family = "Outfit",
          margin = margin(b = 5),
        ),
        legend.text = element_text(size = 8, family = "Outfit"),
        legend.key.height = unit(.42, "lines"),
        legend.key.width = unit(5.6, "cm"),
        legend.position.inside = c(.11, -.085),
      )
    )
  ) +
  guides(
    alpha = "none",
    linewidth = "none",
    color = guide_colourbar(
      title = "<span style='font-size: 10pt;'>Date: 2025-11-05</span><br>**Average Wind Speed (m/s)**",
      position = "inside",
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0,
      label.hjust = .5,
      theme = theme(
        legend.ticks = element_blank(),
        legend.title = element_textbox_simple(
          size = 11,
          family = "Outfit",
          margin = margin(b = 5),
        ),
        legend.text = element_text(size = 8, family = "Outfit"),
        legend.key.height = unit(.42, "lines"),
        legend.key.width = unit(5.6, "cm"),
        legend.position.inside = c(.11, -.085),
      )
    )
  )


ggh4x::save_plot(
  here::here("2025/maps", "day10_air_wind-speed.png"),
  plot = map,
  width = 29.7,
  height = 21,
  units = "cm",
  dpi = 300
)
