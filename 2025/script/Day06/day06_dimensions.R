# Heavily inspired from Milos Agathon tutorial on Tanaka elevation maps
# Code here: https://github.com/milos-agathon/tanaka-elevation-maps
# Packages -------------------------------------------------------

library(tidyverse)
library(terra)
library(rnaturalearth)
library(metR)
library(colorspace)
library(sf)
library(ggtext)
library(magick)
library(rayshader)

# Colors ---------------------------------------------------------

color_black <- "#344032"
color_cap <- "#a89f96"
color_title <- "#1a4d0d"
color_white <- "#fcfaf8"

pal <- c(
  "#008435",
  "#2DC016",
  "#BFE555",
  "#F5D75B",
  "#D59E3C",
  "#AD7C4C",
  "#FFFFFF"
)

light_col <- lighten(pal[3], amount = .1)
dark_col <- darken(pal[7], amount = .25)

gbr_bounds <- ne_states(country = "United Kingdom", returnclass = "sf")


# Download Scotland boundaries -----------------------------------

sct_bounds <- gbr_bounds |>
  filter(gu_a3 == "SCT")

# Get Scotland elevation
dem_rast <- elevatr::get_elev_raster(
  sct_bounds,
  z = 7,
  clip = "location"
)

# Projection
proj <- "EPSG:27700"

# Transform to a Data Frame
sct_df <- as.data.frame(sct_rast_proj, xy = TRUE)

colnames(sct_df)[3] <- "elevation"

# Convert to a tibble (optional?)
sct_tibble <-
  sct_df |>
  as_tibble()

# Texts ----------------------------------------------------------

chart <- "#30DayMapChallenge 2025 Day 06 - Dimensions"

caption_text <- glue::glue(
  "{chart} | @rajodm.bsky.social | #rstats | #rayshader"
)

# Limits ---------------------------------------------------------

lim <- range(sct_tibble$elevation, na.rm = TRUE)
# breaks for the color scale
breaks <- seq(
  floor(lim[1] / 50) * 50,
  # Add 500 because some part was left out
  ceiling((lim[2] + 500) / 50) * 50,
  by = 160
)

# Theme ----------------------------------------------------------
# Set themes to element blank
# Trick from @jdonland.bsky.social
# https://bsky.app/profile/jdonland.bsky.social/post/3lrto2da6tk24

blank_elements <- function(theme_elements) {
  rep(alist(element_blank()), length(theme_elements)) |>
    set_names(theme_elements) |>
    do.call(theme, args = _)
}

# Theme elements to remove
el_to_rmv <- c("axis.line", "panel.grid", "axis.title", "axis.text")

# Custom theme for the chart
custom_theme <- function(unwanted_elements) {
  theme_minimal(base_size = 10, base_family = "Lato") +
    theme(
      text = element_text(color = color_black),
      plot.background = element_rect(fill = color_white, color = NA),
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        size = 22,
        family = "Cinzel",
        face = "bold",
        color = color_black,
        margin = margin(t = 40, b = 0, l = 30)
      ),
      plot.caption.position = "plot",
      plot.caption = element_textbox_simple(
        size = 6,
        margin = margin(10, 0, 5, 0),
        halign = .5,
        hjust = .5,
        # lineheight = 1,
        color = color_black
      ),
      plot.margin = margin(.1, .1, .1, .1),
    ) +
    blank_elements(unwanted_elements)
}


# 2D Map ------------------------------------------------------------

map <- sct_tibble |>
  ggplot(aes(x = x, y = y, z = elevation)) +
  geom_contour_fill(
    name = "Elevation",
    breaks = breaks
  ) +
  geom_contour_tanaka(
    breaks = breaks,
    sun.angle = 45,
    light = light_col,
    dark = dark_col,
    range = c(.01, .3),
    smooth = .8
  ) +
  scale_fill_gradientn(
    name = "Elevation (m)",
    colors = pal,
    limits = lim,
    guide = guide_colorbar(
      title.position = "top",
      ticks = FALSE,
      barheight = unit(5, "cm"),
      # barwidth = unit(1, "lines"),
      frame.colour = NA
    )
  ) +
  coord_sf(crs = proj) +
  # Add the custom theme
  custom_theme(el_to_rmv)

# Save the 2D chart
# ggsave(
#   "test.png",
#   map,
#   width = 7,
#   height = 7
# )

# 3D maps --------------------------------------------------------

plot_gg(
  map,
  width = 8,
  height = 8,
  shadow = FALSE,
  # shadow_intensity = 1,
  windowsize = c(800, 800),
  zoom = .72,
  phi = 72,
  theta = 0,
  background = color_white,
  multicore = TRUE
)

# HDR rendering
url <- "https://dl.polyhaven.org/fil/ph-assets/HDRIs/hdr/4k/venice_sunrise_4k.hdr"
hdri_file <- basename(url)
download.file(
  url = url,
  destfile = hdri_file,
  mode = "wb"
)

Sys.sleep(0.2)

# This takes some times to render
render_highquality(
  'scotland.png',
  sample = 320,
  sample_method = "sobol",
  interactive = FALSE,
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity = 1.28,
  rotate_env = 90,
  parallel = TRUE,
  width = 1800,
  height = 1800,
)

# Final touch ----------------------------------------------------

scotland_png <- image_read("scotland.png")

# Remove some unwanted borders
png_cropped <- image_crop(
  scotland_png,
  "1580x1680+110+80"
)

png_tagged <- image_annotate(
  png_cropped,
  "SCOTLAND",
  size = 83,
  font = "Cinzel",
  color = color_title,
  weight = 700,
  location = "+180+240",
) |>
  image_annotate(
    caption_text,
    size = 22,
    font = "Lato",
    color = color_cap,
    gravity = "southeast",
    location = "+15+20"
  )

image_write(
  png_tagged,
  here::here("2025/maps", "day06-dimensions-scotland.png"),
  quality = 100
)

# fs::file_delete("scotland.png")
# fs::file_delete(hdri_file)
