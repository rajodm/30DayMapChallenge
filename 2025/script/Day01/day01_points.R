library(tidyverse)
library(sf)
library(tmap)
library(duckdb)
library(duckplyr)
library(duckspatial)

font_text <- "Outfit"
color_white <- "#fef5ed"
color_white2 <- "#d4c5b9"
color_bg <- "#2d2424"
color_cap <- "#a89b91"

hf_levels <- c(
  "Maternal & Child Health",
  "Referral Hospital",
  "District Hospital",
  "Hospital (Other)",
  "Primary Care"
)

pal <- c(
  "Referral Hospital" = "#FF9D9A",
  "District Hospital" = "#9467BD",
  "Hospital (Other)" = "#AAAA00",
  "Maternal & Child Health" = "#E73F74",
  "Primary Care" = "#99DDFF"
)

size <- c(
  "Referral Hospital" = .18,
  "District Hospital" = .16,
  "Hospital (Other)" = .18,
  "Maternal & Child Health" = .2,
  "Primary Care" = .08
)

alpha <- c(
  "Referral Hospital" = 1,
  "District Hospital" = .8,
  "Hospital (Other)" = .85,
  "Maternal & Child Health" = 1,
  "Primary Care" = .4
)


drv <- duckdb(here::here("2025/data", "30DMC25.db"))
con <- dbConnect(drv)

# The `spatial` extension is required (error)
# but the table was added to the db anyway when I used `ddbs_write_vector()`
# and I was prompt to add the `overwrite = TRUE` arg
# dbExecute(con, "install spatial")
dbExecute(con, "load spatial")

# ddbs_write_vector(con, ssa_hf, "ssa_hf_22", overwrite = TRUE)

classify_facility <- function(facility_type, facility_name = NA) {
  facility_type <- str_to_lower(facility_type)
  facility_name <- str_to_lower(facility_name)

  # Combine type and name for better matching
  combined <- paste(facility_type, facility_name, sep = " ")

  case_when(
    str_detect(
      combined,
      "\\bpost[eo]?\\b|dispensa.?r|hut|community-based"
    ) ~ "Health Post/Dispensary",

    str_detect(
      combined,
      "health cent|centr[eo] de s[aú]|centro de saúde|centre m.d.+|clini|primary|medical cent.."
    ) ~ "Health Centre/Clinic",

    str_detect(
      combined,
      "matern|child|[ie]nfant.+|mch\\b|pmtct|antenatal|m.re"
    ) ~ "Maternal & Child Health",

    str_detect(
      combined,
      "sub-district|district|level 1|zonal|municipal|pr[eé]fect|distrital|rural hospital|community\\b"
    ) ~ "District Hospital",

    str_detect(
      combined,
      "national|university|teaching|tertiary|universitaire|chu\\b|referral|general|geral|referral|level [2-3]|tertiare|state|hospital central|central hospital|county"
    ) ~ "Tertiary/National Hospital",

    str_detect(
      combined,
      "provincial?|r[eé]gional"
    ) ~ "Regional/Provincial Hospital",

    str_detect(
      combined,
      "h[oô][sş]pita"
    ) ~ "Hospital (Other)",

    TRUE ~ "Other Primary Care"
  )
}

ssa_hf <- duckspatial::ddbs_read_vector(con, "ssa_hf_22") |>
  mutate(
    facility_c = classify_facility(facility_t, facility_n),
    facility_c = case_when(
      facility_c %in%
        c(
          "Tertiary/National Hospital",
          "Regional/Provincial Hospital"
        ) ~ "Referral Hospital",
      facility_c == "District Hospital" ~ "District Hospital",
      facility_c == "Hospital (Other)" ~ "Hospital (Other)",
      facility_c == "Maternal & Child Health" ~ "Maternal & Child Health",
      facility_c %in%
        c(
          "Health Centre/Clinic",
          "Health Post/Dispensary",
          "Other Primary Care"
        ) ~ "Primary Care",
      TRUE ~ "Other"
    ),
    facility_c = fct_rev(factor(facility_c, levels = hf_levels))
  ) |>
  arrange(facility_c)

map <- tm_shape(ssa_hf) +
  tm_dots(
    shape = 16,
    size = "facility_c",
    size.scale = tm_scale_categorical(values = size),
    fill.scale = tm_scale_categorical(values = pal),
    fill_alpha.scale = tm_scale_categorical(values = alpha),
    fill = "facility_c",
    fill.legend = tm_legend(
      str_to_title("Facility Type"),
      position = tm_pos_in("left", "bottom"),
      text.fontfamily = "Outfilt",
      title.fontfamily = font_text,
      orientation = "portrait",
      reverse = TRUE,
      title.color = color_white,
      title.size = 1.2,
      title.fontface = "bold",
      title.padding = c(.15, 0, 0, 0),
      text.color = color_white2,
      text.size = 1,
      item.space = .4,
      bg.color = NA,
      bg.alpha = 0,
      frame = FALSE,
      margins = c(2.6, 6, 0, 0)
    ),
    size.legend = tm_legend(show = FALSE)
  ) +
  tm_title(
    "Healthcare Infrastructure Across Sub-Saharan Africa",
    size = 2,
    fontface = "bold",
    color = color_white,
    fontfamily = font_text,
    just = c("left", "top"),
    padding = c(2, .6, -6.4, 0)
  ) +
  tm_credits(
    "96,395 facilities (2019) mapped by type and location",
    size = 1.2,
    position = c("left", "top"),
    just = c("left", "top"),
    padding = c(0, .4, -5.4, 0),
    fontfamily = font_text,
    color = color_white2,
  ) +
  tm_credits(
    glue::glue(
      "Note: Facilities with missing coordinates excluded. Facility classifications are approximations, ",
      "based on naming conventions. ",
      "Only hospital data were available for some regions.\n",
      "Source: HDX - Maina J. et al., Health Facilities in Sub-Saharan Africa, 2019 | #30DayMapChallenge 2025 - Day 1: Points | ",
      "Viz: @rajodm.bsky.social | #rstats | #tmap"
    ),
    color = color_cap,
    fontfamily = font_text,
    padding = c(0, 0, -16, 0),
    size = .75,
  ) +
  tm_layout(
    bg.color = color_bg,
    outer.bg.color = color_bg,
    inner.margins = c(.02, .02, .05, .02),
    outer.margins = rep(0, 4),
    frame = FALSE,
    panel.show = FALSE
  )


tmap::tmap_save(
  map,
  here::here("2025/maps", "day01_points_ssa-hf.png"),
  asp = 1 / 0.618,
  device = ragg::agg_png,
  width = 12,
  height = 10,
  dpi = 420
)

dbDisconnect(con)
