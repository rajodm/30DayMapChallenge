mdg <- rnaturalearth::ne_states(country = "kenya")

mdg_hf <- ssa_hf |>
  st_transform(crs = 4326) |>
  st_filter(mdg)

ssa_hf1 <- ssa_hf |>
  # filter(facility_c %in% c("Referral Hospital", "District Hospital", "Hospital (Other)")) |>
  st_transform(crs = 4326)

hex_grid <- st_make_grid(
  ssa_hf1,
  cellsize = .5,
  square = FALSE,
  what = "polygons"
) %>%
  st_sf(hex_id = 1:length(.)) |>
  st_filter(ssa_hf1)

hex_data <- hex_grid |>
  st_join(ssa_hf1) |>
  summarize(
    .by = hex_id,
    total_hf = n(),
    n_hosp = sum(
      facility_c %in%
        c("Referral Hospital", "District Hospital", "Hospital (Ohter)"),
      na.rm = TRUE
    ),
    geometry = .[1]
  ) |>
  replace_na(list(total_hf = 0, n_hosp = 0))

map_data <- hex_data |>
  st_transform(crs = "ESRI:102022")

tm_shape(hex_data) +
  tm_polygons()

ggplot() +
  geom_sf(
    data = map_data,
    aes(fill = n_hosp),
    color = "#100f0f",
    linewidth = .08,
    show.legend = FALSE
  ) +
  flexoki::scale_fill_flexoki_c("base") +
  theme_void() +
  theme_sub_plot(
    background = element_rect(fill = "#fffcf0", color = NA)
  ) +
  theme_sub_panel(
    background = element_rect(fill = "#fffcf0", color = NA)
  )

flexoki::flexoki
