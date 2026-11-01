mdg <- rnaturalearth::ne_states(country = "Madagascar") |>
  st_transform(crs = "ESRI:102022")

mdg_hf <- ssa_hf |>
  st_filter(mdg) |>
  st_transform(crs = 4326)

mdg <- mdg |>
  st_transform(crs = 4326)

hex_grid <- mdg |>
  st_make_grid(
    cellsize = .5,
    square = FALSE
  ) %>%
  st_sf(hex_id = 1:length(.)) |>
  st_filter(mdg)

hex_w_count <- hex_grid %>%
  mutate(
    n_hosp = lengths(st_intersects(., mdg_hf)),
    acces_cat = case_when(
      n_hosp == 0 ~ "> 50 km",
      n_hosp == 1 ~ "25-50 km",
      n_hosp <= 3 ~ "10-25 km",
      TRUE ~ "< 10 km"
    )
  )

ggplot() +
  geom_sf(
    data = hex_w_count,
    aes(fill = acces_cat),
    color = NA,
    alpha = .7
  ) +
  geom_sf(
    data = mdg,
    fill = NA,
    color = "#100f0f",
    linewidth = .3
  ) +
  geom_sf(
    data = mdg_hf,
    color = "#dad8ce",
    size = .3,
    alpha = .5
  ) +
  scale_fill_manual(
    values = c(
      "< 10 km" = "#66a0c8",
      "10-25 km" = "#a0af54",
      "25-50 km" = "#dfb431",
      "> 50 km" = "#e47da8"
    )
  ) +
  theme_void()
