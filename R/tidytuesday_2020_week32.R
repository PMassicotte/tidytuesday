library(tidyverse)
library(sf)
library(ggtext)


country_totals <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv") %>%
  pivot_longer(matches("\\d{4}"), names_to = "year", values_to = "production") %>%
  filter(country %in% c("FR")) %>%
  filter(year == 2018)

country_totals %>%
  count(country, country_name, sort = TRUE)

countries <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  filter(iso_a2 %in% country_totals$country)

countries <- st_read("/home/filoche/Downloads/FRA_adm/FRA_adm0.shp") %>%
  rename(iso_a2 = ISO2)

country_totals_sf <- countries %>%
  inner_join(country_totals, by = c("iso_a2" = "country"))

country_totals_sf %>%
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = iso_a2))

country_totals_sf %>%
  st_simplify(dTolerance = 0.01) %>%
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = iso_a2))

country_totals_sf_grid <- country_totals_sf %>%
  st_simplify(dTolerance = 0.01) %>%
  st_make_grid(n = 250, square = FALSE) %>%
  st_as_sf() %>%
  st_join(countries) %>%
  group_nest(iso_a2) %>%
  mutate(grid_id = map(data, ~1:nrow(.))) %>%
  unnest(c(data, grid_id)) %>%
  st_as_sf()

country_totals_sf_grid %>%
  ggplot() +
  geom_sf(aes(fill = iso_a2)) +
  geom_sf_text(aes(label = grid_id), size = 2)

props <- country_totals %>%
  count(iso_a2 = country, type, wt = production) %>%
  group_by(iso_a2) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  group_nest(iso_a2, .key = "prop_data")

country_totals_sf_grid <- country_totals_sf_grid %>%
  group_nest(iso_a2) %>%
  left_join(props) %>%
  drop_na(iso_a2)

country_totals_sf_grid <- country_totals_sf_grid %>%
  mutate(data = map2(data, prop_data, function(data, prop_data) {

    BBmisc::chunk(data$grid_id, props = prop_data$prop) %>%
      set_names(prop_data$type) %>%
      enframe() %>%
      unnest(value) %>%
      bind_cols(data)

  }))

outline <- country_totals_sf_grid %>%
  unnest(data) %>%
  st_as_sf() %>%
  group_by(iso_a2) %>%
  summarise(admi = sum(ID_0))

outline %>%
  ggplot() +
  geom_sf(aes(color = iso_a2), fill = NA)

# Plot --------------------------------------------------------------------

props_labels <- props %>%
  unnest(prop_data) %>%
  mutate(prop_percent = round(prop * 100, digits = 2)) %>%
  filter(n > 0) %>%
  mutate(color = paletteer::paletteer_d("ggsci::light_uchicago")[1:nrow(.)]) %>%
  arrange(desc(prop_percent)) %>%
  mutate(label = glue::glue("<i style='color:{color}'>{type}: {prop_percent}%</i><br>"))

props_labels

p <- country_totals_sf_grid %>%
  unnest(data) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = name), size = 0.1, color = "#373f51") +
  geom_sf(
    data = outline,
    color = "white",
    fill = NA,
    size = 0.5
  ) +
  theme(
    panel.background = element_rect(fill = "#373f51"),
    plot.background = element_rect(fill = "#373f51"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_blank(),
    plot.subtitle = element_markdown(lineheight = 1.25, face = "bold", size = 14),
    plot.title = element_text(
      color = "white",
      size = 28,
      hjust = 0.5,
      family = "Lilita One",
      margin = margin(b = 20, t = 20)
    ),
    plot.margin = margin(l = 40, r = 40, b = 5),
    plot.caption = element_text(color = "gray75")
  ) +
  paletteer::scale_fill_paletteer_d("ggsci::light_uchicago") +
  labs(
    title = "Total net energy production in France in 2018",
    subtitle = paste0(props_labels$label, collapse = ""),
    caption = "Tidytuesday 2020 week #32 | Data: https://bit.ly/2DyGpoc1 | @philmassicotte"

  )

ggsave(
  here::here("graphs/tidytuesday_2020_week32.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 10
)

knitr::plot_crop(here::here("graphs/tidytuesday_2020_week32.pdf"))

pdftools::pdf_convert(
  pdf = here::here("graphs/tidytuesday_2020_week32.pdf"),
  filenames = here::here("graphs/tidytuesday_2020_week32.png"),
  format = "png",
  dpi = 600
)
