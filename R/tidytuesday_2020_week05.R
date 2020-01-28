library(tidyverse)
library(sf)
library(ggpmthemes)
library(osmdata)
library(glue)
library(ggtext)

theme_set(theme_maven())

sf_trees <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv"
)

skimr::skim(sf_trees)

sf_trees %>%
  count(legal_status, sort = TRUE)

sf_trees %>%
  count(species, sort = TRUE)

sf_trees %>%
  count(site_info, sort = TRUE)

sf_trees %>%
  count(caretaker, sort = TRUE)

sf_trees %>%
  count(legal_status, sort = TRUE)

# San Francisco shapefile -------------------------------------------------

sf_shapefile <- curl::curl_download(
  "https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=Shapefile",
  destfile = tempfile(fileext = ".zip")
)

td <- tempdir()
sf_shapefile <- unzip(sf_shapefile, exdir = td)

sf <- st_read(td) %>%
  mutate(name = as.character(name))

sf_outline <- sf %>%
  st_simplify() %>%
  st_union() %>%
  st_buffer(dist = 0.001)

# Road --------------------------------------------------------------------

roads <- st_bbox(sf) %>%
  opq() %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

roads2 <- roads$osm_lines %>%
  st_transform(st_crs(sf)) %>%
  st_intersection(sf)

# -------------------------------------------------------------------------

hedge <- st_bbox(sf) %>%
  opq() %>%
  add_osm_feature("leisure", value = "park") %>%
  osmdata_sf()

hedge$osm_multipolygons %>%
  ggplot() +
  geom_sf()

parks <- st_union(hedge$osm_multipolygons, hedge$osm_polygons) %>%
  st_transform(st_crs(sf)) %>%
  st_intersection(sf)

parks <- parks %>%
  filter(str_detect(name, "Presidio|Golden|Merced|Twin"))

# Stats -------------------------------------------------------------------

trees <- sf_trees %>%
  drop_na(longitude, latitude) %>%
  filter(longitude >= -125) %>%
  # sample_n(1e4) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(sf))

df <- sf %>%
  st_join(trees)

df_viz <- df %>%
  # sample_n(1e3) %>%
  group_by(name) %>%
  summarise(
    mean_height = mean(dbh, na.rm = TRUE),
    n = n()
  )

# Plot --------------------------------------------------------------------

subtitle <- glue(
  "There are a total of **{nrow(sf_trees)} trees** in San Francisco regrouped into **{length(unique(sf_trees$species))} species**."
)

p <- df_viz %>%
  ggplot() +
  geom_sf(aes(fill = n), color = "#333333", size = 0.25) +
  rcartocolor::scale_fill_carto_c(
    palette = "DarkMint",
    labels = scales::label_number_auto(),
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(20, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      label.position = "bottom",
      title = "Number of trees"
    )
  ) +
  geom_sf(
    data = sf_outline,
    color = "#808080",
    fill = NA,
    size = 0.75
  ) +
  # geom_sf(
  #   data = roads2,
  #   color = "#383838",
  #   size = 0.05,
  #   alpha = 0.25,
  #   inherit.aes = FALSE
  # ) +
  coord_sf(crs = 7131) +
  scale_x_continuous(expand = c(0.15, 0.1)) +
  # geom_sf_label(
  #   data = parks,
  #   aes(label = name)
  # ) +
  labs(
    title = "The trees of San Francisco",
    subtitle = subtitle,
    caption = "Tidytuesday 2020 week #5 | Data: data.sfgov.org | @philmassicotte"
  ) +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.text = element_text(
      colour = "gray45",
      family = "Roboto Condensed",
      size = 8
    ),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    legend.background = element_rect(fill = "#333333"),
    legend.key = element_blank(),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = 0.5, family = "Amaranth", size = 32),
    plot.subtitle = element_markdown(color = "white", hjust = 0.5, family = "Titillium Web"),
    plot.caption = element_text(color = "gray75", size = 8, hjust = 0.5)
  )

destfile <- here::here("graphs", "tidytuesday_2020_week05.pdf")

ggsave(destfile,
  device = cairo_pdf,
  width = 8,
  height = 8
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "tidytuesday_2020_week05.png")
png::writePNG(bitmap, destfile)
