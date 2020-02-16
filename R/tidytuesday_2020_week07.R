library(tidyverse)
library(ggpmthemes)
library(rnaturalearth)
library(sf)
library(rvest)

rm(list = ls())

theme_set(theme_light_modified(base_family = "Londrina Solid Light"))

# Both hotels are located in Portugal: H1 at the resort region of Algarve and H2 at the city of Lisbon

url <- pins::pin(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv"
)

hotels <- read_csv(url)

hotels <- hotels %>%
  mutate(arrival_date = as.Date(
    paste(
      arrival_date_year,
      arrival_date_month,
      arrival_date_day_of_month
    ),
    format = "%Y %b %d"
  )) %>%
  mutate(stays_in_nights = stays_in_weekend_nights + stays_in_week_nights)

skimr::skim(hotels)

df <- hotels %>%
  mutate(country = case_when(country == "CN" ~ "CAN", TRUE ~ country)) %>%
  count(country, sort = TRUE) %>%
  filter(country != "NULL") %>%
  mutate(
    country_name = countrycode::countrycode(
      country,
      origin = "iso3c",
      destination = "country.name",
      custom_match = list("TMP" = "East Timor")
    )
  ) %>%
  unnest(country_name)

proj <- 3034

eu_countries <-
  st_read(
    "data/2020week07/NUTS_RG_10M_2016_3857_LEVL_3.geojson"
  ) %>%
  st_transform(crs = 4326) %>%
  st_crop(c(
    xmin = -10,
    xmax = 50,
    ymin = 35,
    ymax = 70
  )) %>%
  st_transform(crs = proj)

sf_country <-
  rnaturalearth::ne_countries(returnclass = "sf", scale = 50) %>%
  st_transform(crs = proj)

sf_country <- st_join(eu_countries, sf_country)

sf_country %>%
  ggplot() +
  geom_sf()

df <-
  full_join(df, sf_country %>% as_tibble(), by = c("country" = "adm0_a3")) %>%
  st_as_sf()

fromto <- df %>%
  select(country, n, geometry) %>%
  group_by(country, n) %>%
  # st_union() %>%
  summarise(center = st_centroid(st_union(geometry))) %>%
  # st_set_geometry(NULL) %>%
  cbind(., st_coordinates(.$center)) %>%
  as_tibble() %>%
  rename(
    longitude = X,
    latitude = Y
  ) %>%
  arrange(desc(n)) %>%
  filter(country != "PRT")

fromto <- fromto %>%
  drop_na(n) %>%
  mutate(bin = santoku::chop_equally(n, groups = 4))

fromto %>%
  distinct(bin)

# Plot --------------------------------------------------------------------

df %>%
  ggplot() +
  geom_sf(
    size = 0.1, color = "gray20",
    aes(fill = as.numeric(factor(country_name)))
  ) +
  annotate(
    "text",
    x = 5264359,
    y = 4251057,
    label = str_wrap("The thickness of each line is proportional to the number of hotel bookings made between 2015 and 2017.", 30),
    color = "gray75",
    hjust = 0,
    size = 3
  ) +
  geom_curve(
    data = fromto,
    aes(
      x = longitude,
      y = latitude,
      xend = 2564359,
      yend = 1651057,
      size = n
    ),
    curvature = 0.1,
    color = "gray80",
    alpha = 0.75,
    lineend = "round"
  ) +
  scale_size(range = c(0.1, 1)) +
  scale_fill_gradient(low = "gray30", high = "gray50") +
  labs(
    title = str_wrap(
      "Hotel bookings at two hotels located in the region of Algarve and in the city of Lisbon from other EU countries",
      40
    ),
    caption = "Tidytuesday 2020 week #7\nData: https://www.sciencedirect.com/science/article/pii/S2352340918315191#bib5\n@philmassicotte"
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      size = 18
    ),
    plot.caption = element_text(
      size = 6,
      color = "gray70",
      family = "Roboto Thin",
      hjust = 0
    )
  )

pdf_file <- "graphs/tidytuesday_2020_week07.pdf"

ggsave(pdf_file,
  device = cairo_pdf
)

knitr::plot_crop(pdf_file)

bitmap <- pdftools::pdf_render_page(pdf_file, dpi = 600)
png_file <- here::here("graphs", "tidytuesday_2020_week07.png")
png::writePNG(bitmap, png_file)
