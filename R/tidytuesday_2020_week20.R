library(tidyverse)
library(sf)
library(ggpmthemes)

theme_set(theme_poppins())

volcano <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv")
eruptions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv")
events <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv")
tree_rings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv")
sulfur <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv")

eruptions <- eruptions %>%
  mutate(start_eruption = lubridate::make_date(start_year, start_month, start_day)) %>%
  mutate(end_eruption = lubridate::make_date(end_year, end_month, end_day)) %>%
  mutate(duration_eruption = lubridate::interval(start_eruption, end_eruption)) %>%
  mutate(duration_eruption = as.numeric(duration_eruption, "years")) %>%
  mutate(duration_eruption = round(duration_eruption, digits = 0))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

df_viz <- eruptions %>%
  top_n(25, duration_eruption)

p <- df_viz %>%
  ggplot(aes(x = longitude, y = latitude, group = volcano_name)) +
  geom_sf(data = world, inherit.aes = FALSE, size = 0.25) +
  geom_point(size = 4, color = "white") +
  geom_point(size = 3) +
  ggforce::geom_mark_circle(
    aes(label = volcano_name, description = duration_eruption),
    expand = unit(1, "mm"),
    label.family = "Oxanium",
    label.minwidth = unit(200, "cm"),
    label.fill = "red"
  )

ggsave(
  here::here("graphs/tidytuesday_2020_week20.pdf"),
  device = cairo_pdf,
  width = 14,
  height = 14
)
