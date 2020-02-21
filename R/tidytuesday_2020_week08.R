library(tidyverse)
library(ggforce)
library(ggpmthemes)

# For plot.title.position = "plot"
# devtools::install_github("tidyverse/ggplot2")

theme_set(theme_exo())

food_consumption <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv"
  )

food_consumption %>%
  count(country)

food_consumption %>%
  count(food_category)

info <- food_consumption %>%
  group_by(food_category) %>%
  filter(co2_emmission == min(co2_emmission))

food_consumption %>%
  mutate(food_category = str_wrap(food_category, 20)) %>%
  mutate(food_category = fct_reorder(food_category, -co2_emmission, mean)) %>%
  ggplot(aes(x = food_category, y = co2_emmission)) +
  geom_sina(aes(color = co2_emmission),
    method = "c",
    show.legend = FALSE,
    size = 1,
    scale = "width"
  ) +
  scale_y_continuous(trans = "log1p", limits = c(0, 1800), breaks = c(0, 1800)) +
  # scale_y_log10() +
  coord_flip() +
  scale_color_viridis_c(
    option = "plasma",
    trans = "log10",
    begin = 0.05
  ) +
  labs(
    y = bquote("CO"[2]~emission~(Kg~CO[2]~person^{-1}~year^{-1})),
    title = str_wrap("CO2 emission for various food categories around the World", 35),
    subtitle = str_wrap("Each point corresponds to one of the 130 countries included in the data", 40),
    caption = "Tidytuesday 2020 week #8\nData: https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018\n@philmassicotte"
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(size = 0.25, color = "gray30"),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(color = "gray75", size = 6)
  )

ggsave(
  here::here("graphs", "tidytuesday_2020_week08.png"),
  type = "cairo",
  dpi = 600
)
