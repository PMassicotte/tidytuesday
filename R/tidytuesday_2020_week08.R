library(tidyverse)
library(ggforce)

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
  mutate(food_category = fct_reorder(food_category, -co2_emmission, mean)) %>%
  ggplot(aes(x = food_category, y = co2_emmission + 0.1)) +
  geom_sina(aes(color = co2_emmission + 0.1), show.legend = FALSE) +
  scale_y_log10() +
  coord_flip() +
  paletteer::scale_color_paletteer_c("vapoRwave::vapoRwave") +
  # scale_color_viridis_c(option = "plasma", trans = "log10") +
  geom_mark_circle(
    data = info, aes(

    description = co2_emmission,
    label = country,
    fill = country
  ),
  label.fontsize = c(6, 4)) +
  theme(legend.position = "none")

ggsave("c:/Users/pmass/Desktop/test.pdf", width = 10, height = 10)
