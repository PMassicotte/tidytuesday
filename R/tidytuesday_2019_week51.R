library(tidyverse)
library(ggtext)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Alef"))

url <- pins::pin("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv")

dog_descriptions <- readr::read_csv(url)

dog_descriptions %>%
  count(contact_state)

df <- dog_descriptions %>%
  filter(contact_state %in% state.abb)

df %>%
  count(contact_state, sort = TRUE)

us_population <- read_csv("https://raw.githubusercontent.com/jakevdp/PythonDataScienceHandbook/master/notebooks/data/state-population.csv") %>%
  janitor::clean_names() %>%
  filter(ages == "total" & year == max(year))

df_viz <- df %>%
  count(contact_state, sort = TRUE) %>%
  left_join(us_population, by = c("contact_state" = "state_region")) %>%
  mutate(human_dog_ratio = population / n) %>%
  mutate(state = state.name[match(contact_state, state.abb)]) %>%
  mutate(state = fct_reorder(state, human_dog_ratio))

subtitle <- "***Adopt not shop*** consists in finding a new dog at a local shelter<br>or rescue organization rather than a pet store or breeder<br>(*pudding.cool*). In the USA, the ratios of human population<br>to adoptable dogs vary greatly among states."

df_viz %>%
  ggplot(aes(y = human_dog_ratio, x = state, fill = human_dog_ratio)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.02))) +
  xlab(NULL) +
  ylab("Population-to-dog ratio") +
  paletteer::scale_fill_paletteer_c("ggthemes::Orange") +
  labs(
    title = "Adoptable dogs in the USA",
    subtitle = subtitle,
    caption = "Tidytuesday week #51 | Data: https://pudding.cool/2019/10/shelters/ | @philmassicotte"
  ) +
  theme(
    legend.position = "none",
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    axis.text = element_text(color = "white"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(color = "gray50", size = 0.1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5, family = "Antic"),
    plot.caption = element_text(color = "gray60", size = 10)
  )

ggsave(
  here::here("graphs", "tidytuesday_2019_week51.png"),
  type = "cairo",
  dpi = 600,
  width = 7,
  height = 8
)

