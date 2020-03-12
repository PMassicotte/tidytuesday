library(tidyverse)
library(ggpmthemes)
library(ggforce)

theme_set(theme_maven())

game_goals <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv")

# top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')
#
# season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

df_viz <- game_goals %>%
  separate(age, into = c("age", "yday"), convert = TRUE) %>%
  group_by(player, age) %>%
  summarise(total_goal = sum(goals,na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(age) %>%
  mutate(mean_goal = median(total_goal)) %>%
  ungroup()

df_viz %>%
  group_by(age) %>%
  filter(total_goal == max(total_goal)) %>%
  arrange(age)

df_viz %>%
  ggplot(aes(x = factor(age), y = total_goal, fill = mean_goal, color = mean_goal)) +
  geom_sina() +
  coord_flip() +
  # geom_boxplot(size = 0.1, outlier.size = 1) +
  # geom_jitter() +
  scale_fill_viridis_c(option = "plasma") +
  scale_color_viridis_c(option = "plasma") +
  labs(
    y = "Player age",
    x = "Number of goals",
    title = str_wrap("At which age NHL players make the most goals?", 30)
  ) +
  # scale_y_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#43505F"),
    panel.background = element_rect(fill = "#43505F"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(color = "white", size = 6)
  )

# ggsave(
#   here::here("graphs/", "tidytuesday_2020_week10.png")
# )


