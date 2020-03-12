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

player_max <- df_viz %>%
  group_by(age) %>%
  filter(total_goal == max(total_goal)) %>%
  arrange(age) %>%
  ungroup() %>%
  select(player2 = player, age, total_goal2 = total_goal)

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  left_join(player_max, by = "age") %>%
  filter(age <= 40) %>%
  mutate(label = glue::glue("{player2} ({age} years old) - {total_goal2} goals")) %>%
  mutate(label = fct_reorder(label, age)) %>%
  ggplot(aes(
    x = factor(age),
    y = total_goal,
    fill = mean_goal,
    color = mean_goal,
    group = age
  )) +
  # geom_point() +
  geom_sina() +
  coord_flip() +
  # geom_boxplot(size = 0.1, outlier.size = 1) +
  # geom_jitter() +
  scale_fill_viridis_c(option = "plasma") +
  scale_color_viridis_c(option = "plasma") +
  facet_wrap(~label,
    scales = "free_y",
    ncol = 1,
    strip.position = "right"
  ) +
  labs(
    x = "Player age",
    y = "Number of goals",
    title = str_wrap("At which age NHL players make the most goals?", 100),
    subtitle = "Player that scored the most goals at each age is indicated on the right",
    caption = "Tidytuesday 2020 week #10 | Data: https://www.hockey-reference.com/leaders/goals_career.html | @philmassicotte"
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
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(
      t = unit(10, "cm"),
      b = unit(20, "cm")
    )),
    plot.subtitle = element_text(
      hjust = 0.5,
      margin = margin(
        t = unit(0, "cm"),
        b = unit(30, "cm")
      )),
    # plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(color = "gray75", size = 8, hjust = 1),
    plot.caption.position = "plot",
    strip.text.y = element_text(
      margin = margin(10, 1, 10, 1, "cm"),
      angle = 360,
      color = "white",
      face = "plain",
      family = "Roboto",
      hjust = 0
    ),
    strip.background.y = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "lines")
  )

ggsave(
  here::here("graphs/", "tidytuesday_2020_week10.png"),
  type = "cairo",
  dpi = 600,
  width = 8.51,
  height = 7.29
)


