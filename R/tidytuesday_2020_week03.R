library(tidyverse)
library(ggpmthemes)

theme_set(theme_poppins())

passwords <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv"
  ) %>%
  drop_na()

psw <- passwords %>%
  top_n(20, strength)

psw <- passwords %>%
  group_nest(category) %>%
  mutate(t = map2(category, data, function(category, data) {
    data %>%
      mutate(category = category) %>%
      add_row(category = category) %>%
      add_row(category = category) %>%
      add_row(category = category)
  })) %>%
  select(-category) %>%
  unnest(t) %>%
  rowid_to_column() %>%
  mutate(password = fct_reorder(password, category))

label_data <- psw
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$rowid - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 0, 1)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

cat <- psw %>%
  drop_na() %>%
  group_nest(category) %>%
  mutate(pos = map_dbl(data, ~ mean(.$rowid, na.rm = TRUE))) %>%
  mutate(angle = 90 - 360 * (pos - 0.5) / number_of_bar) %>%
  mutate(angle2 = ifelse(angle < -90, angle + 180, angle)) %>%
  mutate(hjust = ifelse(angle < -90, 0, 1))

p <- psw %>%
  ggplot(aes(
    x = factor(rowid),
    y = strength + 1,
    fill = category
  )) +
  # geom_col() +
  geom_col() +
  scale_y_continuous(limits = c(-50, 50), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_polar(start = 0) +
  geom_text(
    data = label_data,
    aes(
      x = factor(rowid),
      y = -1,
      label = password,
      hjust = hjust
    ),
    color = "white",
    fontface = "plain",
    alpha = 0.9,
    size = 0.66,
    angle = label_data$angle,
    inherit.aes = FALSE,
    family = "Source Sans Pro"
  ) +
  paletteer::scale_fill_paletteer_d("vapoRwave::vapoRwave") +
  paletteer::scale_color_paletteer_d("vapoRwave::vapoRwave") +
  geom_text(
    data = cat,
    aes(
      x = pos,
      y = -5,
      label = str_replace_all(category, "-", "\n"),
      angle = angle2,
      color = category,
      hjust = hjust
    ),
    size = 4
  ) +
  annotate(
    "text",
    x = 0,
    y = -50,
    label = "How bad are\nyour passwords?",
    color = "gray75",
    family = "Nobile",
    fontface = "bold",
    size = 8
  ) +
  # annotate(
  #   "text",
  #   x = 0,
  #   y = -50,
  #   label = str_wrap("The height of each bar represents the strength of each password", 40),
  #   color = "gray75",
  #   family = "Nobile",
  #   fontface = "plain",
  #   size = 4
  # ) +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    plot.margin=unit(c(-50,-85,-85,-40), "mm")
  )

ggsave(
  here::here("graphs", "tidytuesday_2020_week03.png"),
  type = "cairo",
  dpi = 600,
  height = 8,
  width = 8
)

