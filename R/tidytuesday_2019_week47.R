library(tidyverse)
library(ggpmthemes)
library(ggforce)
library(ggrepel)

theme_set(theme_maven())

nz_bird <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv") %>%
  drop_na()

nz_bird %>%
  count(bird_breed, sort = TRUE)

df <- nz_bird %>%
  # filter(bird_breed == "Bittern") %>%
  group_by(date, hour, bird_breed) %>%
  summarise(n = n()) %>%
  arrange(date, hour) %>%
  group_by(bird_breed) %>%
  mutate(nn = cumsum(n)) %>%
  ungroup() %>%
  mutate(date2 = lubridate::make_datetime(
    lubridate::year(date),
    lubridate::month(date),
    lubridate::day(date),
    hour,
    0,
    0
  ))

labels <- df %>%
  group_by(bird_breed) %>%
  filter(nn == max(nn)) %>%
  ungroup() %>%
  filter(dense_rank(desc(nn)) <= 6) %>%
  select(bird_breed) %>%
  semi_join(df, .) %>%
  mutate(bird_breed = fct_reorder(bird_breed, nn, max))

df %>%
  ggplot(aes(x = date2, y = nn, group = bird_breed, fill = bird_breed)) +
  geom_line(color = "gray50", size = 0.1) +
  geom_line(data = labels, aes(color = bird_breed)) +
  scale_y_continuous(
    labels = scales::label_number_si(),
    breaks = scales::breaks_pretty(),
    expand = c(0, 0)
  ) +
  xlab(NULL) +
  ylab("Number of votes") +
  labs(
    title = "Cumulative votes for the New Zealand bird of the year",
    subtitle = str_wrap("The colored lines show the cumulative votes for the birds with the highest number of votes", 70),
    caption = "Tidytuesday 2019W47 | Visualization: @philmassicotte"
    ) +
  paletteer::scale_color_paletteer_d(
    rcartocolor,
    Vivid,
    guide = guide_legend(
      label.position = "top",
      nrow = 3,
      keywidth = unit(1, "cm"),
      override.aes = list(size = 2)
    )
  ) +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(0.01, 0.99),
    legend.title = element_blank(),
    plot.title = element_text(size = 14),
    legend.background = element_rect(fill = "#3c3c3c"),
    legend.key = element_blank(),
    legend.text = element_text(size = 8, margin = margin(b = -4, unit = "pt")),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    # plot.subtitle = element_markdown(lineheight = 1.1),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(color = "gray50"),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs", "tidytuesday_2019_week47.png"),
  type = "cairo",
  dpi = 600,
  width = 6,
  height = 6 / 1.6
)
