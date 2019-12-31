library(tidyverse)
library(tidytext)
library(widyr)
library(ggtext)
library(ggfittext)

library(ggpmthemes)

theme_set(theme_maven())

christmas_lyrics <-
  readr::read_tsv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_lyrics.tsv"
  )

tokens <- christmas_lyrics %>%
  tidytext::unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>%
  add_count(word) %>%
  filter(n >= 100) %>%
  mutate(weekid = anytime::anydate(weekid))

correlations <- tokens %>%
  pairwise_cor(word, songid, sort = TRUE, method = "pearson")

correlations

# Which word is the most frequent?
tokens %>%
  filter(n == max(n))

df_viz <- correlations %>%
  filter(item1 == "christmas") %>%
  mutate(item2 = str_to_title(item2)) %>%
  mutate(item2 = fct_reorder(item2, correlation)) %>%
  filter(!is.nan(correlation))

df_viz %>%
  ggplot(aes(x = item2, y = correlation, label = item2)) +
  geom_col(fill = "#BB2528", position = "dodge") +
  coord_flip() +
  xlab(NULL) +
  ylab("Pearson's correlation") +
  labs(
    title = "The correlations of Billboard top 100 Christmas songs",
    subtitle = "The word **christmas** is often used in Christmas songs. But with what other words present in the<br>lyrics of the top 100 songs of Bilboard is it correlated?",
    caption = "Tidytuesday week #52 | Data: https://www.kaggle.com/sharkbait1223/billboard-top-100-christmas-carol-dataset | @philmassicotte"
  ) +
  geom_bar_text(
    position = "dodge",
    grow = FALSE,
    reflow = FALSE,
    place = "left",
    color = "white",
    fontface = "bold",
    family = "Mountains of Christmas"
  ) +
  theme(
    plot.subtitle = element_markdown(),
    plot.background = element_rect(fill = "#185C34", color = NA),
    panel.background = element_rect(fill = "#185C34", color = NA),
    panel.grid = element_line(color = "#4BA928"),
    panel.border = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white", face = "bold"),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    plot.caption = element_text(family = "Exo", size = 8, color = "gray75")
  )

ggsave(
  here::here("graphs", "tidytuesday_2019_week52.png"),
  type = "cairo",
  dpi = 600,
  height = 8,
  width = 8
)

