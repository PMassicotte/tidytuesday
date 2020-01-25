library(tidyverse)
library(ggridges)
library(paletteer)
library(ggforce)
library(ggpmthemes)

theme_set(theme_maven())

spotify_songs <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv"
  ) %>%
  mutate(year = str_match(track_album_release_date, "(^\\d{4})")[, 2]) %>%
  mutate(year = parse_number(year))

spotify_songs %>%
  count(playlist_genre)

spotify_songs %>%
  ggplot(aes(x = energy)) +
  geom_histogram() +
  facet_wrap(~playlist_genre)

skimr::skim(spotify_songs)

spotify_songs %>%
  # filter(liveness <= 0.4) %>%
  filter(playlist_genre == "rock") %>%
  ggplot(aes(
    x = liveness,
    y = year,
    fill = stat(x),
    group = year
  )) +
  geom_density_ridges_gradient(rel_min_height = 0.01) +
  paletteer::scale_fill_paletteer_c("gameofthrones::lannister") +
  facet_wrap(~playlist_genre)

spotify_songs %>%
  # mutate(playlist_genre = fct_reorder(playlist_genre, liveness, min)) %>%
  ggplot(aes(x = playlist_subgenre, y = year, fill = liveness)) +
  geom_tile() +
  facet_wrap(~playlist_genre, scales = "free_x") +
  theme(aspect.ratio = 1)

# Final plot --------------------------------------------------------------

spotify_songs %>%
  filter(playlist_genre == "rock") %>%
  distinct(track_artist) %>%
  pull()

labels <- spotify_songs %>%
  filter(track_artist %in% c("Metallica")) %>%
  distinct(track_album_name, track_name, .keep_all = TRUE)

df_viz <- spotify_songs %>%
  filter(playlist_genre == "rock") %>%
  group_by(year) %>%
  summarise(
    max_liveness = max(loudness),
    min_liveness = -max_liveness,
    n = n()
  )

df_viz %>%
  ggplot(aes(
    x = year,
    ymin = min_liveness,
    ymax = max_liveness,
    group = year
  )) +
  # scale_alpha(range = c(0.45, 1)) +
  geom_linerange(color = "#7da2cb", size = 1) +
  geom_mark_circle(
    data = labels,
    aes(
      x = year,
      y = 0,
      label = track_artist,
      description = track_name,
      group = interaction(track_name, track_artist)
    ),
    inherit.aes = FALSE,
    label.buffer = unit(20, "mm"),
    label.fontsize = 10,
    label.width = unit(5, "cm"),
    con.size = 0.25,
    con.colour = "white",
    color = NA,
    label.fill = NA,
    label.colour = "white"
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(
    y = "Loudness",
    title = "Evolution of Rock loudness between 1957 and 2020",
    subtitle = str_wrap("The overall loudness of a track in decibels (dB). Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db.", 110),
    caption = "Tidytuesday 2020 week #4 | Data: spotifyr | @philmassicotte\nLoudness definition from: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md"
  ) +
  theme(
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_text(color = "white", size = 18),
    axis.text.x = element_text(color = "white", size = 18),
    legend.position = "none",
    plot.title = element_text(color = "white", size = 32, hjust = 0.5),
    plot.subtitle = element_text(color = "white", size = 16, hjust = 0.5),
    plot.caption = element_text(color = "gray75", size = 12),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs", "tidytuesday_2020_week04.png"),
  type = "cairo",
  dpi = 600,
  height = 6,
  width = 12
)
