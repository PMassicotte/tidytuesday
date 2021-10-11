library(tidyverse)

tdf_winners <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv"
  )

tdf_winners %>%
  count(age, sort = TRUE)

tdf_winners %>%
  mutate(nn = fct_lump_n(nationality, 5)) %>%
  # mutate(nn = fct_reorder(nn, ))
  ggplot(aes(
    x = lubridate::year(start_date),
    y = distance,
    fill = nn
  )) +
  geom_col() +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 12))

tdf_winners %>%
  ggplot(aes(
    x = lubridate::year(start_date),
    y = age
  )) +
  geom_point() +
  geom_line()

tdf_winners %>%
  ggplot(aes(height, weight)) +
  geom_point()

tdf_winners %>%
  mutate(year = lubridate::year(start_date)) %>%
  ggplot(aes(x = year, y = weight)) +
  geom_point()

tdf_winners %>%
  mutate(year = lubridate::year(start_date)) %>%
  ggplot(aes(x = year, y = distance)) +
  geom_point() +
  facet_wrap(~winner_team, scales = "free_x")

tdf_winners %>%
  filter(distance >= 3000) %>%
  mutate(nationality = fct_lump_n(nationality, 3)) %>%
  ggplot(aes(x = time_overall, y = distance, color = nationality)) +
  geom_point() +
  ggforce::geom_mark_circle(
    aes(
      label = winner_name,
      description = winner_team,
      filter = distance == max(distance, na.rm = T)
    ),
    show.legend = FALSE
  )
