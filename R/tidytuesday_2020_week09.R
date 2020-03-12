library(tidyverse)
library(sf)

measles <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv"
)

measles %>%
  count(year)

measles %>%
  count(district)

df <- measles %>%
  filter(year == "2018-19")

range(df$enroll, na.rm = TRUE)

df %>%
  filter(enroll == 0)

df %>%
  filter_at(vars(c("overall", "mmr")), .vars_predicate = all_vars(. != -1)) %>%
  ggplot(aes(mmr, overall)) +
  geom_point() +
  geom_abline(color = "red")

df %>%
  count(county, sort = TRUE)

measles %>%
  filter(mmr != -1) %>%
  ggplot(aes(x = mmr, fill = year)) +
  geom_histogram()

df2 <- df %>%
  filter(mmr != -1) %>%
  group_by(county, lng, lat) %>%
  summarise(mean_mmr = mean(mmr, na.rm = TRUE), n = n()) %>%
  ungroup() %>%
  filter(lng < 0) %>%
  arrange(desc(n)) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

df2 %>%
  ggplot() +
  geom_sf()



states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  rename(state = ID) %>%
  mutate(state = str_to_title(state)) %>%
  as_tibble()

mmr <- measles %>%
  filter(mmr >= 0) %>%
  group_by(state) %>%
  summarise(mean_mmr = mean(mmr, na.rm = TRUE))

states %>%
  left_join(mmr) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_mmr / 100)) +
  rcartocolor::scale_fill_carto_c(
    palette = "Magenta",
    na.value = "grey95",
    labels = scales::label_percent()
  )
