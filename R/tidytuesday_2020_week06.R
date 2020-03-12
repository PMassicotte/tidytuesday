library(tidyverse)
library(glue)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')

standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

games %>%
  count(day, sort = TRUE)

games %>%
  ggplot(aes(x = day, y = yds_win)) +
  geom_boxplot()

games %>%
  mutate(win_at_home = home_team == winner) %>%
  count(day, win_at_home)
