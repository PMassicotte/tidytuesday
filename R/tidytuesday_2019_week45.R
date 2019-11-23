library(tidyverse)
library(geofacet)
library(ggpmthemes)

theme_set(theme_exo2())

commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv") %>%
  mutate(state = case_when(
    state == "Massachusett" ~ "Massachusetts",
    state == "Ca" ~ "California",
    TRUE ~ state
  ))

p <- commute_mode %>%
  group_by(state, mode) %>%
  summarise(average_percent = mean(percent, na.rm = TRUE) / 100) %>%
  ggplot(aes(x = mode, y = average_percent, fill = mode)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~state_abb) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = scales::pretty_breaks(n = 2),
    expand = expand_scale(mult = c(0, 0.2))
  ) +
  geofacet::facet_geo(~state, grid = "us_state_grid2") +
  xlab(NULL) +
  ylab(NULL) +
  paletteer::scale_fill_paletteer_d(ggthemes, wsj_rgby) +
  labs(
    fill = "Commute\nmode",
    title = "Sdf",
    subtitle = "More people walk on the East coast of the US"
  ) +
  theme(
    axis.title = element_blank(),
    panel.spacing = unit(2, "lines"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  here::here("graphs", "tidytuesday_2019_week45.png"),
  type = "cairo",
  dpi = 600,
  width = 18,
  height = 11.5
)
