library(tidyverse)
library(ggpmthemes)
library(ggtext)

theme_set(theme_light_modified(base_family = "Maven Pro"))

astronauts <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv"
  )

astronauts %>%
  count(nationality, sort = TRUE)

astronauts %>%
  count(occupation, sort = TRUE)

astronauts %>%
  count(military_civilian, sort = TRUE)

astronauts %>%
  count(selection, sort = TRUE)

astronauts %>%
  count(in_orbit, sort = TRUE)

df_viz <- astronauts %>%
  mutate(nationality_lump = fct_lump_n(nationality, 2)) %>%
  filter(str_detect(
    selection,
    regex("nasa|tspk", ignore_case = TRUE)
  )) %>%
  mutate(agency = case_when(
    str_detect(selection, fixed("nasa", ignore_case = TRUE)) ~ "nasa",
    str_detect(selection, fixed("tspk", ignore_case = TRUE)) ~ "tspk",
    TRUE ~ selection
  ))


# Plot --------------------------------------------------------------------


df_viz2 <- df_viz %>%
  count(year_of_mission, agency) %>%
  group_by(agency) %>%
  mutate(nn = cumsum(n)) %>%
  ungroup() %>%
  arrange(agency, year_of_mission)

set.seed(2020)
n <- 75
stars <- tibble(
  x = runif(n, 1960, 2020),
  y = runif(n, 0, 800),
  size = runif(n, 0, 2),
  color = gray.colors(n)
)

df_viz2 %>%
  ggplot(aes(x = year_of_mission, y = nn, color = agency)) +
  ggstar::geom_star(
    data = stars,
    aes(
      x,
      y,
      size = size,
      fill = color,
      color = after_scale(colorspace::lighten(fill, 0.4))
    ),
    inherit.aes = FALSE,
    # color = NA,
    alpha = 0.1,
    show.legend = FALSE,
    starstroke = 1
  ) +
  geom_line(
    show.legend = FALSE,
    size = 2,
    lineend = "round"
  ) +
  labs(
    title = "A race toward the space: cumulative number of<br>astronauts who participated in space missions<br>from <span style='color:#5991ff;font-family:\"Nasalization Rg\"'>NASA</span> and <span style='color:#BA224B;font-family:\"Open Sans\"'>TsPK</span> space agencies",
    caption = "Tidytuesday 2020 week #29\nData: https://www.sciencedirect.com/science/article/abs/pii/S2214552420300444\n@philmassicotte",
    x = NULL,
    y = NULL
  ) +
  scale_fill_identity() +
  scale_color_manual(values = c("nasa" = "#5991ff", "tspk" = "#BA224B")) +
  theme(
    plot.title = element_markdown(
      hjust = 0.5,
      color = "#95A1A9",
      size = 40
    ),
    plot.title.position = "plot",
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    axis.text = element_text(color = "#95A1A9", size = 18),
    axis.ticks = element_blank(),
    plot.caption = element_text(color = "white")
  )

pdf_file <- here::here("graphs/tidytuesday_2020_week29.pdf")
png_file <- here::here("graphs/tidytuesday_2020_week29.png")

ggsave(pdf_file,
  device = cairo_pdf,
  height = 8,
  width = 8 * 1.618
)

pdftools::pdf_convert(pdf_file,
  filenames = png_file,
  format = "png",
  dpi = 600
)
