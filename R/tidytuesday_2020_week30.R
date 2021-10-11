library(tidyverse)
library(sf)
library(ozmaps)
library(ggpmthemes)
library(ggtext)
library(patchwork)

theme_set(theme_light_modified(base_family = "Nunito Sans Black"))

animal_outcomes <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv"
  )

# animal_complaints <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv")

brisbane_complaints <-
  vroom::vroom(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv"
  )

brisbane_complaints %>%
  count(suburb, sort = TRUE)

brisbane_complaints %>%
  count(animal_type, sort = TRUE)

brisbane_complaints %>%
  count(category, sort = TRUE)

animal_outcomes %>%
  count(year, animal_type, sort = TRUE)

df_viz <- animal_outcomes %>%
  janitor::clean_names() %>%
  select(-total) %>%
  pivot_longer(act:wa, names_to = "state", values_to = "number_of_animal") %>%
  mutate(
    state = case_when(
      state == "act" ~ "Australian Captial Territory",
      state == "nsw" ~ "New South Wales",
      state == "nt" ~ "Northern Territory",
      state == "qld" ~ "Queensland",
      state == "sa" ~ "South Australia",
      state == "tas" ~ "Tasmania",
      state == "vic" ~ "Victoria",
      state == "wa" ~ "Western Australia",
      TRUE ~ state
    )
  )

df_viz <- df_viz %>%
  filter(animal_type %in% c("Cats", "Dogs")) %>%
  mutate(outcome = fct_lump(outcome, 4)) %>%
  group_by(animal_type, state, outcome) %>%
  summarise(number_of_animal = sum(number_of_animal, na.rm = TRUE)) %>%
  ungroup()

df_viz <- df_viz %>%
  group_by(animal_type, state) %>%
  filter(number_of_animal == max(number_of_animal))


df_viz <- ozmap_states %>%
  inner_join(df_viz, by = c("NAME" = "state"))

outline <- df_viz %>%
  st_union() %>%
  st_buffer(0.25)

p <- df_viz %>%
  mutate(center = st_centroid(geometry)) %>%
  ggplot() +
  geom_sf(
    data = outline,
    inherit.aes = FALSE,
    color = "white",
    fill = NA,
    size = 0.5
  ) +
  geom_sf(aes(fill = outcome), size = 0.1, color = "#3c3c3c") +
  facet_wrap(~animal_type) +
  geom_sf_text(aes(geometry = center, label = number_of_animal),
    size = 2,
    color = "#3c3c3c",
    fontface = "bold",
    family = "Exo"
  ) +
  scale_fill_manual(
    values = c("#5D8CA8", "#93C2DF", "#CBF7FF"),
    guide = guide_legend(
      title = NULL,
      label.position = "top",
      label.theme = element_text(color = "#616C74", family = "Nunito Sans Black"),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(1, "cm")
    )
  ) +
  coord_sf() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.background = element_rect(fill = "#3c3c3c"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#3c3c3c"),
    legend.background = element_rect(fill = "#3c3c3c"),
    legend.position = "bottom",
    legend.key = element_blank(),
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(color = "#616C74", size = 40)
  )

# p2 <- ggplot() +
#   annotate("richtext", x = 0, y = 0, label = "ASd") +
#   theme_void()
#
# p + p2


file <- here::here("graphs/tidytuesday_2020_week30.pdf")

ggsave(file,
  device = cairo_pdf(),
  width = 9,
  height = 9
)

knitr::plot_crop(file)
