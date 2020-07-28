library(tidyverse)
library(patchwork)
library(ggtext)

theme_set(theme_light())

set.seed(2020)

penguins <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv"
  )

chinstrap_color <- "#C05BCB"
gentoo_color <- "#056F75"
adelie_color <- "#FF6A00"
background_color <- "black"

my_theme <- theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  legend.position = "none",
  panel.background = element_rect(fill = background_color, color = background_color),
  plot.background = element_rect(fill = background_color, color = background_color),
  axis.text.y = element_text(color = "grey60"),
  axis.title.y = element_text(color = "grey50"),
  plot.margin = unit(c(0, 0, 0, 0.5), "cm")
)

penguins_means <- penguins %>%
  group_by(species) %>%
  summarise(across(
    c(bill_length_mm, bill_depth_mm, flipper_length_mm),
    .fns = mean,
    na.rm = TRUE
  ))

wiki_definition <- tibble(
  species = c("Adelie", "Chinstrap", "Gentoo"),
  text = c(
    "The <span style='color:#FF6A00'>**Adélie penguin**</span> (*Pygoscelis adeliae*) is a species of penguin common along the entire coast of<br>the Antarctic continent, which is its only habitat. It is the most widely spread penguin species,<br>as well as the most southerly distributed of all penguins, along with the emperor penguin.",
    "The <span style='color:#C05BCB'>**chinstrap penguin**</span> (*Pygoscelis antarcticus*) is a species of penguin that inhabits a variety of<br>islands and shores in the Southern Pacific and the Antarctic Oceans. Its name stems from the narrow<br>black band under its head, which makes it appear as if it were wearing a black helmet, making it<br>easy to identify.",
    "The <span style='color:#056F75'>**gentoo penguin**</span> (*Pygoscelis papua*) is a penguin species in the genus Pygoscelis, most closely<br>related to the Adélie penguin (*P. adeliae*) and the chinstrap penguin (*P. antarcticus*). The earliest<br>scientific description was made in 1781 by Johann Reinhold Forster with a type locality in the<br>Falkland Islands. They call in a variety of ways, but the most frequently heard is a loud trumpeting<br> which the bird emits with its head thrown back."
  )
)

p0 <- ggplot(wiki_definition) +
  geom_richtext(
    aes(
      x = 0,
      y = 0,
      label = text
    ),
    color = "white",
    label.color = NA,
    fill = NA,
    size = 2,
    family = "Exo",
    lineheight = 2
  ) +
  theme_void() +
  geom_vline(aes(xintercept = -Inf, color = species), size = 2) +
  theme(
    panel.background = element_rect(fill = background_color, color = background_color),
    plot.background = element_rect(fill = background_color, color = background_color),
    legend.position = "none"
  ) +
  facet_wrap(~species) +
  scale_color_manual(
    values = c(
      "Gentoo" = gentoo_color,
      "Chinstrap" = chinstrap_color,
      "Adelie" = adelie_color
    )
  )

p1 <- penguins %>%
  ggplot(aes(
    y = species,
    x = bill_length_mm,
    color = species,
    group = species
  )) +
  geom_jitter(
    alpha = 0.3,
    stroke = 0,
    shape = 16,
    size = 2
  ) +
  coord_flip() +
  scale_color_manual(
    values = c(
      "Gentoo" = gentoo_color,
      "Chinstrap" = chinstrap_color,
      "Adelie" = adelie_color
    )
  ) +
  my_theme +
  geom_vline(
    xintercept = -Inf,
    size = 0.5,
    color = "grey60"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.25, 0)),
    breaks = scales::breaks_pretty(n = 5)
  ) +
  labs(x = "Bill length (mm)") +
  geom_errorbar(
    data = penguins_means,
    width = 0.6,
    size = 0.5,
    aes(xmax = ..x.., xmin = ..x.., x = bill_length_mm)
  )

p2 <- penguins %>%
  ggplot(aes(
    y = species,
    x = bill_depth_mm,
    color = species,
    group = species
  )) +
  geom_jitter(
    alpha = 0.3,
    stroke = 0,
    shape = 16,
    size = 2
  ) +
  coord_flip() +
  scale_color_manual(
    values = c(
      "Gentoo" = gentoo_color,
      "Chinstrap" = chinstrap_color,
      "Adelie" = adelie_color
    )
  ) +
  my_theme +
  geom_vline(
    xintercept = -Inf,
    size = 0.5,
    color = "grey60"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.25, 0)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  labs(x = "Bill depth (mm)") +
  geom_errorbar(
    data = penguins_means,
    width = 0.6,
    size = 0.5,
    aes(xmax = ..x.., xmin = ..x.., x = bill_depth_mm)
  )

p3 <- penguins %>%
  ggplot(aes(
    y = species,
    x = flipper_length_mm,
    color = species,
    group = species
  )) +
  geom_jitter(
    alpha = 0.3,
    stroke = 0,
    shape = 16,
    size = 2
  ) +
  coord_flip() +
  scale_color_manual(
    values = c(
      "Gentoo" = gentoo_color,
      "Chinstrap" = chinstrap_color,
      "Adelie" = adelie_color
    )
  ) +
  my_theme +
  scale_x_continuous(
    expand = expansion(mult = c(0.25, 0)),
    breaks = scales::breaks_pretty(n = 4)
  ) +
  labs(x = "Flipper length (mm)") +
  geom_errorbar(
    data = penguins_means,
    width = 0.6,
    size = 0.5,
    aes(xmax = ..x.., xmin = ..x.., x = flipper_length_mm)
  )

p <- p0 / p1 / p2 / p3 +
  plot_annotation(
    theme = theme(
      panel.background = element_rect(fill = background_color, color = background_color),
      plot.background = element_rect(fill = background_color, color = background_color),
      panel.border = element_blank(),
      plot.title = element_text(
        color = "white",
        family = "Poppins",
        face = "bold",
        size = 24,
        hjust = 0.5
      ),
      plot.caption = element_text(
        color = "gray55",
        family = "Poppins",
        size = 6
      )
    ),
    title = "The morphology of the Palmer Penguins",
    caption = "Tidytuesday 2020 week #31\nData: https://doi.org/10.1371/journal.pone.0090081\nPenguins definitions copied from Wikipedia\n@philmassicotte"
  ) +
  plot_layout(heights = c(1, 2, 2, 2))

ggsave(
  here::here("graphs/tidytuesday_2020_week31.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 12
)

pdftools::pdf_convert(
  here::here("graphs/tidytuesday_2020_week31.pdf"),
  format = "png",
  filenames = here::here("graphs/tidytuesday_2020_week31.png"),
  dpi = 600
)
