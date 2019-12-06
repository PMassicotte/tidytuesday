library(tidyverse)
library(ggpmthemes)
library(osmdata)
library(patchwork)

theme_set(theme_poppins())

url <-
  pins::pin(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv"
  )

bbox <- c(
  xmin = -75.3,
  xmax = -74.95,
  ymin = 39.85,
  ymax = 40.15
)

tickets <- readr::read_csv(url) %>%
  filter(between(lon, bbox[1], bbox[2]) &
    between(lat, bbox[3], bbox[4])) %>%
  mutate(
    violation_desc = case_when(
      violation_desc == "METER EXPIRED CC" ~ "METER EXPIRED",
      violation_desc == "STOP PROHIBITED CC" ~ "STOP PROHIBITED",
      TRUE ~ violation_desc
    )
  )

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

bbox2 <- bbox %>%
  matrix(ncol = 2, byrow = TRUE)

colnames(bbox2) <- c("min", "max")
rownames(bbox2) <- c("x", "y")

roads <- bbox2 %>%
  opq() %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

top_violation <- tickets %>%
  count(violation_desc, sort = TRUE) %>%
  top_n(4, n)

# Plot --------------------------------------------------------------------

tickets2 <- tickets %>%
  filter(between(lon, bbox[1], bbox[2]) &
    between(lat, bbox[3], bbox[4])) %>%
  filter(violation_desc %in% top_violation$violation_desc)

# %>%
#   sample_frac(0.01)

make_plot <- function(violation_desc, pal, df) {
  p <- df %>%
    filter(violation_desc == {{ violation_desc }}) %>%
    mutate(density = get_density(lon, lat, n = 100)) %>%
    mutate(violation_desc = str_to_title(violation_desc)) %>%
    ggplot(aes(x = lon, y = lat, color = density)) +
    geom_point(size = 0.05, alpha = 0.25) +
    geom_sf(
      data = roads$osm_lines,
      color = "gray50",
      size = 0.05,
      inherit.aes = FALSE
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_sf(xlim = bbox[1:2], ylim = bbox[3:4]) +
    scale_colour_distiller(palette = pal) +
    labs(title = violation_desc) +
    theme(
      legend.position = "none",
      panel.border = element_blank(),
      panel.background = element_rect(fill = "#3c3c3c", color = "#3c3c3c"),
      plot.background = element_rect(fill = "#3c3c3c", color = "#3c3c3c"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(
        color = "gray65",
        hjust = 0.5,
        size = 14
      ),
      legend.key = element_rect(size = 6, colour = NA),
      legend.key.size = unit(1, "cm"),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 10)
    )

  return(p)
}

p <- map2(
  top_violation$violation_desc,
  c("Blues", "Greens", "Oranges", "Purples"),
  .f = make_plot,
  df = tickets2
)

p_final <- p[[1]] + p[[2]] + p[[3]] + p[[4]] +
  plot_annotation(
    title = "Philly Tickets",
    subtitle = str_wrap("Density maps of parking tickets in Philadelphy by type of violation", 40),
    caption = "Tidytuesday week #49 | Data: https://www.opendataphilly.org/dataset/parking-violations | @philmassicotte",
    theme = theme(
      plot.background = element_rect(fill = "#3c3c3c"),
      panel.background = element_rect(fill = "#3c3c3c"),
      plot.caption = element_text(
        color = "gray85",
        size = 10,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        color = "gray85",
        size = 24,
        hjust = 0.5,
        margin = margin(b = 40, unit = "pt")
      ),
      plot.title = element_text(
        color = "gray85",
        size = 28,
        hjust = 0.5,
        margin = margin(b = 30, t = 40, unit = "pt")
      )
    )
  )

ggsave(
  here::here("graphs", "tidytuesday_2019_week49.png"),
  type = "cairo",
  dpi = 600,
  width = 9,
  height = 12
)

