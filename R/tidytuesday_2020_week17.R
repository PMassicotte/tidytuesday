library(tidyverse)
library(sf)
library(countrycode)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Open Sans"))

gdpr_violations <- readr::read_tsv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv"
) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  filter(date >= "2019-01-01")

range(gdpr_violations$date)

gdpr_violations %>%
  count(name, type, sort = TRUE)

gdpr_violations %>%
  group_by(controller) %>%
  summarise(p = sum(price)) %>%
  arrange(desc(p))

sf <-
  st_read("data/2020week17/NUTS_RG_10M_2016_4326_LEVL_0.geojson") %>%
  mutate(across(is.factor, as.character)) %>%
  mutate(name = countrycode(CNTR_CODE,
    origin = "eurostat",
    destination = "country.name"
  )) %>%
  mutate(name = ifelse(name == "Czechia", "Czech Republic", name))

anti_join(gdpr_violations, sf, by = "name")

gdpr_violations <- gdpr_violations %>%
  group_by(name) %>%
  summarise(total_fine = sum(price), n_fine = n())

dat <- full_join(sf, gdpr_violations, by = "name")

# Outline
outline <- sf %>%
  group_by(name) %>%
  summarise()

# xlim = c(-25, 50), ylim = c(35, 75)

lim <- c(xmin = -12, xmax = 50, ymin = 35, ymax = 75)

outline <- outline %>%
  st_crop(lim)

dat <- dat %>%
  st_crop(lim)

lab <- outline %>%
  st_centroid() %>%
  left_join(gdpr_violations) %>%
  drop_na() %>%
  st_as_sf() %>%
  mutate(total_fine_label = scales::label_number_si()(total_fine)) %>%
  mutate(total_fine_label = paste0("\u20ac", total_fine_label)) %>%
  mutate(total_fine_label = paste0(name, "\n", total_fine_label)) %>%
  top_n(12, total_fine)

# Plot --------------------------------------------------------------------

dat %>%
  ggplot() +
  geom_sf(aes(fill = total_fine), size = 0.1) +
  geom_sf(data = outline, fill = "transparent", color = "white", size = 0.1) +
  geom_sf_text(
    data = lab,
    aes(label = total_fine_label, size = total_fine),
    color = "white",
    family = "Aldrich"
  ) +
  scale_size(range = c(3, 5)) +
  rcartocolor::scale_fill_carto_c(
    palette = "Burg",
    labels = scales::label_dollar(prefix = "\u20ac"),
    trans = "log10",
    na.value = "#BDA5AC"
  ) +
  labs(
    title = "GDPR enforcement by country:\nthe bad students of EU",
    caption = "Tidytuesday 2020 week #17\nData: https://www.privacyaffairs.com/gdpr-fines\n@philmassicotte",
    subtitle = str_wrap("The General Data Protection Regulation (GDPR) is a law in the EU for the protection of personal data that came into force on May 25, 2018. This map shows the top 10 countries that have been fined for violating this law between 2019-01-01 and 2020-03-25.", 100)
  ) +
  coord_sf() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "#2F4858"),
    panel.background = element_rect(fill = "#2F4858"),
    plot.title = element_text(
      size = 30,
      family = "Mada",
      color = "white",
      hjust = 0.5
    ),
    plot.caption = element_text(
      color = "gray75",
      family = "Roboto Condensed",
      size = 8
    ),
    plot.subtitle = element_text(
      color = "gray85",
      family = "Saira ExtraCondensed",
      size = 16,
      hjust = 0.5
    )
  )

outfile <- "graphs/tidytuesday_2020_week17.pdf"

ggsave(
  outfile,
  width = 10,
  height = 10,
  device = cairo_pdf
)

knitr::plot_crop(outfile)

outfile <- "graphs/tidytuesday_2020_week17.pdf"

ggsave(outfile,
  width = 10,
  height = 10,
  device = cairo_pdf
)

knitr::plot_crop(outfile)

bitmap <- pdftools::pdf_render_page(outfile, dpi = 600)
destfile <- here::here("graphs", "tidytuesday_2020_week17.png")
png::writePNG(bitmap, destfile)
