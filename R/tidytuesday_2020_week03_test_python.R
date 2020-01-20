library(reticulate)
library(tidyverse)
library(ggpmthemes)


theme_set(theme_maven())

# Set the path to the Python executable file
use_python("C:/Users/pmass/AppData/Local/Programs/Python/Python38/", required = T)

# Check the version of Python.
py_config()



get_info <- function(password) {

  z <- import("zxcvbn")

  res <- z$zxcvbn(password)

  res$password
  res$guesses

  res$crack_times_seconds %>%
    enframe() %>% # creates the 'value' as a `list` column
    mutate(value = map(value, as.character)) %>% # change to single type
    unnest(value) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    mutate_all(parse_number) %>%
    # add_column(password = res$password, .before = 1) %>%
    add_column(score = res$score, .after = 1) %>%
    as_tibble()

}


passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

res <- passwords %>%
  # slice(1) %>%
  mutate(res = map(password, get_info)) %>%
  unnest(res)

res %>%
  filter(category != "NA") %>%
  filter(offline_fast_hashing_1e10_per_second < 1e-5) %>%
  mutate(password = fct_reorder(password, offline_fast_hashing_1e10_per_second, .fun = median)) %>%
  group_by(category) %>%
  top_n(10, offline_fast_hashing_1e10_per_second) %>%
  ungroup() %>%
  mutate(category = fct_reorder(category, offline_fast_hashing_1e10_per_second, max, .desc = T)) %>%
  ggplot(aes(x = password, y = offline_fast_hashing_1e10_per_second)) +
  geom_col(fill = "#687677") +
  # scale_y_log10() +
  coord_flip() +
  facet_wrap(~category, scales = "free_y") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
  labs(
    title = "sdfsdf sdf0234"
  ) +
  theme(
    strip.text = element_text(color = "#25364a", size = 14, face = "bold"),
    strip.background = element_blank(),
    axis.text.y = element_text(family = "Passero One")
  )

ggsave("c:/Users/pmass/Desktop/test.pdf", device = cairo_pdf, width = 12, height = 10)


res %>%
  filter(offline_fast_hashing_1e10_per_second < 1e-5) %>%
  mutate(password = fct_reorder(password, offline_fast_hashing_1e10_per_second)) %>%
  ggplot(aes(x = as.integer(password), y = offline_fast_hashing_1e10_per_second)) +
  geom_col() +
  coord_flip()
