nose <- tibble(
  x = c(-1.5, -0.5, 0, 0.5, 1.5),
  y = c(5, 3, 3, 3, 5)
)

face <- tibble(
  x = c(0, -5, -5, 0, 5, 5, 0),
  y = c(10, 10, 0, 0, 0, 10, 10)
)

b <- tibble(
  x = c(-1.5, -3, -3),
  y = c(4.5, 5, 5.5)
) %>%
  crossing(angle = seq(0, pi, length.out = 4)) %>%
  group_nest(angle) %>%
  mutate(scale = seq(1, 1.5, length.out = 4)) %>%
  mutate(l = map2(data, angle, function(data, angle) {

    spdep::Rotation(data - (angle * 0.2), angle * pi / 180) %>%
      as.data.frame() %>%
      as_tibble() %>%
      set_names(c("x", "y"))

  })) %>%
  unnest(l) %>%
  arrange(angle, y)

ggplot() +
  geom_bspline_closed(
    data = nose,
    aes(x = x, y = y)
  ) +
  geom_circle(aes(x0 = -2, y0 = 7, r = 0.5), size = 2) +
  geom_circle(aes(x0 = 2, y0 = 7, r = 0.5), size = 2) +
  geom_bspline(
    data = face,
    aes(x = x, y = y)
  ) +
  geom_bspline(
    data = b,
    aes(x, y, group = factor(angle), color = scale)
  ) +
  # geom_point(
  #   data = nose,
  #   aes(x = x, y = y)
  # ) +
  coord_equal()
