library(gtools)
library(tidyverse)

N <- 1000
alphas <- c(.05, .1, .25, .5, 1, 2, 4, 10, 20)
tmp <- map_dfr(alphas, function(.x) {
  rdirichlet(N, rep(.x, 20)) %>%
  as.data.frame() %>%
  set_names(paste0("theta", 1:20)) %>%
  mutate(idx = 1:N) %>%
  tidyr::gather(-idx, key = var, value = value) %>%
  arrange(idx, desc(value)) %>%
  group_by(idx) %>%
  mutate(rank = 1:n(),
         alpha = .x)
})

ggplot(data = tmp) +
  geom_line(aes(x = rank, y = value, group = idx), alpha = .01) +
  facet_wrap(~alpha, labeller = label_both) +
  scale_y_continuous("Selection Probability") +
  scale_x_continuous("Panel")



sim_lineup_model <- function(alpha, m = 20, k = 20, N = 50) {
  theta <- rdirichlet(1, rep(alpha, m))
  sels <- rmultinom(N, size = k, prob = theta)
  sels
}


alphas <- c(.001, .01, .05, .25, .5, 1, 2, 4, 20, 100, 1000)
tmp <- tibble(alpha = alphas,
       plot_sels = purrr::map(alpha, sim_lineup_model, N = 100000),
       max_sels = purrr::map(plot_sels, ~apply(., 2, max)),
       max2_sels = purrr::map(plot_sels, ~apply(., 2, function(x) order(x, decreasing = T)[2])),
       interesting_plot_count = purrr::map(plot_sels, ~colSums(.x > 3))) %>%
  unnest(max_sels, max2_sels, interesting_plot_count)

tmp %>%
  tidyr::gather(key = measure, value = value, max_sels:interesting_plot_count) %>%
  mutate(measure = str_replace_all(measure, c("max_sels" = "Selections of most popular panel",
                                              "max2_sels" = "Selections of second most popular panel",
                                              "interesting_plot_count" = "Panels with at least 3 selections"))) %>%
  ggplot() +
  geom_line(aes(x = value, y = ..count.., color = factor(alpha)), stat = "count") +
  geom_point(aes(x = value, y = ..count.., color = factor(alpha)), stat = "count") +
  facet_wrap(~measure, scales = "free") +
  ggtitle("Prior Predictive Distributions") #+
  # scale_x_discrete("Count") + scale_y_discrete("# Simulations")

ggplot(tmp) + geom_histogram(aes(x = interesting_plot_count)) + facet_wrap(~alpha, labeller = label_both) +
  ggtitle("# panels with at least 5 selections out of 20 evaluations")

ggplot(tmp) + geom_histogram(aes(x = max_sels)) + facet_wrap(~alpha, labeller = label_both) +
  ggtitle("# selections of most popular panel")

