library(vinference)
library(tidyverse)

alphas <- exp(seq(-6, 6, by = .01))
data_breaks <- c(1:6, 10, 15, 20)

source("code/functions.R")

point_ests <- tibble(
  C = data_breaks, K = 20, m = 20,
  binom = purrr::pmap_dbl(list(q = C - 1, size = K, prob = 1/m), pbinom, lower.tail = F) # Consistent with vinference pkg
)

dist_ests <- tidyr::crossing(C = data_breaks, K = 20, m = 20, rep = 1:100) %>%
  mutate(
  sim = purrr::pmap(list(x = C, K = K, m = m), vinference::pVsim, N = 5000, scenario = 3, upper.tail = T),
  sim = purrr::map(sim, ~as_tibble(.) %>%
                            select(scenario3 = simulated, binom_sim = binom))) %>%
  unnest_wider(sim)

sim_dir_multinom <- function(N = 2000, m = 20, K = NULL, alpha = 1) {
  if (is.null(K)) stop("Please specify K")
  rmultinom(N, size = K, prob = rdirichlet(n = 1, rep(alpha, m)))
}

b_binomial_sim <- function(x, target = 1, lower.tail = F, ...) {
  args <- list(...)
  obs <- do.call(sim_dir_multinom, args)
  if (target > 1) {
    t_sel <- colSums(obs[1:target,])
  } else {
    t_sel <- obs[1,]
  }

  if (lower.tail)  mean(t_sel <= x)
  else mean(t_sel > x)
}

dist_ests2 <- tidyr::crossing(C = data_breaks, K = 20, m = 20, rep = 1:100) %>%
  mutate(val = purrr::pmap_dbl(list(x = C, m = m, K = K, alpha = 1),
                               b_binomial_sim, target = 1, lower.tail = F))

alpha_ests <- tidyr::crossing(C = data_breaks, K = 20, m = 20, alphas = alphas) %>%
  mutate(p = purrr::pmap_dbl(list(C = C, K = K, alpha = alphas, m = m), vis_p_value))



ggplot(alpha_ests, aes(x = alphas, y = p, color = factor(C), group = factor(C))) +
  geom_line(size = 1) +
  geom_point(aes(x = exp(6.25), y = binom, color = factor(C),
                 shape = "Binomial\np-value"), data = point_ests, size = 2) +
  geom_jitter(aes(x = 1, y = scenario3, color = factor(C), shape = "Scenario 3\nsimulation\nvalue"),
              data = dist_ests, alpha = .1) +
  geom_jitter(aes(x = 1, y = scenario3, color = factor(C), shape = "BetaBinomial\nsimulation\nvalue"),
              data = dist_ests, alpha = .1) +
  scale_y_continuous("Visual p-value") +
  scale_x_continuous(expression(alpha), trans = "log10", breaks = c(0.001, 0.01, .1, 1, 10, 100),
                     labels = c("0.001", "0.01", "0.1", "1", "10", "100")) +
  scale_color_brewer("# Data\nPanel\nIdentifications\n(K = 20)", palette = "Paired") +
  scale_shape_discrete("") +
  geom_hline(yintercept = 0.05, color = "grey") +
  guides(color = guide_legend(override.aes = list(shape = NA))) +
  annotate("segment", x = exp(6.25), xend = exp(6.25), y = -Inf, yend = .7, color = "grey", alpha = .5) +
  annotate("text", x = exp(6.25), y = 0.75,
           label = "Binomial\nmodel", vjust = 1, hjust = .75) +
  annotate("segment", x = 1, xend = 1, y = -Inf, yend = .7, color = "grey", alpha = .5) +
  annotate("text", x = 1, y = 0.75,
           label = "vinference\npackage\n(Scenario 3)", vjust = 1, hjust = .5) +
  theme_bw()
