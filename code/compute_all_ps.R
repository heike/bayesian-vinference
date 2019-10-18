library(tidyverse)
library(vinference)

setwd(here::here())
source("code/process_all_lineup_data.R")

mvbeta <- function(alpha, log = F) {
  z <- sum(lgamma(alpha)) - lgamma(sum(alpha))
  if (!log) return(exp(z)) else return(z)
}

bf <- function(a1, a2, m = 20, c, k = sum(c)) {
  stopifnot(a1 > 0, a2 > 0, c <= k, m > 1)

  beta(a2, (m - 1)*a2) * beta(c + a1, k - c + (m - 1)*a1) /
    (beta(a1, (m - 1)*a1) * beta(c + a2, k - c + (m - 1)*a2))
}
bf_vec <- function(a1, a2, m = 20, c, k = sum(c)) {
  stopifnot(a1 > 0, a2 > 0, c <= k, m > 1)

  exp(mvbeta(rep(a2, length(c)), log = T) + mvbeta(a1 + c, log = T) -
        mvbeta(rep(a1, length(c)), log = T) - mvbeta(a2 + c, log = T))
}
vis_p_value <- function(C, K, alpha = 1, m = 20){
  single_p <- function(cc, kk, aa, mm) {
    x <- cc:kk
    sum(exp(lchoose(kk, x) - lbeta(aa, (mm - 1) * aa) + lbeta(x + aa, kk - x + (mm - 1) * aa)))
  }

  df <- tibble(cc = C,
               kk = K,
               aa = alpha,
               mm = m) %>%
    unnest(everything()) %>%
    mutate(p = purrr::pmap_dbl(., single_p))
  df$p
}

get_all_ps <- function(data, alpha) {
  n_target <- filter(data, obs_plot_location == response_no)$n
  n_total <- sum(data$n)

  tmp <- vinference::pVsim(n_target, n_total, N = 5000, scenario = 3)
  tibble(binomial = pbinom(n_target, size = n_total, prob = 1/20, lower.tail = F),
    vinf_sim = tmp[2],
    vinf_binom = tmp[3],
    multinomial_a_1 = vis_p_value(C = n_target, K = n_total),
    multinomial_a_est = vis_p_value(C = n_target, K = n_total, alpha = alpha))
}

future::plan("multicore", workers = 36)
all_p_val_methods <- studies_sum %>%
  nest(data = c(obs_plot_location, response_no, n)) %>%
  left_join(select(studies_alpha_est, study, param_value, test_param, alpha))

all_p_val_methods <- all_p_val_methods %>%
  mutate(p_calc = furrr::future_map2(data, alpha, get_all_ps))
save(all_p_val_methods, file = "data/all_p_values_computed.Rdata")



load("data/all_p_values_computed.Rdata")
all_p_val_methods <- all_p_val_methods %>%
  unnest_wider(p_calc)

all_p_val_long <- all_p_val_methods %>%
  pivot_longer(cols = binomial:multinomial_a_est, names_to = "method", values_to = "p") %>%
  mutate(method = factor(method, levels = c("binomial", "vinf_binom", "vinf_sim", "multinomial_a_1", "multinomial_a_est")))

ggplot(all_p_val_long, aes(x = method, y = p, group = pic_name)) +
  geom_line(alpha = .2)
