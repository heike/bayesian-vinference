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
vis_p_value_orig <- function(C, K, m = 20){
  single_p <- function(cc, kk, aa, mm) {
    x <- cc:kk
    sum(exp(lchoose(kk, x) - x*log(mm) + (kk-x)*log(1-1/mm)))
  }

  df <- tibble(cc = C,
               kk = K,
               mm = m) %>%
    unnest(everything()) %>%
    mutate(p = purrr::pmap_dbl(., single_p))
  df$p
}

get_all_ps <- function(data, alpha) {
  n_target <- filter(data, obs_plot_location == response_no)$n
  n_total <- sum(data$n)

  list(binomial = pbinom(n_target, size = n_total, prob = 1/20, lower.tail = F),
    vinference_sim = vinference::pVsim(n_target, n_total, N = 5000, scenario = 3),
    multinomial_a_1 = vis_p_value_orig(C = n_target, K = n_total),
    multinomial_a_est = vis_p_value(C = n_target, K = n_total, alpha = alpha))
}

future::plan("multicore", workers = 36)
all_p_val_methods <- studies_sum %>%
  nest(data = c(obs_plot_location, response_no, n)) %>%
  left_join(select(studies_alpha_est, study, param_value, test_param, alpha))

all_p_val_methods <- all_p_val_methods %>%
  mutate(p_calc = furrr::future_map2(data, alpha, get_all_ps))
save(all_p_val_methods, file = "data/all_p_values_computed.Rdata")
