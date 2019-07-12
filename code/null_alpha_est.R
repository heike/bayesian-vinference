#' mle of alpha
alpha.ml <- function(p, weight = NULL, eps = 10^(-7)) {
  # matrix p
  if (is.null(dim(p))) {
    m <- length(p)
    n <- 1
    p <- matrix(p, nrow = 1)
  } else {
    n <- dim(p)[1]
    m <- dim(p)[2]
  }
  if (is.null(weight)) {
    weight <- rep(1, n)
  } else {
    n <- sum(weight)
  }

  weight <- weight / sum(weight)
  ps <- p
  ps[p < eps] <- eps # make sure we don't take the log of 0
  ps <- ps / rowSums(ps)
  logp <- sum(weight * rowSums(log(ps))) / m
  ml <- function(alpha) {
    digamma(alpha) - digamma(alpha * m) - logp
  }
  # find alpha such that digamma(alpha) - digamma(alpha*m) = logp
  alpha <- uniroot(f = ml, interval = c(eps, 10))

  alpha$root
}

pic_data <- read_csv("data/Turk16/data-picture-details-gini.csv")
t16_res <- read_csv("data/Turk16/turk16_results_anon.csv", col_types = "_c__cc___") %>%
  mutate(pic_id = str_trim(pic_id) %>% as.numeric()) %>%
  filter(!is.na(pic_id)) %>%
  mutate(nick_name = as.numeric(factor(nick_name))) %>%
  mutate(response_no = purrr::map(response_no, str_split, pattern = ",", simplify = F) %>%
           purrr::map(unlist),
         response_no = purrr::map(response_no, as.numeric),
         n_results = purrr::map_int(response_no, length),
         weight = 1/n_results) %>%
  select(-matches("ip_address|time|conf_level|choice_reason")) %>%
  unnest(response_no) %>%
  group_by(pic_id, response_no) %>%
  summarize(weight = sum(weight)) %>%
  complete(pic_id, response_no = 1:20, fill = list(weight = 0)) %>%
  left_join(select(pic_data, test_param, obs_plot_location, pic_name, data_name) %>%
              mutate(pic_id = 1:n())) %>%
  filter(!is.na(response_no)) %>%
  extract(obs_plot_location, into = c("cluster_target", "trend_target", "gini_target"),
          regex = c("(\\d{1,}), ?(\\d{1,}), ?(\\d{1,})")) %>%
  mutate_at(vars(cluster_target:gini_target), as.numeric) %>%
  mutate(is_target = ((response_no == cluster_target) | (response_no == trend_target)),
         null_no = factor(response_no*(!is_target)) %>% as.numeric %>% magrittr::subtract(1)) %>%
  group_by(pic_id) %>%
  mutate(null_n = sum((!is_target)*weight)) %>%
  mutate(set_type = str_remove(data_name, "set-\\d{1,}-"),
         aes_type = str_remove(test_param, "turk16-"))

t16_null_wide <- t16_res %>%
  select(pic_id, set_type, aes_type, null_no, weight) %>%
  group_by(pic_id, set_type, aes_type) %>%
  filter(null_no > 0) %>%
  mutate(n_total = sum(weight)) %>%
  filter(n_total > 0) %>%
  tidyr::spread(-c(1:3), key = null_no, value = weight) %>%
  ungroup()

t16_null_wide %>% filter(aes_type == "plain", set_type == "k-3-sdline-0.25-sdgroup-0.25") %>%
  select(-c(1:4)) %>%
  as.matrix() %>%
  alpha.ml


t16_null_alpha <- t16_null_wide %>%
  extract(set_type, into = c("k", "sdline", "sdgroup"), regex = "k-([35])-sdline-([\\d\\.]{1,})-sdgroup-([\\d\\.]{1,})") %>%
  mutate_at(vars(k, sdline, sdgroup), as.numeric) %>%
  group_by(aes_type) %>%
  nest() %>%
  mutate(alpha = purrr::map_dbl(data, function(x) alpha.ml(as.matrix(x[,-c(1:5)])))) %>%
  mutate(aes_type = factor(aes_type, levels = c("colorShapeEllipse", "colorTrend", "colorEllipse", "colorShape", "shape", "color", "trend", "plain", "colorEllipseTrendError", "trendError"), ordered = T)) %>%
  mutate(alpha_type = "null")

ggplot(data = t16_null_alpha, aes(x = aes_type, y = alpha)) +
  geom_point() + coord_flip()


t16_all_wide <- t16_res %>%
  select(pic_id, set_type, aes_type, response_no, weight) %>%
  group_by(pic_id, set_type, aes_type) %>%
  # filter(null_no > 0) %>%
  mutate(n_total = sum(weight)) %>%
  filter(n_total > 0) %>%
  tidyr::spread(-c(1:3), key = response_no, value = weight) %>%
  ungroup()

t16_all_alpha <- t16_all_wide %>%
  extract(set_type, into = c("k", "sdline", "sdgroup"), regex = "k-([35])-sdline-([\\d\\.]{1,})-sdgroup-([\\d\\.]{1,})") %>%
  mutate_at(vars(k, sdline, sdgroup), as.numeric) %>%
  group_by(aes_type) %>%
  nest() %>%
  mutate(alpha = purrr::map_dbl(data, function(x) alpha.ml(as.matrix(x[,-c(1:5)])))) %>%
  mutate(aes_type = factor(aes_type, levels = c("colorShapeEllipse", "colorTrend", "colorEllipse", "colorShape", "shape", "color", "trend", "plain", "colorEllipseTrendError", "trendError"), ordered = T)) %>%
  mutate(alpha_type = "all")

t16_alpha <- bind_rows(t16_null_alpha, t16_all_alpha)

ggplot(data = t16_alpha, aes(x = aes_type, y = alpha, color = alpha_type)) +
  geom_point() + coord_flip()
