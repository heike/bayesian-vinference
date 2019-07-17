aes_order <- c("colorShapeEllipse", "colorTrend", "colorEllipse", "colorShape", "shape", "color", "trend", "plain", "colorEllipseTrendError", "trendError")
pic_data <- read_csv("data/Turk16/data-picture-details-gini.csv")
t16_res <- read_csv("data/Turk16/turk16_results_anon.csv", col_types = "_c__cc___") %>%
  mutate(pic_id = str_trim(pic_id) %>% as.numeric()) %>%
  filter(!is.na(pic_id)) %>%
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
  mutate(
    set_number = str_extract(data_name, "set-(\\d{1,})") %>% str_remove("set-") %>% as.numeric,
    set_type = str_remove(data_name, "set-\\d{1,}-"),
    aes_type = str_remove(test_param, "turk16-"))

t16_null_wide <- t16_res %>%
  select(pic_id, set_number, set_type, aes_type, null_no, weight) %>%
  group_by(pic_id, set_type, aes_type) %>%
  filter(null_no > 0) %>%
  mutate(n_total = sum(weight)) %>%
  filter(n_total > 0) %>%
  tidyr::spread(key = null_no, value = weight) %>%
  ungroup()


t16_null_alpha_plotwise <- t16_null_wide %>%
  extract(set_type, into = c("k", "sdline", "sdgroup"),
          regex = "k-([35])-sdline-([\\d\\.]{1,})-sdgroup-([\\d\\.]{1,})") %>%
  mutate_at(vars(k, sdline, sdgroup), as.numeric) %>%
  filter(n_total > 4) %>%
  group_by(aes_type, set_number, k, sdline, sdgroup, pic_id, n_total) %>%
  nest(`1`:`18`) %>%
  mutate(alpha = purrr::map2_dbl(data, n_total,
                                 function(x, y) alpha.ml(as.matrix(x), weight = y))) %>%
  mutate(aes_type = factor(aes_type, levels = aes_order, ordered = T)) %>%
  mutate(alpha_type = "null") %>%
  group_by(k, sdline, sdgroup) %>%
  mutate(set_min = min(set_number))

t16_null_alpha_setwise <- t16_null_wide %>%
  extract(set_type, into = c("k", "sdline", "sdgroup"),
          regex = "k-([35])-sdline-([\\d\\.]{1,})-sdgroup-([\\d\\.]{1,})") %>%
  mutate_at(vars(k, sdline, sdgroup), as.numeric) %>%
  group_by(aes_type, k, sdline, sdgroup) %>%
  nest() %>%
  mutate(n = purrr::map(data, "n_total")) %>%
  mutate(alpha = purrr::map2_dbl(data, n, function(x, y) alpha.ml(as.matrix(x), weight = y))) %>%
  mutate(aes_type = factor(aes_type, levels = aes_order, ordered = T)) %>%
  mutate(alpha_type = "null") %>%
  mutate(n_total = purrr::map_dbl(n, sum)) %>%
  filter(n_total > 4) %>%
  group_by(k, sdline, sdgroup) %>%
  mutate(set_min = min(purrr::map_dbl(data, ~min(.$set_number))),
         set_max = max(purrr::map_dbl(data, ~max(.$set_number))))


t16_null_alpha_aeswise <- t16_null_wide %>%
  extract(set_type, into = c("k", "sdline", "sdgroup"),
          regex = "k-([35])-sdline-([\\d\\.]{1,})-sdgroup-([\\d\\.]{1,})") %>%
  mutate_at(vars(k, sdline, sdgroup), as.numeric) %>%
  group_by(aes_type) %>%
  nest() %>%
  mutate(n = purrr::map(data, "n_total")) %>%
  mutate(alpha = purrr::map2_dbl(data, n, function(x, y) alpha.ml(as.matrix(x), weight = y))) %>%
  mutate(aes_type = factor(aes_type, levels = aes_order, ordered = T)) %>%
  mutate(alpha_type = "null") %>%
  mutate(n_total = purrr::map_dbl(n, sum))

t16_all_wide <- t16_res %>%
  select(pic_id, set_type, aes_type, response_no, weight) %>%
  group_by(pic_id, set_type, aes_type) %>%
  # filter(null_no > 0) %>%
  mutate(n_total = sum(weight)) %>%
  filter(n_total > 0) %>%
  tidyr::spread(key = response_no, value = weight) %>%
  ungroup()
