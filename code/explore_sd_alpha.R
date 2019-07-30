t16_all_wide

t16_res



filter(t16_res, null_n > 7) %>%
  filter(null_no != 0) %>%
  select(pic_id, set_type, set_number, aes_type, null_no, weight) %>%
  group_by(pic_id, set_type, set_number, aes_type) %>%
  tidyr::spread(key = null_no, value = weight) %>%
  nest(5:22) %>%
  mutate(data = purrr::map(data, as.vector)) %>%
  mutate(alpha = purrr::map_dbl(data, alpha.ml))%T>%
  {
    ggplot(., aes(x = aes_type, y = alpha)) +
      geom_jitter() +
      geom_boxplot(outlier.shape = NA, alpha = .5) +
      coord_flip()
  } %>%
  {
    group_by(., aes_type) %>%
      summarize(sd_alpha = sd(alpha), n = n())
  }


median(t16_res$null_n)

#
# purrr::map_dfr(3:20, function(i) {
#   filter(t16_res, null_n > i) %>%
#     filter(null_no != 0) %>%
#     select(pic_id, set_type, set_number, aes_type, null_no, weight) %>%
#     group_by(pic_id, set_type, set_number, aes_type) %>%
#     tidyr::spread(key = null_no, value = weight) %>%
#     nest(5:22) %>%
#     mutate(data = purrr::map(data, as.vector)) %>%
#     mutate(alpha = purrr::map_dbl(data, alpha.ml)) %>%
#     group_by(., aes_type) %>%
#       summarize(sd_alpha = sd(alpha), n = n()) %>%
#     mutate(cutoff = i)
# }) %>%
#   ggplot(aes(x = cutoff, y = sd_alpha, color = aes_type)) +
#   geom_line()
