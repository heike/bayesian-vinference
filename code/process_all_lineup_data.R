# Script to pull and anonymize results from all turk studies.

library(tidyverse)
source(here::here("code/alpha_ml.R"))
# dir <- "~/Dropbox/GraphicsGroup/Lineups-nf/"
#
# pic_details <- tibble(pic_details_path = list.files(dir, pattern = "picture[\\._-]details.csv", recursive = T, full.names = T),
#                       study = str_extract(pic_details_path, "turk\\d{1,}")) %>%
#   group_by(study) %>%
#   arrange(nchar(pic_details_path)) %>%
#   filter(row_number() == 1)
#
# turk_data <- tibble(results_path = list.files(dir, pattern = "(data.turk\\d{1,})|(turk\\d{1,}.results)|(turk\\d{1,}.raw.results)|(turk\\d{1,}_raw_results)|(^turk\\d{1,}).csv", recursive = T, full.names = T),
#                     study = str_extract(results_path, "turk\\d{1,}")) %>%
#   group_by(study)
#
#
# fix_up_turk_results <- function(turkdata, picdata) {
#   if (!"pic_id" %in% names(picdata) & !"pic_name" %in% names(picdata)) {
#     if ("id" %in% names(picdata)) {
#       picdata <- rename(picdata, pic_id = id)
#     } else {
#       picdata <- mutate(picdata, pic_id = 1:n())
#     }
#   }
#
#   if ("pic_name" %in% names(picdata)) {
#     if (is.logical(picdata$pic_name)) {
#       picdata <- select(picdata, -pic_name)
#     }
#     picdata$pic_name <- str_remove(picdata$pic_name, "(images)?(svgs)?/")
#   }
#
#   if ("pic_name" %in% names(turkdata)) {
#     if (is.logical(turkdata$pic_name)) {
#       turkdata <- select(turkdata, -pic_name)
#     }
#     turkdata$pic_name <- str_remove(turkdata$pic_name, "(images)?(svgs)?/")
#   }
#
#   if ("plot_location" %in% names(turkdata)) {
#     turkdata$obs_plot_location <- turkdata$plot_location
#   }
#   if ("plot_location" %in% names(picdata)) {
#     picdata$obs_plot_location <- picdata$plot_location
#   }
#
#   picdata <- picdata %>%
#     select(one_of(c("pic_id", "obs_plot_location", "pic_name", "param_value", "test_param")))
#
#   if ("nick_name" %in% names(turkdata)) {
#     turkdata$nick_name <- purrr::map_chr(as.character(turkdata$nick_name), digest::digest)
#   } else if ("id" %in% names(turkdata)) {
#     turkdata$nick_name <- as.character(turkdata$id)
#   } else {
#     warning(paste0("nickname not found for ", unique(turkdata$experiment)))
#   }
#
#   turkdata <- turkdata %>%
#     select(one_of(c("nick_name", "pic_name", "pic_id", "response_no", "obs_plot_location", "test_param", "param_value")))
#
#   turkdata$response_weight <- 1
#   if (is.factor(turkdata$response_no) | is.character(turkdata$response_no)) {
#     turkdata$response_no <- purrr::map(turkdata$response_no, ~as.character(.) %>%
#                                          str_split(",") %>% unlist() %>% as.integer())
#     turkdata$response_weight <- purrr::map_dbl(turkdata$response_no, ~1/length(.))
#     turkdata$response_weight[is.na(turkdata$response_weight)] <- 1
#     turkdata <- unnest(turkdata)
#   }
#   turkdata$obs_plot_location <- as.character(turkdata$obs_plot_location)
#   picdata$obs_plot_location <- as.character(picdata$obs_plot_location)
#
#   # if (is.character(turkdata$obs_plot_location) | is.factor(turkdata$obs_plot_location)) {
#   #   turkdata$obs_plot_location <- purrr::map_int(turkdata$obs_plot_location,
#   #                                                ~as.character(.) %>% str_split(",") %>%
#   #                                                  unlist() %>% as.integer())
#   # }
#   if (is.list(turkdata$response_no)) turkdata <- unnest(turkdata)
#
#   full_join(turkdata, picdata)
# }
# fix_safely <- safely(fix_up_turk_results)
#
# studies <- inner_join(pic_details, turk_data, by = "study") %>%
#   mutate(pic_details = purrr::map(pic_details_path, read.csv),
#          turk_results = purrr::map(results_path, read.csv),
#          joined = purrr::map2(turk_results, pic_details, fix_safely)) %>%
#   mutate(targets = ifelse(study %in% c("turk16", "turk18"), 2, 1)) %>%
#   mutate(err = map(joined, "error"),
#          joined = map(joined, "result")) %>%
#   filter(purrr::map_lgl(err, is.null))
#
# study_details <- studies %>%
#   select(-pic_details, -pic_details_path, -results_path, -turk_results, -err) %>%
#   unnest() %>%
#   filter(!is.na(response_no) & !is.na(nick_name) & !is.na(test_param))
#
# studies_sum <- study_details %>%
#   group_by(study, param_value, test_param, pic_id, pic_name, obs_plot_location) %>%
#   count(response_no, wt = 1/response_weight) %>%
#   complete(nesting(study, param_value, test_param, pic_id, pic_name, obs_plot_location), response_no = 1:20, fill = list(n = 0)) %>%
#   ungroup()
#
# write_csv(studies_sum, "data/all-turk-studies-summary.csv")

studies_sum <- read_csv("data/all-turk-studies-summary.csv")
studies_alpha_est <- studies_sum %>%
  mutate(targets = ifelse(study %in% c("turk16", "turk18"), 2, 1)) %>%
  filter(targets == 1) %>%
  mutate(obs_plot_location = purrr::map_int(obs_plot_location, ~str_split(., ",") %>% unlist() %>% as.integer),
         match = purrr::map2_lgl(response_no, obs_plot_location, ~.x %in% .y)) %>%
  filter(!match) %>%
  group_by(study, pic_id, pic_name, param_value, test_param) %>%
  arrange(study, pic_id, pic_name, param_value, test_param, response_no) %>%
  mutate(null_no = 1:n()) %>%
  select(study, param_value, test_param, pic_id, pic_name, null_no, n) %>%
  mutate(weight = sum(n)) %>%
  filter(weight > 0) %>%
  ungroup() %>%
  tidyr::spread(key = null_no, value = n) %>%
  select(-pic_id, -pic_name, -weight) %>%
  nest(data = -c(study:test_param)) %>%
  mutate(data = purrr::map(data, as.matrix),
         weight = purrr::map(data, rowSums)) %>%
  mutate(alpha = purrr::map2_dbl(data, weight, alpha.ml))

plot_df <- studies_alpha_est %>%
  filter(!str_detect(test_param, "ref") & !str_detect(param_value, "ref")) %>%
  mutate(test_param = str_remove(test_param, "turk\\d{1,}[-_]") %>% str_remove("-rep\\d{1,}")) %>%
  mutate(param_value = str_remove(param_value, "-rep\\d{1,}") %>% str_remove("turk\\d{1,}-"),
         param_value = ifelse(study %in% c("turk11", "turk14"), str_remove(param_value, "-\\d$"), param_value)) %>%
  mutate(type = str_extract(str_to_lower(test_param), "single|multiple") %>%
           ifelse(is.na(.), "single", .)) %>%
  mutate(test_param = str_remove(test_param, "-?(([Ss]ingle)|([Mm]ultiple))")) %>%
  mutate(type2 = str_extract(test_param, "Inner|Outer"),
         type2 = ifelse(is.na(type2), "Default", type2),
         test_param = str_remove(test_param, "-(Inner|Outer)")) %>%
  left_join(.,
            select(., study, param_value) %>%
              unique() %>%
              mutate(dataset = as.numeric(as.factor(param_value))) %>%
              group_by(study) %>%
              mutate(dataset = sprintf("%02d", order(dataset))) %>%
              ungroup()) %>%
  left_join(.,
            select(., study, test_param) %>%
              unique() %>%
              mutate(presentation = as.numeric(as.factor(test_param))) %>%
              group_by(study) %>%
              mutate(presentation = sprintf("%02d", order(presentation))) %>%
              ungroup) %>%
  mutate(label = ifelse(study %in% c("turk11", "turk14"), param_value, test_param)) %>%
  left_join(.,
            select(., study, label) %>%
              unique() %>%
              mutate(param_value2 = as.numeric(as.factor(label))) %>%
              group_by(study) %>%
              mutate(param_value2 = sprintf("%02d", order(param_value2))) %>%
              ungroup) %>%
  mutate(
    # tmp = presentation,
    # presentation = ifelse(study == "turk5", dataset, presentation),
    # dataset = ifelse(study == "turk5", tmp, dataset),
    dataset2 = as.numeric(dataset)) %>%
  mutate(study = factor(study, levels = sprintf("turk%d", c(4, 6, 7, 5, 11, 14, 10, 13)), labels = sprintf("Study %d", 1:8), ordered = T))

# plot_df %>%
# ggplot(aes(x = param_value2, y = alpha, color = type, group = param_value)) +
#   facet_wrap(~study, scales = "free_x") +
#   geom_point(position = position_dodge(width = .75))
# plot_df %>%
#   ggplot(aes(x = dataset2, y = alpha, color = type, group = presentation)) +
#   facet_wrap(~study, scales = "free_x") +
#   scale_color_manual("Selection\nType", values = c("orange4", "purple")) +
#   geom_point(position = position_dodge(width = .3), shape = 1) +
#   theme_bw() +
#   theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
#   scale_x_continuous("Dataset") +
#   scale_y_continuous(expression(hat(alpha))) +
#   ggtitle(expression(paste(hat(alpha), " for Single-target Lineup Studies")))

turk16_details <- read_csv("data/Turk16/data-picture-details-gini.csv") %>%
  mutate(obs = str_extract(obs_plot_location, "^\\d{1,2}, \\d{1,2}"),
         obs = purrr::map(obs, ~str_split(., ",") %>%
                            unlist() %>% parse_number())
  ) %>%
  rename(data_id = pic_id) %>%
  mutate(pic_id = (data_id-1)*10+j)

turk16_sum <- read.csv("data/Turk16/turk16_results.csv", stringsAsFactors = F) %>%
  mutate(response = purrr::map(response_no, ~tibble(response = str_split(., ",") %>% unlist() %>% parse_number(),
                                                    weight = 1/length(response)))) %>%
  unnest(cols = c(response)) %>%
  group_by(pic_id, response) %>%
  summarize(n = sum(weight)) %>%
  complete(crossing(pic_id, response = 1:20), fill = list(n = 0)) %>%
  ungroup()

turk16_sum2 <- turk16_sum %>%
  left_join(turk16_details) %>%
  mutate(target = purrr::map2_lgl(response, obs, function(x, y) x %in% unlist(y)))

write_csv(select(turk16_sum2, -obs), "data/turk16_results_summary.csv")
