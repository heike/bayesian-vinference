# Script to pull and anonymize results from all turk studies.

dir <- "~/Dropbox/GraphicsGroup/Lineups-nf/"

pic_details <- tibble(pic_details_path = list.files(dir, pattern = "picture[\\._-]details.csv", recursive = T, full.names = T),
                      study = str_extract(pic_details_path, "turk\\d{1,}")) %>%
  group_by(study) %>%
  arrange(nchar(pic_details_path)) %>%
  filter(row_number() == 1)

turk_data <- tibble(results_path = list.files(dir, pattern = "(data.turk\\d{1,})|(turk\\d{1,}.results)|(turk\\d{1,}.raw.results)|(turk\\d{1,}_raw_results)|(^turk\\d{1,}).csv", recursive = T, full.names = T),
                    study = str_extract(results_path, "turk\\d{1,}")) %>%
  group_by(study) %>%
  arrange(nchar(results_path)) %>%
  filter(row_number() == 1)


fix_up_turk_results <- function(turkdata, picdata) {
  if (!"pic_id" %in% names(picdata) & !"pic_name" %in% names(picdata)) {
    if ("id" %in% names(picdata)) {
      picdata <- rename(picdata, pic_id = id)
    } else {
      picdata <- mutate(picdata, pic_id = 1:n())
    }
  }

  if ("pic_name" %in% names(picdata)) {
    if (is.logical(picdata$pic_name)) {
      picdata <- select(picdata, -pic_name)
    }
    picdata$pic_name <- str_remove(picdata$pic_name, "(images)?(svgs)?/")
  }

  if ("pic_name" %in% names(turkdata)) {
    if (is.logical(turkdata$pic_name)) {
      turkdata <- select(turkdata, -pic_name)
    }
    turkdata$pic_name <- str_remove(turkdata$pic_name, "(images)?(svgs)?/")
  }

  if ("plot_location" %in% names(turkdata)) {
    turkdata <- rename(turkdata, obs_plot_location = plot_location)
  }
  if ("plot_location" %in% names(picdata)) {
    picdata <- rename(picdata, obs_plot_location = plot_location)
  }

  picdata <- picdata %>%
    select(one_of(c("pic_id", "obs_plot_location", "pic_name")))

  if ("nick_name" %in% names(turkdata)) {
    turkdata$nick_name <- purrr::map_int(as.character(turkdata$nick_name), digest::digest2int)
  } else if ("id" %in% names(turkdata)) {
    turkdata$nick_name <- turkdata$id
  } else {
    warning(paste0("nickname not found for ", unique(turkdata$experiment)))
  }

  turkdata <- turkdata %>%
    select(one_of(c("nick_name", "pic_name", "pic_id", "response_no", "obs_plot_location", "test_param", "param_value")))

  if (is.factor(turkdata$response_no) | is.character(turkdata$response_no)) {
    turkdata$response_no <- purrr::map_int(turkdata$response_no, ~as.character(.) %>% str_split(",") %>% unlist() %>% as.integer())
  }

  full_join(turkdata, picdata)
}
fix_safely <- safely(fix_up_turk_results)

studies <- inner_join(pic_details, turk_data, by = "study") %>%
  mutate(pic_details = purrr::map(pic_details_path, read.csv),
         turk_results = purrr::map(results_path, read.csv),
         joined = purrr::map2(turk_results, pic_details, fix_safely)) %>%
  mutate(targets = ifelse(study %in% c("turk16", "turk18"), 2, 1)) %>%
  filter(purrr::map(joined, "error") %>% purrr::map_lgl(is.null)) %>%
  mutate(joined = purrr::map(joined, "result")) %>%
  select(-pic_details, -pic_details_path, -results_path, -turk_results) %>%
  unnest() %>%
  filter(!is.na(response_no) & !is.na(nick_name) & !is.na(test_param))

studies_sum <- studies %>%
  group_by(study, test_param, pic_id, pic_name, obs_plot_location) %>%
  count(response_no) %>%
  complete(nesting(study, test_param, pic_id, pic_name, obs_plot_location), response_no = 1:20, fill = list(n = 0)) %>%
  ungroup()

write_csv(studies_sum, "data/all-turk-studies-summary.csv")
