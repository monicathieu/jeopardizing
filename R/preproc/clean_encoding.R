clean_encoding <- function (encoding_data, pretest_data) {
  out <- encoding_data %>% 
    select(subj_num = `Participant Private ID`,
           group,
           group_trial_num = trial_num,
           trial_screen = `Screen Name`,
           zone = `Zone Name`,
           rt = `Reaction Time`,
           resp = `Response`,
           starts_with("encoding_sentence")) %>% 
    filter((trial_screen == "fact" & zone == "slider") | trial_screen == "already_knew") %>%
    nest(data = -c(subj_num, group, group_trial_num, trial_screen)) %>%
    pivot_wider(names_from = trial_screen, values_from = data) %>% 
    mutate(already_knew = map_chr(already_knew, ~.x$resp)) %>% 
    unnest(fact) %>% 
    mutate(# Should catch slider trials where the slider was never clicked
      # bc I think timeout will still be true if submitResp was never clicked but the slider was moved
      resp = if_else(zone == "scale" & rt >= 34900, NA_character_, resp),
      resp = as.integer(resp)) %>%
    nest(data = -c(subj_num, group, group_trial_num)) %>% 
    # stuff to pull only the row of scale trials where the slider stopped moving
    mutate(data = map(data, get_slider_rt)) %>%
    unnest(data) %>% 
    # this SHOULD keep one row per trial and also keep no responses
    select(-zone) %>% 
    nest(data = -subj_num) %>% 
    left_join(pretest_data, by = "subj_num") %>% 
    mutate(data = map2(data, categories,
                       ~.x %>% select(group_trial_num,
                                      group,
                                      resp,
                                      rt,
                                      already_knew,
                                      encoding_sentence = paste0("encoding_sentence.", .y)))) %>%
    unnest(data) %>% 
    mutate(category = if_else(group == "academic", category1, category2)) %>% 
    select(-c(category1, category2, categories)) %>% 
    mutate(already_knew = fct_recode(already_knew,
                                     none = "Did not know this before",
                                     some = "Knew some of this before",
                                     all = "Knew all of this before"),
           already_knew = fct_relevel(already_knew, "all", after = Inf))
  
  return (out)
}

# pretest <- read_csv(here::here("ignore", "data", "task_pretest.csv")) %>%
#   select(-n_correct) %>%
#   pivot_wider(names_from = category_num, values_from = category, names_prefix = "category") %>%
#   mutate(categories = paste(category1, category2, sep = "_"))

# write_csv(less_raw, file = here::here("ignore", "data", "task_encoding.csv"))
