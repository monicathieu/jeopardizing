clean_pretest <- function (pretest_data) {
  out <- pretest_data %>% 
    select(subj_num = "Participant Private ID",
           category_num = "Zone Name",
           category = "Response",
           n_correct = "Reaction Time") %>% 
    filter(startsWith(category_num, "category")) %>% 
    mutate(category_num = as.integer(str_sub(category_num, start = -1L)))
  
  return (out)
}

# the encoding and retrieval cleaning functions expect a slightly reshaped version
reshape_pretest <- function (pretest_data_clean) {
  out <- pretest_data_clean %>%
    select(-n_correct) %>%
    pivot_wider(names_from = category_num, values_from = category, names_prefix = "category") %>%
    mutate(categories = paste(category1, category2, sep = "_"))
  
  return (out)
}

# write_csv(less_raw, file = here::here("ignore", "data", "task_pretest.csv"))
