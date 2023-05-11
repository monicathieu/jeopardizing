clean_q_trivia_demos <- function (trivia_demo_data) {
  out <- trivia_demo_data %>% 
    select(subj_num = `Participant Private ID`, q_key = `Question Key`, resp = Response) %>%
    filter(!endsWith(q_key, "QUESTIONNAIRE")) %>% 
    pivot_wider(names_from = q_key, values_from = resp) %>% 
    rename_with(~str_replace(., "-", "_"), everything()) %>% 
    mutate(across(c(starts_with("trivia"), ends_with("quantised")), as.integer))
  
  return (out)
}

# write_csv(less_raw, file = here::here("ignore", "data", "q_trivia_demos.csv"))
