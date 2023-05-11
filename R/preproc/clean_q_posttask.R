clean_q_posttask <- function (posttask_data) {
  out <- posttask_data %>% 
    select(subj_num = `Participant Private ID`, q_key = `Question Key`, resp = Response) %>%
    filter(!endsWith(q_key, "QUESTIONNAIRE")) %>% 
    pivot_wider(names_from = q_key, values_from = resp) %>% 
    rename_with(~str_replace(., "-", "_"), everything())
  
  return (out)
}

# write_csv(less_raw, file = here::here("ignore", "data", "q_posttask.csv"))
