clean_demos_ids <- function (id_data) {
  out <- id_data %>% 
    select(subj_num = `Participant Private ID`, q_key = `Question Key`, resp = Response) %>%
    filter(!endsWith(q_key, "QUESTIONNAIRE")) %>% 
    pivot_wider(names_from = q_key, values_from = resp)
  
  return (out)
}

# write_csv(less_raw, file = here::here("ignore", "data", "demos_ids.csv"))
